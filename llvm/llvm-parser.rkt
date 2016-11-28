#lang racket

(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc
	 "../parser.rkt" "../inst.rkt")

(provide llvm-parser%)

(define llvm-parser%
  (class parser%
    (super-new)
    (inherit-field asm-parser asm-lexer)
    (init-field [compress? #f] [no-rename (list)])

    (define-tokens a (VAR WORD NUM))
    (define-empty-tokens b (EOF EQ COMMA HOLE))

    (define-lex-abbrevs
      (digit10 (char-range "0" "9"))
      (number10 (number digit10))
      (snumber10 (re-or number10 (re-seq "-" number10)))

      (identifier-characters (re-or (char-range "A" "Z") (char-range "a" "z")))
      (identifier-characters-ext (re-or digit10 identifier-characters "_" "*"))
      (identifier (re-seq identifier-characters 
                          (re-* identifier-characters-ext)))

      (var (re-: "%" (re-+ (re-or identifier-characters digit10))))
      
      )

    (set! asm-lexer
      (lexer-src-pos
       (","        (token-COMMA))
       ("="        (token-EQ))
       ("?"        (token-HOLE))
       (snumber10  (token-NUM lexeme))
       (var        (token-VAR lexeme))
       (identifier (token-WORD lexeme))
       (whitespace   (position-token-token (asm-lexer input-port)))
       ((eof) (token-EOF))))

    
    (set! asm-parser
      (parser
       (start code)
       (end EOF)
       (error
        (lambda (tok-ok? tok-name tok-value start-pos end-pos)
          (raise-syntax-error 'parser
                              (format "syntax error at '~a' in src l:~a c:~a"
                                      tok-name
                                      (position-line start-pos)
                                      (position-col start-pos)))))
       (tokens a b)
       (src-pos)
       (grammar
        (words
         (() (list))
         ((WORD words) (cons $1 $2)))
        
        (arg
         ((VAR) $1)
         ((NUM) $1))
        
        (term
         ((words arg) (append $1 (list $2))))
        
        (terms 
         ((term)             (list $1))
         ((term COMMA terms) (cons $1 $3)))
        
        (instruction
         ((HOLE)         (inst #f #f))
         ((terms)        (convert2store $1))
         ((VAR EQ WORD WORD COMMA term) (convert2load $3 $1 $6))
         ((VAR EQ terms) (convert2inst $1 (car $3) (cdr $3))))
        
        (inst-list 
         (() (list))
         ((instruction inst-list) (cons $1 $2)))
        
        (code
         ((inst-list) (if compress? (rename (list->vector $1) no-rename) (list->vector $1))))
       )))

    (define/override (info-from-file file)
      (define lines (file->lines file))
      (define live-out (string-split (first lines) ","))
      (set! compress? #t)
      (set! all-names (list->set live-out))
      (vector live-out #t))

    ))

(define (convert2inst lhs term1 terms)
  (define op
    (if (equal? (first term1) "icmp")
	(second term1)
	(first term1)))

  (define type (second (reverse term1)))
  (define out (last term1))
  
  (unless (equal? type "i32")
	  (raise "Currently only support i32 type."))

  (define args
    (for/list ([term terms])
	      (when (> (length term) 1)
		    (unless (equal? (car term) "i32")
			    (raise "Currently only support i32 type.")))
	      (last term)))
  
  (inst op (list->vector (cons lhs (cons out (map last terms))))))

(define (convert2store terms)
  (define term1 (first terms))
  (define term2 (second terms))

  (define op (first term1))
  (define type1 (second term1))
  (define val (last term1))

  (define type2 (first term2))
  (define addr (last term2))

  (unless (equal? type1 "i32")
	  (raise "Currently only support i32 type."))

  (unless (equal? type2 "i32*")
	  (raise "Currently only support i32 type."))

  (inst op (vector val addr)))

(define (convert2load op dest term)
  (define type (first term))

  (unless (equal? type "i32*")
	  (raise "Currently only support i32 type."))

  (inst op (vector dest (last term))))

(define all-names (set))      

(define (rename inst-vec no-rename)
  ;; Rename variable from old to new up to instruction at index.
  (define (rename-var old new index)
    (define def #f)
    (for ([i (reverse (range index))] #:break def)
	 (let* ([my-inst (vector-ref inst-vec i)]
		[args (inst-args my-inst)])
	   (for ([arg args]
		 [arg-i (in-naturals)] #:break def)
		(when (equal? arg old)
		      (vector-set! args arg-i new)
		      (when (= arg-i 0) (set! def #t)))))))

  ;; Check if arg is an input variable to the program.
  (define (is-input? arg index)
    (define def #f)
    (for ([i index] #:break def)
         (let* ([my-inst (vector-ref inst-vec i)]
                [args (inst-args my-inst)])
           (when (equal? arg (vector-ref args 0))
                 (set! def #t))))
    (not def))

  (define (exclude? arg)
    (or (member arg no-rename) (member (string->symbol arg) no-rename)))
  
  (define len (vector-length inst-vec))
  (define names (list))
  (for ([index (reverse (range len))])
       (let ([my-inst (vector-ref inst-vec index)])
         (when
          (inst-op my-inst)
          (let* ([args (inst-args my-inst)]
                 [arg-o (vector-ref args 0)])
            (unless (member args names) (set! names (cons arg-o names)))

	    ;; remove from names first
            (for ([i (vector-length args)])
                 (let ([arg (vector-ref args i)])
                   (cond
                    [(= i 0)
                     (unless (set-member? all-names arg)
                             (set! all-names (set-add all-names arg)))]

		    [(set-member? all-names arg)
		     (set! names (remove arg names))])))

	    ;; rename intermediates
            (for ([i (vector-length args)])
                 (let ([arg (vector-ref args i)])
                   (cond
                    [(or (= i 0) ;; output of this inst
			 (not (equal? (substring arg 0 1) "%")) ;; constant
                         (set-member? all-names arg) ;; name is in the set
                         ;;(use-after arg i)
                         (is-input? arg index) ;; input to the program
                         (exclude? arg) ;; in no-rename list
                         )
                     (void)] ;; don't rename
                    
                    [(empty? names)
                     (set! all-names (set-add all-names arg))]
                    
                    [else
                     (rename-var arg (car names) (add1 index))
                     (set! names (cdr names))]))))
          )))

  inst-vec)
