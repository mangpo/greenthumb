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

    (define-tokens a (VAR WORD NUM ITYPE))
    (define-empty-tokens b (EOF EQ COMMA HOLE LT GT X))

    (define-lex-abbrevs
      (digit10 (char-range "0" "9"))
      (number10 (number digit10))
      (snumber10 (re-or number10 (re-seq "-" number10)))

      (identifier-characters (re-or (char-range "A" "Z") (char-range "a" "z")))
      (identifier-characters-ext (re-or digit10 identifier-characters "_" "*"))
      (identifier (re-seq identifier-characters 
                          (re-* identifier-characters-ext)))

      (var   (re-: "%" (re-+ (re-or identifier-characters digit10))))
      (itype (re-: "i" (re-+ digit10) (re-? "*")))
      
      )

    (set! asm-lexer
      (lexer-src-pos
       (","        (token-COMMA))
       ("="        (token-EQ))
       ("<"        (token-LT))
       (">"        (token-GT))
       ("x"        (token-X))
       ("?"        (token-HOLE))
       (snumber10  (token-NUM lexeme))
       (var        (token-VAR lexeme))
       (itype      (token-ITYPE lexeme))
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
         ((WORD) (list $1))
         ((WORD words) (cons $1 $2)))
        
        (arg
         ((VAR) $1)
         ((NUM) $1))

        (arg-list
         ((arg) (list $1))
         ((arg COMMA arg-list) (cons $1 $3)))

        (type
         ((ITYPE) $1)
         ((LT NUM X ITYPE GT) (cons $2 $4)))
        
        (instruction
         ((HOLE)         (inst #f #f))
         ((WORD type arg COMMA type arg) (convert2store $1 $2 $3 $5 $6))
         ((VAR EQ words type COMMA type arg) (convert2load $3 $4 $1 $6 $7))
         ((VAR EQ words type arg-list) (convert2inst $3 $4 $1 $5))
         )
        
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

(define (convert2inst ops type lhs args)
  (define op
    (if (equal? (first ops) "icmp")
	(second ops)
	(first ops))) ;; TODO: currently ignore terms like nuw
  
  (unless (equal? type "i32")
	  (raise "Currently only support i32 type."))
  
  (inst op (list->vector (cons lhs args))))

(define (convert2store op val-type val-arg p-type p-arg)
  (unless (equal? val-type "i32")
	  (raise "Currently only support i32 type."))
  (unless (equal? p-type "i32*")
	  (raise "Currently only support i32 type."))

  (inst op (vector val-arg p-arg)))

(define (convert2load ops dest-type dest-arg p-type p-arg)
  (unless (equal? dest-type "i32")
	  (raise "Currently only support i32 type."))
  (unless (equal? p-type "i32*")
	  (raise "Currently only support i32 type."))

  (inst (car ops) (vector dest-arg p-arg)))

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
