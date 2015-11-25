#lang racket

(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc
	 "../parser.rkt" "../ast.rkt")

(provide llvm-demo-parser%)

(define llvm-demo-parser%
  (class parser%
    (super-new)
    (inherit-field asm-parser asm-lexer)

    (define-tokens a (VAR WORD NUM))
    (define-empty-tokens b (EOF EQ COMMA HOLE))

    (define-lex-abbrevs
      (digit10 (char-range "0" "9"))
      (number10 (number digit10))
      (snumber10 (re-or number10 (re-seq "-" number10)))

      (identifier-characters (re-or (char-range "A" "Z") (char-range "a" "z")))
      (identifier-characters-ext (re-or digit10 identifier-characters "_"))
      (identifier (re-seq identifier-characters 
                          (re-* (re-or identifier-characters digit10))))

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
         ((VAR EQ terms) (convert2inst $1 (car $3) (cdr $3))))
        
        (inst-list 
         (() (list))
         ((instruction inst-list) (cons $1 $2)))
        
        (code   
         ((inst-list) (rename (list->vector $1)))))
       ))

    (define/public (info-from-file file)
      (define lines (file->lines file))
      (define live-out (string-split (first lines) ","))
      (define live-in (string-split (second lines) ","))
      (values live-out live-in))

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

(define (live-in-set inst-vec)
  (define use (set))
  (define def (set))
  (for ([my-inst inst-vec])
       (when (inst-op my-inst)
             (let ([args (inst-args my-inst)])
               (for ([arg (vector-copy args 1)])
                    (when (and (equal? (substring arg 0 1) "%")
                               (not (set-member? def arg)))
                          (set! use (set-add use arg))))
               (set! def (set-add def (vector-ref args 0))))))
  use)
                    

(define (rename inst-vec)
  ;; Rename variable from old to new up to instruction at index.
  (define (rename-var old new index)
    (for ([i index]
          [my-inst inst-vec])
         (let ([args (inst-args my-inst)])
           (for ([arg args]
                 [arg-i (in-naturals)])
                (when (equal? arg old)
                      (vector-set! args arg-i new))))))
  
  (define len (vector-length inst-vec))
  (define names (list))
  (define all-names (live-in-set inst-vec))
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

	    ;; rename
            (for ([i (vector-length args)])
                 (let ([arg (vector-ref args i)])
                   (cond
                    [(or (= i 0)
			 (not (equal? (substring arg 0 1) "%"))
                         (set-member? all-names arg)
                         ;;(use-after arg i)
                         )
                     (void)]
                    
                    [(empty? names)
                     (set! all-names (set-add all-names arg))]
                    
                    [else
                     (rename-var arg (car names) (add1 index))
                     (set! names (cdr names))]))))
          )))

  inst-vec)
                
                           
                    
                    
