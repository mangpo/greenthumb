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
         ((inst-list) (list->vector $1))))
       ))))

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
  
  ;; (define last-arg (last args))
  ;; (unless (regexp-match #rx"%" last-arg) 
  ;;         (set! op (string-append op "#")))
  
  (inst op (list->vector (cons lhs (cons out (map last terms))))))
	     
