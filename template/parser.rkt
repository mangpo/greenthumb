#lang racket

(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc
	 "../parser.rkt" "../inst.rkt")

(provide $-parser%)

(define $-parser%
  (class parser%
    (super-new)
    (inherit-field asm-parser asm-lexer)
    (init-field [compress? #f])
    
    (define-tokens a (WORD NUM)) ;; add more tokens
    (define-empty-tokens b (EOF HOLE)) ;; add more tokens

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

    ;; Complete lexer
    (set! asm-lexer
      (lexer-src-pos
       ("?"        (token-HOLE))
       (snumber10  (token-NUM lexeme))
       (identifier (token-WORD lexeme))
       (whitespace   (position-token-token (asm-lexer input-port)))
       ((eof) (token-EOF))))

    ;; Complete parser
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
        (instruction
         ((HOLE)         (inst #f #f)))
        
        (code   
         (() (list))
         ((instruction code) (cons $1 $2)))
       )))

    ;; Required method if using cooperative search driver.
    ;; Parse file that contains live-in and live-out information
    ;; (in any user-defined format) of a code fragment to be optimized into
    ;; another form (also in user-defined format).
    ;; (define/public (info-from-file file)
    ;;   (valies live-out live-in))

    ))
