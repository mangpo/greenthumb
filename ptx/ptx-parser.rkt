#lang racket

(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc
	 "../parser.rkt" "../inst.rkt")

(provide ptx-parser%)

;; This is a Racket Lex Yacc parser.
;; Refer to the follow resources to complete this file.
;; - Lexer:   http://docs.racket-lang.org/parser-tools/Lexers.html
;; - Parser:  http://docs.racket-lang.org/parser-tools/LALR_1__Parsers.html
;; - Example: https://gist.github.com/danking/1068185
(define ptx-parser%
  (class parser%
    (super-new)
    (inherit-field asm-parser asm-lexer)
    (init-field [compress? #f])
    
    (define-tokens a (WORD NUM VAR)) ;; add more tokens
    (define-empty-tokens b (EOF HOLE COMMA DOT SEMICOL)) ;; add more tokens

    (define-lex-abbrevs
      (digit10 (char-range "0" "9"))
      (number10 (number digit10))
      (snumber10 (re-or number10 (re-seq "-" number10)))

      (identifier-characters (re-or (char-range "A" "Z") (char-range "a" "z")))
      (identifier-characters-ext (re-or digit10 identifier-characters "_" "."))
      (identifier (re-seq identifier-characters 
                          (re-* (re-or identifier-characters digit10))))
      (var (re-: "%" (re-+ (re-or identifier-characters digit10))))
      )

    ;; Complete lexer
    (set! asm-lexer
      (lexer-src-pos
       ("?"        (token-HOLE))
       (","        (token-COMMA))
       ("."        (token-DOT))
       (";"        (token-SEMICOL))
       (var        (token-VAR lexeme))
       (snumber10  (token-NUM lexeme))
       (identifier (token-WORD lexeme))
       (whitespace   (position-token-token (asm-lexer input-port)))
       ((eof) (token-EOF))))

    ;; Complete parser
    (set! asm-parser
      (parser
       (start program)
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
        (arg  ((VAR) $1)
              ((NUM) $1))

        (args ((arg) (list $1))
              ((arg COMMA args) (cons $1 $3)))

	(words ((WORD) (list $1))
	      ((WORD DOT words) (cons $1 $3)))
        
        (instruction
         ((words args SEMICOL)
	  (let* ([org $1]
		 [terms (take org (sub1 (length org)))]) ;; drop type
	    (inst (string-join terms ".") (list->vector $2))))
         
         ;; when parsing ?, return (inst #f #f) as an unknown instruction
         ;; (a place holder for synthesis)
         ((HOLE)         (inst #f #f))) 
        
        (code   
         (() (list))
         ((instruction code) (cons $1 $2)))

        (program
         ((code) (list->vector $1)))
       )))


    ;;;;;;;;;;;;;;;;;;;;;;;;; For cooperative search ;;;;;;;;;;;;;;;;;;;;;;;
    ;; Required method if using cooperative search driver.
    ;; Read from file and convert file content into the format we want.
    ;; Info usually includes live-out information.
    ;; It can also contain extra information such as precondition of the inputs.
    (define/override (info-from-file file)
      ;; read from file
      (define lines (file->lines file))
      (define len (length lines))
      (define live-regs (string-split (first lines) ","))
      (define live-preds
	(if (> len 1)
	    (string-split (second lines) ",")
	    (list)))


      (define assume-regs (list))
      (define assume-preds (list))

      (when (> len 2)
	    (let* ([constraints (string-split (third lines) ",")])
	      (for ([c constraints])
		   (let* ([l (regexp-match #rx"([a-z])([0-9]+)[ ]*([<=]+)[ ]*([0-9]+)" c)]
			  [type (list-ref l 1)]
			  [index (list-ref l 2)]
			  [op (list-ref l 3)]
			  [val (list-ref l 4)])
		     (if (equal? type "r")
			 (set! assume-regs (cons (list index op val) assume-regs))
			 (set! assume-preds (cons (list index op val) assume-preds)))))))
      
      (values (cons live-regs live-preds) (cons assume-regs assume-preds)))

    ))

