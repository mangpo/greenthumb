#lang racket

(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc
	 "../parser.rkt" "../ast.rkt")

(provide arm-parser%)

(define arm-parser%
  (class parser%
    (super-new)
    (inherit-field asm-parser asm-lexer)

    (define-tokens a (LABEL BLOCK WORD NUM))
    (define-empty-tokens b (EOF NOP TEXT COMMA DQUOTE HOLE))

    (define-lex-trans number
      (syntax-rules ()
        ((_ digit)
         (re-: (uinteger digit)
               (re-? (re-: "." (re-? (uinteger digit))))))))

    (define-lex-trans uinteger
      (syntax-rules ()
        ((_ digit) (re-+ digit))))

    (define-lex-abbrevs
      (block-comment (re-: "; BB" number10 "_" number10 ":"))
      (line-comment (re-: (re-& (re-: ";" (re-* (char-complement #\newline)))
                                (complement (re-: block-comment any-string)))
                          #\newline))
      (digit10 (char-range "0" "9"))
      (number10 (number digit10))
      (snumber10 (re-or number10 (re-seq "-" number10)))
      (identifier-characters (re-or (char-range "A" "Z") (char-range "a" "z") "." "-" "@" "_" "+"))
      (identifier-characters-ext (re-or digit10 identifier-characters))
      (identifier (re-seq identifier-characters
                          (re-* identifier-characters-ext)))
      (identifier: (re-seq identifier ":"))
      )

    (set! asm-lexer
      (lexer-src-pos
       ("nop"      (token-NOP))
       (".text"    (token-TEXT))
       (","        (token-COMMA))
       ("\""       (token-DQUOTE))
       ("?"        (token-HOLE))
       (identifier: (token-LABEL lexeme))
       (identifier (token-WORD lexeme))
       (snumber10  (token-NUM lexeme))
       (block-comment (token-BLOCK lexeme))
       (line-comment (position-token-token (asm-lexer input-port)))
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
        (words ((WORD) $1)
               ((NUM) $1)
               ((WORD words) (string-append $1 " " $2)))

        (arg  ((WORD) $1)
              ((DQUOTE words DQUOTE) (string-append "\"" $2 "\""))
              ((NUM) $1))

        (args ((arg) (list $1))
              ((arg COMMA args) (cons $1 $3)))

        (instruction ((WORD args) (inst $1 (list->vector $2)))
                     ((NOP)       (inst "nop" (vector)))
                     ((TEXT)      (inst ".text" (vector)))
                     ((HOLE)      (inst #f #f)))
        (inst-list   (() (list))
                     ((instruction inst-list) (cons $1 $2)))

        (oneblock    ((BLOCK inst-list) (block (list->vector $2) #f 
                                               (substring $1 2))))
        (blocks      ((oneblock) (list $1))
                     ((oneblock blocks) (cons $1 $2)))
        

        (chunk  ((LABEL blocks)    (label $1 $2 #f))
                ((LABEL inst-list) (label $1 (block (list->vector $2) #f #f) #f)))
        
        (chunks ((chunk) (list $1))
                ((chunk chunks) (cons $1 $2)))

        (code   ((inst-list chunks) (cons (label #f (block (list->vector $1) #f #f) #f)
                                          $2))
                ((inst-list) (list->vector $1))
                )

        )))

    (define/public (liveness-from-file file)
      (define in-port (open-input-file file))
      (define liveness-map (make-hash))
      (define (parse)
        (define line (read-line in-port))
        (unless (equal? eof line)
                (define match (regexp-match-positions #rx":" line))
                (when match
                      (define pos (cdar match))
                      (define live-regs (map (lambda (x) (string->number (string-trim x)))
                                             (string-split (substring line pos) ",")))
                      (hash-set! liveness-map (substring line 0 pos) live-regs))
                (parse)))
      (parse)
      liveness-map)

    ))