#lang racket

(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc
	 "../parser.rkt" "../inst.rkt")

(provide arm-parser%)

(define arm-parser%
  (class parser%
    (super-new)
    (inherit-field asm-parser asm-lexer)

    (define-tokens a (LABEL BLOCK WORD _WORD NUM REG))
    (define-empty-tokens b (EOF TEXT COMMA DQUOTE HOLE HASH LSQBR RSQBR NOP))

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
      (identifier-characters (re-or (char-range "A" "Z") (char-range "a" "z")))
      (identifier-characters-ext (re-or digit10 identifier-characters "_"))
      ;(identifier (re-+ identifier-characters))
      (identifier (re-seq identifier-characters 
                          (re-* (re-or identifier-characters digit10))))
      (identifier: (re-seq identifier ":"))
      (_identifier (re-seq "_" (re-* identifier-characters-ext)))
      (reg (re-or "fp" "ip" "lr" "sl" (re-seq "r" number10)))
      )

    (set! asm-lexer
      (lexer-src-pos
       ("nop"      (token-NOP))
       (".text"    (token-TEXT))
       (","        (token-COMMA))
       ("\""       (token-DQUOTE))
       ("?"        (token-HOLE))
       ("#"        (token-HASH))
       ("["        (token-LSQBR))
       ("]"        (token-RSQBR))
       (reg        (token-REG lexeme))
       (identifier: (token-LABEL lexeme))
       (identifier (token-WORD lexeme))
       (_identifier (token-_WORD lexeme))
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

        (arg  ((REG) $1)
              ((HASH NUM) $2)
              ((NUM) $1))

	(arg-pair
	      ((WORD arg) (list $1 $2))
	      ((LSQBR REG COMMA arg RSQBR) (list $2 $4)))

        (args ((arg) (list $1))
	      ((arg-pair) $1)
              ((arg COMMA args) (cons $1 $3))
	      ((arg-pair COMMA args) (append $1 $3)))

        (instruction ((WORD args) (create-inst $1 (list->vector $2)))
		     ((WORD _WORD) (create-special-inst $1 $2))
                     ((NOP)       (create-inst "nop" (vector)))
		     ((HOLE) (inst #f #f)))

        (inst-list   (() (list))
                     ((instruction inst-list) (cons $1 $2)))

        (code   ((inst-list) (list->vector $1))
                )

        )))

    (define (create-special-inst op1 op2)
      (cond
       [(equal? op2 "__aeabi_idiv")
	(inst (vector "sdiv" "" "") (vector "r0" "r0" "r1"))]
       [(equal? op2 "__aeabi_uidiv")
	(inst (vector "udiv" "" "") (vector "r0" "r0" "r1"))]
       [else
	(raise (format "Undefine special instruction: ~a ~a" op1 op2))]))

    (define (create-inst op args)
      (define args-len (vector-length args))
      (cond
       [(and (>= args-len 4) 
	     (member (string->symbol (vector-ref args (- args-len 2)))
                     '(asr asl lsr lsl ror)))

        (define shfop (vector-ref args (- args-len 2)))
        (when (equal? shfop "asl") (set! shfop "lsl"))

        (define base (create-inst op (vector-append (vector-copy args 0 (- args-len 2))
                                                    (vector (vector-ref args (- args-len 1))))))
        (define ops-vec (inst-op base))
        (vector-set! ops-vec 2 shfop)
        base]

       [else
	(when (equal? op "asl") (set! op "lsl"))
	(define op-len (string-length op))
	;; Determine type
	(define cond-type (substring op (- op-len 2)))
	(define cond?
          (and (member cond-type (list "eq" "ne" "ls" "hi" "cc" "cs" "lt" "ge")) 
               (> op-len 3)
               (not (equal? op "smmls"))))
	;; ls
	(set! cond-type (if cond? cond-type ""))
	(when cond? (set! op (substring op 0 (- op-len 2))))

	;; for ldr & str, fp => r99, divide offset by 4
	(when (or (equal? op "str") (equal? op "ldr"))
	      (define offset (vector-ref args 2))
	      (unless (equal? (substring offset 0 1) "r")
		      (vector-set! 
		       args 2
		       (number->string (quotient (string->number offset) 4)))))

        (inst (vector op cond-type "") (vector-map rename args))]))

    (define (rename x)
      (cond
       [(equal? x "sb") "r9"]
       [(equal? x "sl") "r10"]
       [(equal? x "fp") "r11"]
       [(equal? x "ip") "r12"]
       [(equal? x "sp") "r13"]
       [(equal? x "lr") "r14"]
       [(equal? x "pc") "r15"]
       [else x]))

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

    (define/override (info-from-file file)
      (define lines (file->lines file))
      (define live-out (map (lambda (x) (or (string->number x) x))
                            (string-split (first lines) ",")))
      live-out)

    ))
