#lang racket

(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)

(provide (all-defined-out))

(define parser%
  (class object%
    (super-new)
    ;; Required fields to be intialized when extended.
    (init-field [asm-parser #f] [asm-lexer #f])
    (public ir-from-string ir-from-file info-from-file)
    
    (define (lex-this input)
      (lambda ()
        (let ([token (asm-lexer input)])
          ;;(pretty-display token)
          token)))

    (define (ast input)
      (asm-parser (lex-this input)))

    (define (ir-from-string s)
      (let ((input (open-input-string s)))
        (ast input)))

    (define (ir-from-file file)
      (and (file-exists? file)
           (let ((input (open-input-file file)))
             (port-count-lines! input)
             (ast input))))

    ;; Read from file and convert file content info into the format we want.
    ;; Info usually includes live-out information.
    ;; It can also contain extra information such as precondition of the inputs.
    (define (info-from-file filename)
      (raise "info-from-file: unimplemented. Need to extend this function."))

    ))

(define-lex-trans number
  (syntax-rules ()
    ((_ digit)
     (re-: (uinteger digit)
           (re-? (re-: "." (re-? (uinteger digit))))))))

(define-lex-trans uinteger
  (syntax-rules ()
    ((_ digit) (re-+ digit))))

(define-lex-abbrevs
  (digit10 (char-range "0" "9"))
  (number10 (number digit10))
  (snumber10 (re-or number10 (re-seq "-" number10)))
  (identifier-characters (re-or (char-range "A" "Z") (char-range "a" "z") "-" "@" "_" "+")) ;;  TODO: notice no .
  (identifier-characters-ext (re-or digit10 identifier-characters))
  (identifier (re-seq identifier-characters
                      (re-* identifier-characters-ext)))
  (identifier: (re-seq identifier ":")))
