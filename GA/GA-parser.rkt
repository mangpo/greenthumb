#lang racket

(require "../parser.rkt" "../ast.rkt")
(provide GA-parser%)

(define GA-parser%
  (class parser%
    (super-new)
    (override ast-from-string ast-from-file)

    (define (string->inst x)
      (cond
       [(or (equal? x "_") (equal? x "?"))
	(inst #f #f)]
       [(or (string->number x) (member x (list "up" "down" "left" "right" "io")))
	(inst "@p" x)]
       [else
	(inst x #f)]))

    (define (ast-from-string str)
      (for/vector ([x (string-split str)])
		  (string->inst x)))

    (define (ast-from-file file)
      (for*/list ([line (file->lines file)]
		  [x (string-split line)])
		 (string->inst x)))
    ))
