#lang racket

(require "../parser.rkt" "../inst.rkt")
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
      (for*/vector ([line (file->lines file)]
		  [x (string-split line)])
		 (string->inst x)))

    (define/public (info-from-file file)
      (define lines (file->lines file))
      (define live-out
	(for/list ([ele (string-split (first lines) ",")])
		  (let ([toks (string-split ele)])
		    (if (= (length toks) 2)
			(cons (string->symbol (first toks)) (string->number (second toks)))
			(string->symbol (first toks))))))
      (define recv (string->number (second lines)))
      (define assume 
	(if (equal? "-" (string-trim (third lines)))
	    #f
	    (for/list ([ele (string-split (third lines) ",")])
		      (let ([toks (string-split ele)])
			(cons (string->symbol (first toks)) (string->number (second toks)))))))
      (define input-file
	(if (equal? "-" (string-trim (fourth lines)))
	    #f 
	    (fourth lines)))
	
      (values live-out recv assume input-file))
      
    ))
