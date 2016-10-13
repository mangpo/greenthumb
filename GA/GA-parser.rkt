#lang racket

(require "../parser.rkt" "../inst.rkt")
(provide GA-parser%)

(define GA-parser%
  (class parser%
    (super-new)
    (override ir-from-string ir-from-file)

    (define (string->inst x)
      (cond
       [(or (equal? x "_") (equal? x "?"))
	(inst #f #f)]
       [(or (string->number x) (member x (list "up" "down" "left" "right" "io")))
	(inst "@p" x)]
       [else
	(inst x #f)]))

    (define (ir-from-string str)
      (for/vector ([x (string-split str)])
		  (string->inst x)))

    (define (ir-from-file file)
      (for*/vector ([line (file->lines file)]
		  [x (string-split line)])
		 (string->inst x)))

    (define/override (info-from-file file)
      (define lines (file->lines file))
      (define live-out
	(for/list ([ele (string-split (first lines) ",")])
		  (let ([toks (string-split ele)])
		    (if (= (length toks) 2)
			(cons (string->symbol (first toks)) (string->number (second toks)))
			(string->symbol (first toks))))))
      (define assume 
	(if (equal? "-" (string-trim (second lines)))
	    #f
	    (for/list ([ele (string-split (second lines) ",")])
		      (let ([toks (string-split ele)])
			(cons (string->symbol (first toks)) (string->number (second toks)))))))
      (define input-file
	(if (equal? "-" (string-trim (third lines)))
	    #f 
	    (third lines)))
	
      (values live-out assume input-file))
      
    ))
