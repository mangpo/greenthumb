#lang s-exp rosette
(require "../validator.rkt" "ptx-machine.rkt")
(provide ptx-validator%)

(define ptx-validator%
  (class validator%
    (super-new)
    (override get-constructor)

    (define (get-constructor) ptx-validator%)

    (define/override (assume state constraint)
      (cond
       [constraint
	(define regs (progstate-regs state))
	(define preds (progstate-preds state))
	(define c-regs (progstate-regs constraint))
	(define c-preds (progstate-preds constraint))

	
	(define (check item assumption)
	  (when (pair? assumption)
		(cond
		 [(member (car assumption) (list "=" '=))
		  (if (list? (cdr assumption))
		      (assert (member item (cdr assumption)))
		      (assert (equal? item (cdr assumption))))]
		 [(member (car assumption) (list "<=" '<=))
		  ;;(pretty-display `(and (<= ,item ,(cdr assumption)) (>= ,item 0)))
		  (assert (and (<= item (cdr assumption)) (>= item 0)))])
		))
	
	(for ([r regs] [c c-regs]) (check r c))
	(for ([r preds] [c c-preds]) (check r c))
	
	]))

    ))

