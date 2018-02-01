#lang rosette

(require "../validator.rkt" "llvm-machine.rkt")
(provide llvm-validator%)

(define llvm-validator%
  (class validator%
    (super-new)
    (override get-constructor assume)

    (define (get-constructor) llvm-validator%)

    (define (assume state assumption)
      (define (symbol->op s)
	(cond
	 [(equal? s '>) >]
	 [(equal? s '>=) >=]
	 [(equal? s '<) <]
	 [(equal? s '<=) <=]
	 [(equal? s '=) =]))

      ;;(pretty-display `(assume ,state ,assumption))
      (when assumption
	    (define vars (progstate-var state))
	    (define constraints (progstate-var assumption))

	    (for ([v vars]
		  [l constraints])
		 (when l
		       (for ([pair l])
			    (when pair
				  (let ([op (symbol->op (car pair))]
					[n  (cdr pair)])
				    ;;(pretty-display `(assert (,op ,v ,n)))
				    (assert (op v n))
				    ))))))

      )

    ))
