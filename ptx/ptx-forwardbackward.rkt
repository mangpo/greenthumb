#lang racket

(require "../forwardbackward.rkt" "../ops-racket.rkt" "ptx-machine.rkt")

(provide ptx-forwardbackward%)

(define ptx-forwardbackward%
  (class forwardbackward%
    (super-new)
    (override len-limit)

    ;; Number of instructions that can be synthesized within a minute.
    ;; Try setting it to 5 to start and adjust it later.
    (define (len-limit) 5)

    (define/override (reduce-precision-assume assumption)
      (define regs (vector-copy (progstate-regs assumption)))
      (define preds (progstate-preds assumption))
      (for ([r regs] [i (in-naturals)])
	   (when (pair? r)
		 (let ([op (car r)]
		       [v (cdr r)])
		   (cond
		    [(< v -16)
		     (vector-set! regs i
				  (cons op (finitize (bitwise-ior (bitwise-and v 15) 8) 4)))]
		    [else
		     (vector-set! regs i (cons op (bitwise-and v 7)))]
		    ))))

      (progstate regs preds)
      )

    ))

