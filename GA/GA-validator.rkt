#lang s-exp rosette

(require "../validator.rkt"
         "GA-ops-rosette.rkt"
	 "GA-machine.rkt" "GA-simulator-rosette.rkt")

(provide GA-validator%)

(define GA-validator%
  (class validator%
    (super-new)
    (override assume get-constructor assert-state-eq)

    (define (get-constructor) GA-validator%)

    ;; Assert assumption about start-state
    (define (assume state constraint)
      (define (check item assumption)
	(when (pair? assumption)
	      (cond
	       [(member (car assumption) (list "=" '=))
		(if (list? (cdr assumption))
		    (assert (member item (cdr assumption)))
		    (assert (equal? item (cdr assumption))))]
	       [(member (car assumption) (list "<=" '<=))
		(assert (and (<= item (cdr assumption)) (>= item 0)))])
	      ))

      (define-syntax-rule (check-reg progstate-x)
	(check (progstate-x state) (progstate-x constraint)))
      
      (define-syntax-rule (check-stack progstate-x)
	(when (progstate-x constraint)
	      (for ([i (in-range 8)])
		   (check (get-stack (progstate-x state) i)
			  (get-stack (progstate-x constraint) i)))))

      (when constraint
            (check-reg progstate-a)
            (check-reg progstate-b)
            (check-reg progstate-r)
            (check-reg progstate-s)
            (check-reg progstate-t)
            (check-stack progstate-data)
            (check-stack progstate-return)))

    
    (define (assert-state-eq state1 state2 pred)
      (define (check-stack s1 s2 p)
	(when p
	      (for ([i p])
                   (assert (equal? (get-stack s1 i) (get-stack s2 i)) `stack))))
            
      (for ([s1 state1]
            [s2 state2]
            [p pred])
           (cond
            [(stack? s1) (check-stack s1 s2 p)]
            [else (super assert-state-eq s1 s2 p)]))
      )


    ))
