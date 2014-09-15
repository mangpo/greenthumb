#lang s-exp rosette

(require "../solver.rkt"
         "../ast.rkt"
         "../machine.rkt" "GA-machine.rkt" "GA-simulator-rosette.rkt")
(provide GA-solver%)

(define GA-solver%
  (class solver%
    (super-new)
    (inherit-field printer machine simulator)
    (inherit sym-op sym-arg)
    (override get-sym-vars evaluate-state
              encode-sym decode-sym sym-insts
              assume assert-output)

    (set! simulator (new GA-simulator-rosette% [machine machine]))

    (define (sym-insts size)
      (encode-sym (for/vector ([i size]) (inst #f #f))))

    (define (get-sym-vars state)
      (define lst (list))
      (define (add x)
	(when (term? x)
	      (set! lst (cons x lst))))

      (define (add-var progstate-x)
	(add (progstate-x state)))

      (define (add-vector body)
	(for ([i (in-range (vector-length body))])
	     (add (vector-ref body i))))

      (add-var progstate-a)
      (add-var progstate-b)
      (add-var progstate-r)
      (add-var progstate-s)
      (add-var progstate-t)
      (add (stack-sp (progstate-data state)))
      (add (stack-sp (progstate-return state)))
      (add-vector (stack-body (progstate-data state)))
      (add-vector (stack-body (progstate-return state)))
      (add-vector (progstate-memory state))
      (for ([x (progstate-recv state)])
	   (add x))

      lst)

    (define (evaluate-state state sol)
      (define (eval x)
        (let ([ans (evaluate x sol)])
          (if (term? ans) 0 ans)))
      
      (define a (eval (progstate-a state)))
      (define b (eval (progstate-b state)))
      (define r (eval (progstate-r state)))
      (define s (eval (progstate-s state)))
      (define t (eval (progstate-t state)))
      (define data (stack (eval (stack-sp (progstate-data state)))
			  (vector-map eval (stack-body (progstate-data state)))))
      (define return (stack (eval (stack-sp (progstate-return state)))
			    (vector-map eval (stack-body (progstate-return state)))))
      (define memory (vector-map eval (progstate-memory state)))
      
      (define recv (map eval (progstate-recv state)))
      (define comm (map (lambda (xy) (cons (eval (car xy)) (eval (cdr xy)))) (progstate-comm state)))
      (progstate a b r s t data return memory recv comm))

    (define (encode-sym code)
      (define (encode-inst-sym x)
        (if (inst-op x)
            (send printer encode-inst x)
            (inst (sym-op) (sym-arg))))
      
      (traverse code inst? encode-inst-sym))

    (define (decode-sym code model)
      (define (decode-inst-sym x)
        (send
         printer decode-inst
         (inst (evaluate (inst-op x) model)
               (evaluate (inst-args x) model))))

      (traverse code inst? decode-inst-sym))

    (define (assert-output state1 state2 constraint)
      (define (check-reg progstate-x)
	(when (progstate-x constraint)
	      ;;(pretty-display `(check-reg ,(equal? (progstate-x state1) (progstate-x state2))))
	      (assert (equal? (progstate-x state1) (progstate-x state2)) `progstate-x)))
      
      (define-syntax-rule (check-stack progstate-x)
	(when (progstate-x constraint)
	      (for ([i (in-range (progstate-x constraint))])
		   ;; (pretty-display `(check-stack ,(equal? (get-stack (progstate-x state1) i) 
		   ;;                                        (get-stack (progstate-x state2) i))))
		   (assert (equal? (get-stack (progstate-x state1) i) 
				   (get-stack (progstate-x state2) i))
			   `progstate-x))))
      
      (define-syntax-rule (check-mem)
	(let ([mem1 (progstate-memory state1)]
	      [mem2 (progstate-memory state2)]
	      [mem-const (progstate-memory constraint)])
	  (if (vector? mem-const)
	      (for ([i (in-range 0 (vector-length mem1))])
		   (when (vector-ref mem-const i)
			 ;;(pretty-display `(check-mem ,(equal? (vector-ref mem1 i) (vector-ref mem2 i))))
			 (assert (equal? (vector-ref mem1 i) (vector-ref mem2 i)) 
				 `progstate-mem)))
	      (when mem-const
		    (for ([i (in-range 0 (vector-length mem1))])
			 (assert (equal? (vector-ref mem1 i) (vector-ref mem2 i)) 
				 `progstate-mem))))
	  ))
      
      (define-syntax-rule (check-comm)
	(when (progstate-comm constraint)
	      ;; (pretty-display `(check-comm-length ,(equal? (length (progstate-comm state1)) 
	      ;;                                              (length (progstate-comm state2)))))
	      (assert (equal? (length (progstate-comm state1)) 
			      (length (progstate-comm state2))) `comm-length)
	      (for*/all ([j1 (progstate-comm state1)]
			 [j2 (progstate-comm state2)])
			(for ([i1 j1]
			      [i2 j2])
			     (assert (equal? (car i1) (car i2)) `comm-data)
			     (assert (equal? (cdr i1) (cdr i2)) `comm-type)))
	      ))

      (check-reg progstate-a)
      (check-reg progstate-b)
      (check-reg progstate-r)
      (check-reg progstate-s)
      (check-reg progstate-t)
      (check-stack progstate-data)
      (check-stack progstate-return)
      (check-mem)
      (check-comm)
      )
    

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

      (define (check-reg progstate-x)
	(check (progstate-x state) (progstate-x constraint)))
      
      (define (check-stack progstate-x)
	(when (progstate-x constraint)
	      (for ([i (in-range 8)])
		   (check (get-stack (progstate-x state) i)
			  (get-stack (progstate-x constraint) i)))))
      
      (define (check-mem)
	(define mem-state (progstate-memory state))
	(define mem-constraint (progstate-memory constraint))
	(when (= (vector-length mem-constraint)
		 (vector-length mem-state))
	      (for ([i (in-range 0 (vector-length mem-state))])
		   (check (vector-ref mem-state i) (vector-ref mem-constraint i)))))

      (when constraint
            (check-reg progstate-a)
            (check-reg progstate-b)
            (check-reg progstate-r)
            (check-reg progstate-s)
            (check-reg progstate-t)
            (check-stack progstate-data)
            (check-stack progstate-return)
            (check-mem))
      )
      
    ))
