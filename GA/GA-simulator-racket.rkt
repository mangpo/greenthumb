#lang racket

(require "../simulator.rkt" "../ops-racket.rkt" 
         "../ast.rkt"
         "../machine.rkt" "GA-machine.rkt")
(provide GA-simulator-racket% get-stack)
 
(define-syntax-rule (modulo- x y) (if (< x 0) (+ x y) x))
(define-syntax-rule (modulo+ x y) (if (>= x 8) (- x y) x))

;; Get item i from the stack
(define (get-stack stack i)
  (vector-ref (stack-body stack) (modulo- (- (stack-sp stack) i) 8)))

(define GA-simulator-racket%
  (class simulator%
    (super-new)
    (init-field machine)
    (override interpret performance-cost)

    (define bit (get-field bit machine))
    (define inst-id (get-field inst-id machine))

    (define debug #f)

    ;; Creates a policy that determines what kind of communication is allowed 
    ;; during interpretation.  The policy is a procedure that takes as input a 
    ;; comm pair and the current comm list.  If the policy allows 
    ;; the given pair to be added to the list, the pair is inserted at the beginning 
    ;; of the list and the result is returned.  If the policy doesn't allow the pair 
    ;; to be added to the list, then an assertion error is thrown.
    (define-syntax comm-policy
      (syntax-rules (all at-most)
	[(comm-policy all) cons]         ; allow everything
	[(comm-policy at-most state)     ; allow only prefixes of the communication sequence observed in the given state 
	 (let ([limit (reverse (progstate-comm state))])
	   (lambda (p comm)
	     (for*/all ([c comm])        ; this is not needed for correctness, but improves performance
		       (let* ([len (length c)]
			      [limit-p (list-ref limit len)])
			 (assert (< len (length limit)) 'comm-length)  
			 (assert (equal? (car p) (car limit-p)) `comm-data)
			 (assert (equal? (cdr p) (cdr limit-p)) `comm-type)
			 (cons limit-p c)))))])) ; can return (cons p c) here, but this is more efficient. 
					; it is correct because we assert that p == limit-p.


    ;; Interpret a given program from a given state.
    ;; code
    ;; state: initial progstate
    ;; policy: a procedure that enforces a communication policy (see the definition of comm-policy above)
    (define (interpret code state [policy #f])
      (set! policy (if policy
		       (comm-policy at-most policy)
		       (comm-policy all)))
      
      (define a (progstate-a state))
      (define b (progstate-b state))
      (define r (progstate-r state))
      (define s (progstate-s state))
      (define t (progstate-t state))
      ;; (define data (stack (stack-sp (progstate-data state))
      ;; 			  (vector-copy (stack-body (progstate-data state)))))
      ;; (define return (stack (stack-sp (progstate-return state))
      ;; 			    (vector-copy (stack-body (progstate-return state)))))
      (define data-sp (stack-sp (progstate-data state)))
      (define data-body (vector-copy (stack-body (progstate-data state))))
      (define return-sp (stack-sp (progstate-return state)))
      (define return-body (vector-copy (stack-body (progstate-return state))))
      (define memory (vector-copy (progstate-memory state)))
      
      (define recv (progstate-recv state))
      (define comm (progstate-comm state))

      ;; Pushes a value to the given stack's body.
      (define-syntax-rule (push-stack! x-sp x-body value)
	(begin
	  (set! x-sp (modulo+ (add1 x-sp) 8))
	  (vector-set! x-body x-sp value)))

      ;; Pops from the given stack's body.
      (define-syntax-rule (pop-stack! x-sp x-body)
	(let ([ret-val (vector-ref x-body x-sp)])
	  (set! x-sp (modulo- (sub1 x-sp) 8))
	  ret-val))

      ;; Pushes to the data stack.
      (define (push! value)
	(push-stack! data-sp data-body s)
	(set! s t)
	(set! t value))
      
      ;; Pushes to the return stack.
      (define (r-push! value)
	(push-stack! return-sp return-body r)
	(set! r value))
      
      ;; Pops from the data stack.
      (define (pop!)
	(let ([ret-val t])
	  (set! t s)
	  (set! s (pop-stack! data-sp data-body))
	  ret-val))
      
      ;; Pops from the return stack.
      (define (r-pop!)
	(let ([ret-val r])
	  (set! r (pop-stack! return-sp return-body))
          ret-val))
      
      ;; Define comm-type
      (define comm-dict (hash UP 0 DOWN 1 LEFT 2 RIGHT 3 IO 4))
      
      ;; Read from the given memory address or communication port. If it
      ;; gets a communication port, it just returns a random number (for
      ;; now).
      (define (read-memory addr)
	(define (read port)
	  (let ([val (car recv)]
		[type (hash-ref comm-dict port)])
	    (set! comm (policy (cons val type) comm))
	    (set! recv (cdr recv))
	    val))
	(cond
	 [(equal? addr UP)    (read UP)]
	 [(equal? addr DOWN)  (read DOWN)]
	 [(equal? addr LEFT)  (read LEFT)]
	 [(equal? addr RIGHT) (read RIGHT)]
	 [(equal? addr IO)    (read IO)]
	 [else (vector-ref memory addr)]))
      
      ;; Write to the given memeory address or communication
      ;; port. Everything written to any communication port is simply
      ;; aggregated into a list.
      (define (set-memory! addr val)
	(when debug (pretty-display `(set-memory! ,addr ,val)))
	(define (write port)
	  (let ([type (+ 5 (hash-ref comm-dict port))])
	    (set! comm (policy (cons val type) comm))
	    ))
	(cond
	 [(equal? addr UP)    (write UP)]
	 [(equal? addr DOWN)  (write DOWN)]
	 [(equal? addr LEFT)  (write LEFT)]
	 [(equal? addr RIGHT) (write RIGHT)]
	 [(equal? addr IO)    (write IO)]
	 [else (vector-set! memory addr val)]))

      (define (clip x)
	(let ([res (bitwise-and x #x3ffff)])
	  (if (= (bitwise-and #x20000 res) 0)
	      res
	      (- (add1 (bitwise-xor res #x3ffff))))))

      (define (push-right-one x carry)
	(clip (bitwise-ior (<< (bitwise-and #x1 carry) (sub1 bit) bit) (>>> x 1 bit))))
      
      ;; Treats T:A as a single 36 bit register and shifts it right by one
      ;; bit. The most signficicant bit (T17) is kept the same.
      (define (multiply-step-even!)
	(set! a (push-right-one a t))
	(set! t (>> t 1)))
      
      ;; Sums T and S and concatenates the result with A, shifting
      ;; the concatenated 37-bit to the right by one bit.
      (define (multiply-step-odd!)
	(let ([sum (+ t s)])
	  (set! a (push-right-one a sum))
	  (set! t (>> sum 1))))

      (define (interpret-step inst-const)
	(define inst (inst-op inst-const))
	(define const (inst-args inst-const))
	(when debug (pretty-display `(interpret-step ,inst ,const)))
	(define-syntax-rule (inst-eq x) (equal? inst (vector-member x inst-id)))
	(cond
	 [(inst-eq `@p)   (push! const)]
	 [(inst-eq `@+)   (push! (read-memory a)) (set! a (add1 a))]
	 [(inst-eq `@b)   (push! (read-memory b))]
	 [(inst-eq `@)    (push! (read-memory a))]
	 [(inst-eq `!+)   (set-memory! a (pop!)) (set! a (add1 a))]
	 [(inst-eq `!b)   (set-memory! b (pop!))]
	 [(inst-eq `!)    (set-memory! a (pop!))]
	 [(inst-eq `+*)   (if (= (bitwise-and #x1 a) 0)
	 		      (multiply-step-even!)
	 		      (multiply-step-odd!))]
	 [(inst-eq `2*)   (set! t (clip (<< t 1 bit)))]
	 [(inst-eq `2/)   (set! t (>> t 1))] ;; sign shiftx
	 [(inst-eq `-)    (set! t (bitwise-not t))]
	 [(inst-eq `+)    (push! (clip (+ (pop!) (pop!))))]
	 [(inst-eq `and)  (push! (bitwise-and (pop!) (pop!)))]
	 [(inst-eq `or)   (push! (bitwise-xor (pop!) (pop!)))]
	 [(inst-eq `drop) (pop!)]
	 [(inst-eq `dup)  (push! t)]
	 [(inst-eq `pop)  (push! (r-pop!))]
	 [(inst-eq `over) (push! s)]
	 [(inst-eq `a)    (push! a)]
	 [(inst-eq `nop)  (void)]
	 [(inst-eq `push) (r-push! (pop!))]
	 [(inst-eq `b!)   (set! b (pop!))]
	 [(inst-eq `a!)   (set! a (pop!))]
	 [else (assert #f (format "invalid instruction ~a" inst))])
	 )

      (define (interpret-struct x)
	(when debug (pretty-display `(interpret-struct ,x)))
	(cond
	 [(inst? x)
	  (interpret-step x)]

	 [(or (vector? x) (list? x))
	  (for ([i x]) (interpret-struct i))]
	 
	 [(block? x)
	  (interpret-struct (block-body x))]

	 [(forloop? x)
	  (interpret-struct (forloop-init x))
	  (r-push! (pop!))
	  (for ([i (in-range (add1 r))])
	       (interpret-struct (forloop-body x))
	       (set! r (sub1 r)))
	  (r-pop!)
	  ]

	 [(ift? x)
	  (when (not (equal? t 0))
		(interpret-struct (ift-t x)))]

	 [(iftf? x)
	  (if (not (equal? t 0))
	      (interpret-struct (iftf-t x))
	      (interpret-struct (iftf-f x)))]

	 [(-ift? x)
	  (when (negative? t) ;(or (negative? t) (>= t (arithmetic-shift 1 (sub1 bit)))) ;; negative
		(interpret-struct (-ift-t x)))]

	 [(-iftf? x)
	  (if (negative? t) ;(or (negative? t) (>= t (arithmetic-shift 1 (sub1 bit)))) ;; negative
	      (interpret-struct (-iftf-t x))
	      (interpret-struct (-iftf-f x)))]

	 [else (raise (format "interpret-struct: unimplemented for ~a" x))]
	 ))
      
      (interpret-struct code)
      (progstate a b r s t 
		 (stack data-sp data-body)
		 (stack return-sp return-body)
		 memory recv comm)
      )

    (define (performance-cost code)

      (define (cost-step inst-const)
	(define inst (inst-op inst-const))
	(define-syntax-rule (inst-eq x) (equal? inst (vector-member x inst-id)))
	(cond
	 [(inst-eq `@p) 5]
	 [(inst-eq `+) 2]
	 [(inst-eq `nop) 0]
	 [else 1]))

      (define (cost-struct x)
	(cond
	 [(inst? x)
	  (cost-step x)]
	 [(list? x)
	  (foldl (lambda (i all) (+ all (cost-struct i))) 0 x)]
	 [(vector? x)
	  (foldl (lambda (i all) (+ all (cost-struct i))) 0 (vector->list x))]
	 [(block? x)   (cost-struct (block-body x))]
	 [(forloop? x) (+ (cost-struct (forloop-init x)) (cost-struct (forloop-body x)))]
	 [(ift? x)     (cost-struct (ift-t x))]
	 [(iftf? x)    (+ (cost-struct (iftf-t x)) (cost-struct (iftf-f x)))]
	 [(-ift? x)    (cost-struct (-ift-t x))]
	 [(-iftf? x)   (+ (cost-struct (-iftf-t x)) (cost-struct (-iftf-f x)))]
	 [else (raise (format "cost-struct: unimplemented for ~a" x))]
	 ))

      (cost-struct code))

    ))
