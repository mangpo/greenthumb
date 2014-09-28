#lang s-exp rosette

(require "../simulator.rkt" "../ops-rosette.rkt" 
         "../ast.rkt"
         "../machine.rkt" "GA-machine.rkt")
(provide GA-simulator-rosette% get-stack)
 
(define-syntax-rule (modulo- x y) (if (< x 0) (+ x y) x))
(define-syntax-rule (modulo+ x y) (if (>= x 8) (- x y) x))

;; Get item i from the stack
(define (get-stack stack i)
  (vector-ref (stack-body stack) (modulo- (- (stack-sp stack) i) 8)))

(define GA-simulator-rosette%
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
			 (assert (equal? (first p) (first limit-p)) `comm-data)
			 (assert (equal? (second p) (second limit-p)) `comm-port)
			 (assert (equal? (third p) (third limit-p)) `comm-read-write)
			 (cons limit-p c)))))])) ; can return (cons p c) here, but this is more efficient. 
					; it is correct because we assert that p == limit-p.


    ;; Interpret a given program from a given state.
    ;; code
    ;; state: initial progstate
    ;; policy: a procedure that enforces a communication policy (see the definition of comm-policy above)
    (define (interpret code state [policy #f] #:dep [dep #f])
      (set! policy (if policy
		       (comm-policy at-most policy)
		       (comm-policy all)))
      
      (define a (progstate-a state))
      (define b (progstate-b state))
      (define r (progstate-r state))
      (define s (progstate-s state))
      (define t (progstate-t state))
      (define data-sp (stack-sp (progstate-data state)))
      (define data-body (vector-copy (stack-body (progstate-data state))))
      (define return-sp (stack-sp (progstate-return state)))
      (define return-body (vector-copy (stack-body (progstate-return state))))
      (define memory (vector-copy (progstate-memory state)))
      
      (define recv (progstate-recv state))
      (define comm (progstate-comm state))
      
      (define a-dep (progstate-a state))
      (define b-dep (progstate-b state))
      (define r-dep (progstate-r state))
      (define s-dep (progstate-s state))
      (define t-dep (progstate-t state))
      (define data-sp-dep (stack-sp (progstate-data state)))
      (define data-body-dep (and dep (vector-copy (stack-body (progstate-data state)))))
      (define return-sp-dep (stack-sp (progstate-return state)))
      (define return-body-dep (and (vector-copy (stack-body (progstate-return state)))))
      (define memory-dep (and dep (vector-copy (progstate-memory state))))
      (define comm-dep (list))
      (define inter (list))
      (define init-vals (append (list 0 a r s t) 
				(vector->list data-body) 
				(vector->list return-body)))
      
      (define-syntax add-inter
        (syntax-rules ()
          ((add-inter x) (set! inter (cons x inter)))
          ((add-inter x y ...) (set! inter (append (list x y ...) inter)))))

      (define-syntax-rule (create-node val p) (create-node-ex val p init-vals))

      ;; Pushes a value to the given stack's body.
      (define-syntax-rule (push-stack! x-sp x-body value)
	(begin
	  (set! x-sp (modulo+ (add1 x-sp) 8))
	  (vector-set! x-body x-sp value)
	  ))

      ;; Pops from the given stack's body.
      (define-syntax-rule (pop-stack! x-sp x-body)
	(let ([ret-val (vector-ref x-body x-sp)])
	  (set! x-sp (modulo- (sub1 x-sp) 8))
	  ret-val))

      ;; Pushes to the data stack.
      (define (push! value node [new #t])
	(push-stack! data-sp data-body s)
	(set! s t)
	(set! t value)
	(if dep
	    (begin
	      (push-stack! data-sp-dep data-body-dep s-dep)
	      (set! s-dep t-dep)
	      (set! t-dep node)
	      )
	    (when new (add-inter value)))
	)
      
      ;; Pushes to the return stack.
      (define (r-push! value node)
	(push-stack! return-sp return-body r)
	(set! r value)
	(when dep
	      (push-stack! return-sp-dep return-body-dep r-dep)
	      (set! r-dep node))
	)
      
      ;; Pops from the data stack.
      (define (pop!)
	(let ([ret-val t]
	      [ret-val-dep t-dep])
	  (set! t s)
	  (set! s (pop-stack! data-sp data-body))
	  (when dep
		(set! t-dep s-dep)
		(set! s-dep (pop-stack! data-sp-dep data-body-dep)))
	  (values ret-val ret-val-dep)))
      
      ;; Pops from the return stack.
      (define (r-pop!)
	(let ([ret-val r]
	      [ret-val-dep r-dep])
	  (set! r (pop-stack! return-sp return-body))
	  (when dep 
		(set! r-dep (pop-stack! return-sp-dep return-body-dep)))
          (values ret-val ret-val-dep)))
      
      ;; Read from the given memory address or communication port. If it
      ;; gets a communication port, it just returns a random number (for
      ;; now).
      (define (read-memory addr)
	(define (read port)
	  (let ([val (car recv)])
	    (set! comm (policy (list val port 0) comm))
	    (when dep (set! comm-dep (cons (list val port 0) comm-dep)))
	    (set! recv (cdr recv))
	    val))
	(cond
	 [(equal? addr UP)    (values (read UP) #f)]
	 [(equal? addr DOWN)  (values (read DOWN) #f)]
	 [(equal? addr LEFT)  (values (read LEFT) #f)]
	 [(equal? addr RIGHT) (values (read RIGHT) #f)]
	 [(equal? addr IO)    (values (read IO) #f)]
	 [else (values (vector-ref memory addr)
		       (and dep (vector-ref memory-dep addr)))]))
      
      ;; Write to the given memeory address or communication
      ;; port. Everything written to any communication port is simply
      ;; aggregated into a list.
      (define (set-memory! addr val node)
	(when debug (pretty-display `(set-memory! ,addr ,val)))
	(define (write port)
          (set! comm (policy (list val port 1) comm))
	  (when dep (set! comm-dep (cons (list node port 1) comm-dep))))
	(cond
	 [(equal? addr UP)    (write UP)]
	 [(equal? addr DOWN)  (write DOWN)]
	 [(equal? addr LEFT)  (write LEFT)]
	 [(equal? addr RIGHT) (write RIGHT)]
	 [(equal? addr IO)    (write IO)]
	 [else 
	  (vector-set! memory addr val)
	  (when dep (vector-set! memory-dep addr node))]))

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
	(let ([a-val (push-right-one a t)]
	      [t-val (>> t 1)])
	  (set! a a-val)
	  (set! t t-val)
	  (if dep
	      (begin
		(set! a-dep (create-node a-val (list a-dep t-dep)))
		(set! t-dep (create-node t-val (list t-dep))))
	      (add-inter a-val t-val))))
      
      ;; Sums T and S and concatenates the result with A, shifting
      ;; the concatenated 37-bit to the right by one bit.
      (define (multiply-step-odd!)
	(let* ([sum (+ t s)]
	       [a-val (push-right-one a sum)]
	       [t-val (>> sum 1)])
	  (set! a a-val)
	  (set! t t-val)
	  (if dep
	      (begin
		(set! a-dep (create-node a-val (list a-dep t-dep s-dep)))
		(set! t-dep (create-node t-val (list t-dep s-dep))))
	      (add-inter a-val t-val))))

      (define-syntax-rule (mem-to-stack addr addr-dep)
	(let-values ([(val val-dep) (read-memory addr)])
	  (let ([p (if (node? val-dep) (node-p val-dep) (list))])
	    (push! val 
                   (create-node #f (list (create-node val (list val-dep))
                                         (create-node addr (list addr-dep))))))))
                   ;(and dep (create-node val (cons addr-dep p)))))))

      (define-syntax-rule (stack-to-mem addr addr-dep)
	(let-values ([(val val-dep) (pop!)])
	  (let ([p (if (node? val-dep) (node-p val-dep) (list))])
	    (set-memory! addr val 
                         (create-node #f (list (create-node val (list val-dep))
                                               (create-node addr (list addr-dep))))))))
                         ;(and dep (create-node val (cons addr-dep p)))))))

      (define-syntax-rule (stack-1 f)
	(let ([val (f t)])
	  (set! t val)
	  (if dep
	      (set! t-dep (create-node val (list t-dep)))
	      (add-inter val))))

      (define-syntax-rule (stack-2 f)
	(let-values ([(val1 val1-dep) (pop!)]
		     [(val2 val2-dep) (pop!)])
	  (let ([val (f val1 val2)])
	    (push! val (and dep (create-node val (list val1-dep val2-dep)))))))

      (define (interpret-step inst-const)
	(define inst (inst-op inst-const))
	(define const (inst-args inst-const))
	(when debug (pretty-display `(interpret-step ,inst ,const)))
	(define-syntax-rule (inst-eq x) (equal? inst (vector-member x inst-id)))
	(cond
	 [(inst-eq `@p)   (push! const (and dep (create-node const (list))))]
	 [(inst-eq `@+)   (mem-to-stack a a-dep)
	                  (set! a (add1 a))]
	 [(inst-eq `@b)   (mem-to-stack b b-dep)]
	 [(inst-eq `@)    (mem-to-stack a a-dep)]
	 [(inst-eq `!+)   (stack-to-mem a a-dep)
	                  (set! a (add1 a))]
	 [(inst-eq `!b)   (stack-to-mem b b-dep)]
	 [(inst-eq `!)    (stack-to-mem a a-dep)]
	 [(inst-eq `+*)   (if (= (bitwise-and #x1 a) 0)
	 		      (multiply-step-even!)
	 		      (multiply-step-odd!))];
	 [(inst-eq `2*)   (stack-1 (lambda (t) (clip (<< t 1 bit))))]
	 [(inst-eq `2/)   (stack-1 (lambda (t) (>> t 1)))];; sign shiftx
	 [(inst-eq `-)    (stack-1 bitwise-not)]
	 [(inst-eq `+)    (stack-2 (lambda (x y) (clip (+ x y))))]
	 [(inst-eq `and)  (stack-2 bitwise-and)]
	 [(inst-eq `or)   (stack-2 bitwise-xor)]
	 [(inst-eq `drop) (pop!)]
	 [(inst-eq `dup)  (push! t t-dep #f)]
	 [(inst-eq `pop)  (let-values ([(val val-dep) (r-pop!)])
			    (push! val val-dep #f))]
	 [(inst-eq `over) (push! s s-dep #f)]
	 [(inst-eq `a)    (push! a a-dep #f)]
	 [(inst-eq `nop)  (void)]
	 [(inst-eq `push) (let-values ([(val val-dep) (pop!)])
			    (r-push! val val-dep))]
	 [(inst-eq `b!)   (let-values ([(val val-dep) (pop!)])
			    (set! b val)
			    (set! b-dep val-dep))]
	 [(inst-eq `a!)   (let-values ([(val val-dep) (pop!)])
			    (set! a val)
			    (set! a-dep val-dep))]
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
	  (let-values ([(val val-dep) (pop!)])
	    (r-push! val val-dep))
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

      ;; (cond
      ;;  [dep
      ;; 	(pretty-display ">>> a")
      ;; 	(display-node a-dep)
      ;; 	(pretty-display ">>> b")
      ;; 	(display-node b-dep)
      ;; 	(pretty-display ">>> r")
      ;; 	(display-node r-dep)
      ;; 	(pretty-display ">>> t")
      ;; 	(display-node t-dep)
      ;; 	(pretty-display ">>> s")
      ;; 	(display-node s-dep)
      ;; 	(for ([x data-body-dep]
      ;; 	      [i 8])
      ;; 	     (pretty-display (format ">>> stack[~a]" i))
      ;; 	     (display-node x))
      ;; 	(for ([x return-body-dep]
      ;; 	      [i 8])
      ;; 	     (pretty-display (format ">>> rstack[~a]" i))
      ;; 	     (display-node x))
      ;; 	(for ([x memory-dep]
      ;; 	      [i (vector-length memory)])
      ;; 	     (pretty-display (format ">>> mem[~a]" i))
      ;; 	     (display-node x))
      ;; 	(for ([x comm-dep]
      ;; 	      [i (vector-length memory)])
      ;; 	     (pretty-display (format ">>> comm[~a]" i))
      ;; 	     (display-node (car x)))

      ;; 	]
      ;;  [else (pretty-display `(inter ,inter))])

      (define extra
	(if dep
	    (progstate a-dep b-dep r-dep s-dep t-dep
		       (stack data-sp-dep data-body-dep)
		       (stack return-sp-dep return-body-dep)
		       memory-dep #f comm-dep)
	    inter))
      (progstate+ a b r s t 
		  (stack data-sp data-body)
		  (stack return-sp return-body)
		  memory recv comm
		  extra)
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
