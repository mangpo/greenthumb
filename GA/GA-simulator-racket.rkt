#lang racket

(require "../simulator.rkt" "../ops-racket.rkt"  "GA-ops-racket.rkt"
         "../inst.rkt"
         "../machine.rkt" "GA-machine.rkt")
(provide GA-simulator-racket%)

(define GA-simulator-racket%
  (class simulator%
    (super-new)
    (init-field machine)
    (override interpret performance-cost get-constructor)

    (define (get-constructor) GA-simulator-racket%)

    (define bit (get-field bitwidth machine))
    (define opcodes (get-field opcodes machine))

    (define debug #f)
    
    (define UP (get-field UP machine))
    (define DOWN (get-field DOWN machine))
    (define LEFT (get-field LEFT machine))
    (define RIGHT (get-field RIGHT machine))
    (define IO (get-field IO machine))

    ;; Interpret a given program from a given state.
    ;; code
    ;; state: initial progstate
    ;; policy: a procedure that enforces a communication policy (see the definition of comm-policy above)
    (define (interpret code state [ref #f])
      ;; (set! policy (if policy
      ;;   	       (comm-policy at-most policy)
      ;;   	       (comm-policy all)))
      
      (define a (progstate-a state))
      (define b (progstate-b state))
      (define r (progstate-r state))
      (define s (progstate-s state))
      (define t (progstate-t state))
      (define data-sp (stack-sp (progstate-data state)))
      (define data-body (vector-copy (stack-body (progstate-data state))))
      (define return-sp (stack-sp (progstate-return state)))
      (define return-body (vector-copy (stack-body (progstate-return state))))
      (define memory #f)
      (define recv #f)
      (define comm #f)
      
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
      
      ;; Read from the given memory address or communication port. If it
      ;; gets a communication port, it just returns a random number (for
      ;; now).
      (define (read-memory addr)
        (define ret #f)
	(define (read port)
          (unless comm
                  (set! comm (send (progstate-comm state) clone (and ref (progstate-comm ref))))
                  (set! recv (send (progstate-recv state) clone (and ref (progstate-recv ref)))))
          (let ([val (send recv pop)])
            (send comm push (list val port 0))
            val))
	(cond
	 [(equal? addr UP)    (set! ret (read UP))]
	 [(equal? addr DOWN)  (set! ret (read DOWN))]
	 [(equal? addr LEFT)  (set! ret (read LEFT))]
	 [(equal? addr RIGHT) (set! ret (read RIGHT))]
	 [(equal? addr IO)    (set! ret (read IO))]
	 [else
          ;;(pretty-display `(memory-load ,addr))
          (unless memory
                  (set! memory (send (progstate-memory state) clone (and ref (progstate-memory ref)))))
          (assert (and (>= addr 0) (< addr (min 64 (sub1 (arithmetic-shift 1 (sub1 bit)))))))
          (set! ret (send memory load addr))])
        ret)
      
      ;; Write to the given memeory address or communication
      ;; port. Everything written to any communication port is simply
      ;; aggregated into a list.
      (define (set-memory! addr val)
	(when debug (pretty-display `(set-memory! ,addr ,val)))
	(define (write port)
          (unless comm
                  (set! comm (send (progstate-comm state) clone (and ref (progstate-comm ref))))
                  (set! recv (send (progstate-recv state) clone (and ref (progstate-recv ref)))))
          (send comm push (list val port 1)))
	(cond
	 [(equal? addr UP)    (write UP)]
	 [(equal? addr DOWN)  (write DOWN)]
	 [(equal? addr LEFT)  (write LEFT)]
	 [(equal? addr RIGHT) (write RIGHT)]
	 [(equal? addr IO)    (write IO)]
	 [else
          (unless memory
                  (set! memory (send (progstate-memory state) clone (and ref (progstate-memory ref)))))
          (assert (and (>= addr 0) (< addr (min 64 (sub1 (arithmetic-shift 1 (sub1 bit)))))))
          (send memory store addr val)]))

      (define (clip x) (finitize x bit))

      (define (push-right-one x carry)
	(clip (bitwise-ior (<< (bitwise-and #x1 carry) (sub1 bit) bit) (>>> x 1 bit))))
      
      ;; Treats T:A as a single 36 bit register and shifts it right by one
      ;; bit. The most signficicant bit (T17) is kept the same.
      (define (multiply-step-even!)
	(let ([a-val (push-right-one a t)]
	      [t-val (>> t 1)])
	  (set! a a-val)
	  (set! t t-val)))
      
      ;; Sums T and S and concatenates the result with A, shifting
      ;; the concatenated 37-bit to the right by one bit.
      (define (multiply-step-odd!)
	(let* ([sum (+ t s)]
	       [a-val (push-right-one a sum)]
	       [t-val (>> sum 1)])
	  (set! a a-val)
	  (set! t t-val)))

      (define-syntax-rule (mem-to-stack addr)
	(push! (read-memory addr)))

      (define-syntax-rule (stack-to-mem addr)
        (set-memory! addr (pop!)))

      (define-syntax-rule (stack-1 f)
        (set! t (f t)))

      (define-syntax-rule (stack-2 f)
	(let* ([val1 (pop!)]
               [val2 (pop!)])
          (push! (f val1 val2))))

      (define (interpret-step inst-const)
	(define inst (inst-op inst-const))
        (define args (inst-args inst-const))
	(define const (and args (> (vector-length args) 0) (vector-ref args 0)))
	(when debug (pretty-display `(interpret-step ,inst ,const)))
	(define-syntax-rule (inst-eq x) (equal? x (vector-ref opcodes inst)))
	(cond
	 [(inst-eq `@p)   (push! const)]
	 [(inst-eq `@+)   (mem-to-stack a)
	                  (set! a (clip (add1 a)))]
	 [(inst-eq `@b)   (mem-to-stack b)]
	 [(inst-eq `@)    (mem-to-stack a)]
	 [(inst-eq `!+)   (stack-to-mem a)
	                  (set! a (clip (add1 a)))]
	 [(inst-eq `!b)   (stack-to-mem b)]
	 [(inst-eq `!)    (stack-to-mem a)]
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
	 
	 ;; [(block? x)
	 ;;  (interpret-struct (block-body x))]

	 ;; [(forloop? x)
	 ;;  (interpret-struct (forloop-init x))
	 ;;  (r-push! (pop!))
	 ;;  (for ([i (in-range (add1 r))])
	 ;;       (interpret-struct (forloop-body x))
	 ;;       (set! r (sub1 r)))
	 ;;  (r-pop!)
	 ;;  ]

	 ;; [(ift? x)
	 ;;  (when (not (equal? t 0))
	 ;;        (interpret-struct (ift-t x)))]

	 ;; [(iftf? x)
	 ;;  (if (not (equal? t 0))
	 ;;      (interpret-struct (iftf-t x))
	 ;;      (interpret-struct (iftf-f x)))]

	 ;; [(-ift? x)
	 ;;  (when (negative? t) ;(or (negative? t) (>= t (arithmetic-shift 1 (sub1 bit)))) ;; negative
	 ;;        (interpret-struct (-ift-t x)))]

	 ;; [(-iftf? x)
	 ;;  (if (negative? t) ;(or (negative? t) (>= t (arithmetic-shift 1 (sub1 bit)))) ;; negative
	 ;;      (interpret-struct (-iftf-t x))
	 ;;      (interpret-struct (-iftf-f x)))]

	 [else (raise (format "interpret-struct: unimplemented for ~a" x))]
	 ))
      
      (interpret-struct code)

      (progstate a b r s t 
		  (stack data-sp data-body)
		  (stack return-sp return-body)
		  (or memory (progstate-memory state))
                  (or recv (progstate-recv state))
                  (or comm (progstate-comm state))
                  )
      )

    (define (performance-cost code)

      (define (cost-step inst-const)
	(define inst (inst-op inst-const))
	(define-syntax-rule (inst-eq x) (equal? x (vector-ref opcodes inst)))
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
	 ;; [(block? x)   (cost-struct (block-body x))]
	 ;; [(forloop? x) (+ (cost-struct (forloop-init x)) (cost-struct (forloop-body x)))]
	 ;; [(ift? x)     (cost-struct (ift-t x))]
	 ;; [(iftf? x)    (+ (cost-struct (iftf-t x)) (cost-struct (iftf-f x)))]
	 ;; [(-ift? x)    (cost-struct (-ift-t x))]
	 ;; [(-iftf? x)   (+ (cost-struct (-iftf-t x)) (cost-struct (-iftf-f x)))]
	 [else (raise (format "cost-struct: unimplemented for ~a" x))]
	 ))

      (cost-struct code))

    ))
