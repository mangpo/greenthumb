#lang racket

(require "../ops-racket.rkt" "../inst.rkt" "../inverse.rkt")

(provide llvm-demo-inverse%)

(define llvm-demo-inverse%
  (class inverse%
    (super-new)
    (init-field machine simulator)
    (override gen-inverse-behavior interpret-inst)

    ;; Reduced-bit
    (define bit (get-field bit machine))
    (define inst-id (get-field inst-id machine))
    (define val-range
      (for/vector ([v (arithmetic-shift 1 bit)]) (finitize v bit)))

    (define (get-inst-in-out x)
      (define opcode (inst-op x))
      (define args (inst-args x))
      (define inst-type (list opcode))

      (define in (list))
      (define out #f)
      
      (for ([arg args]
	    [type (send machine get-arg-types (vector-ref inst-id opcode))])
	   (cond
	    [(equal? type `var-o) (set! out arg)]
	    [(equal? type `var-i) (set! in (cons arg in))]
	    [else (set! inst-type (cons arg inst-type))]))

      (values (reverse inst-type) (reverse in) out))


    ;; Inverse tables for all instructions.
    (define behaviors-bw (make-hash))

    ;; Generate inverse table behavior for my-inst.
    (define (gen-inverse-behavior my-inst)
      (define opcode (inst-op my-inst))
      (define opcode-name (vector-ref inst-id opcode))
      (define args (inst-args my-inst))
      ;; For collecting which registers in my-inst are input and output.
      (define in (make-vector 5 #f))
      (define out-reg (vector-ref args 0))
      ;; Inverse table behavior
      (define behavior-bw (make-hash))
      
      (define (recurse-regs in-list in-res)
	(cond
	 [(empty? in-list)
          (define out-state
	    (with-handlers*
	     ([exn? (lambda (e) #f)])
	     (send simulator interpret
		   (vector my-inst)
		   (list->vector in-res))))
          
	  (when 
	   out-state
	   (define in-list-filtered (filter number? in-res))
	   (define key (vector-ref out-state out-reg))

           ;; Insert into the inverse table. 
           ;; Key is outputs. Value is a set of possible inputs.
	   (if (hash-has-key? behavior-bw key)
	       (hash-set! behavior-bw key
			  (cons in-list-filtered (hash-ref behavior-bw key)))
	       (hash-set! behavior-bw key (list in-list-filtered))))
	  ]

         [else
          (if (car in-list)
              ;; Enumerate all possible values for a register if the register is input.
	      (for ([i val-range])
		   (recurse-regs (cdr in-list) (cons i in-res)))
	      (recurse-regs (cdr in-list) (cons #f in-res)))]))

          
      ;; Collect information on which registers are input and output.
      (for ([arg args]
	    [type (send machine get-arg-types opcode-name)])
	   (when (equal? type `var-i) (vector-set! in arg #t)))

      (recurse-regs (reverse (vector->list in)) (list))
      
      (define-values (x vars-in var-out) (get-inst-in-out my-inst))
      ;;(pretty-display `(behavior-bw ,behavior-bw))
      (hash-set! behaviors-bw x behavior-bw))

    
    ;; Inverse interpret my-inst using the pre-computed 'behaviors-bw'.
    ;; my-inst: instruction
    ;; state-vec: progstate in vector/list/pair format
    ;; old-liveout: liveout info
    ;; output: a list of progstates in vector/list/pair format
    (define (interpret-inst my-inst state old-liveout)
      ;;(pretty-display `(interpret ,state ,old-liveout))
      (define opcode-name (vector-ref inst-id (inst-op my-inst)))
      (define n (vector-length state))

      (define-values (x vars-in var-out) (get-inst-in-out my-inst))
      (define state-base (make-vector n #f))
      (for ([i n]
	    [l old-liveout])
	   (when (and l (not (= i var-out)))
		 (vector-set! state-base i (vector-ref state i))))
      (define var-out-val (vector-ref state var-out))

      (define mapping (hash-ref behaviors-bw x))
      (define ret (and (hash-has-key? mapping var-out-val) (list)))

      (when ret
	    (define vars-in-val-list (hash-ref mapping var-out-val))
	    ;;(pretty-display `(ref ,var-out-val ,vars-in-val-list))
	    (for ([vars-in-val vars-in-val-list])
		 (let ([new-state (vector-copy state-base)]
		       [pass #t])
		   (for ([r vars-in]
			 [v vars-in-val] #:break (not pass))
			(cond
			 [(vector-ref new-state r)
			  (unless (= v (vector-ref new-state r))
				  
				  (set! pass #f))]
			 [else (vector-set! new-state r v)]))
		   (when pass (set! ret (cons new-state ret))))))
      
      ret)

    ))
