#lang racket

(require "../enumerative.rkt" "../machine.rkt" 
	 "arm-ast.rkt" "arm-machine.rkt" "arm-simulator-racket.rkt" "arm-validator.rkt")
(require racket/generator)

(provide arm-enumerative%)

(define arm-enumerative%
  (class enumerative%
    (super-new)
    (inherit-field machine printer simulator validator generate-inst)
    (override len-limit window-size reset-generate-inst)

    (define (len-limit) 2)
    (define (window-size) 4)
    (set! simulator (new arm-simulator-racket% [machine machine]))
    (set! validator (new arm-validator% [machine machine] [printer printer]))

    (define inst-id (get-field inst-id machine))
    (define inst-with-shf (get-field inst-with-shf machine))
    (define cond-type-len (vector-length (get-field cond-inst-id machine)))
    (define shf-inst-len (vector-length (get-field shf-inst-id machine)))
                     

    (define (reset-generate-inst states live-in regs)
      (define z (progstate-z (car states))) ;; enough to look at one state.
      (define inst-pool (get-field inst-pool machine))
      (set! generate-inst 
	    (generator 
	     ()
	     (when debug (pretty-display `(reset-generate-inst ,inst-pool)))
	     (define (recurse-args opcode-id shfop shfarg cond-type args ranges v-reg)
	       (when debug (pretty-display `(recurse-args ,args ,ranges)))
	       (cond
		[(empty? ranges)
		 (let ([i (arm-inst opcode-id 
				    (list->vector (reverse args)) 
				    shfop shfarg cond-type)])
		   (yield (list i (send machine update-live live-in i) v-reg))
		   )]

		[(equal? (car ranges) #f) ;; SSA (virtual register)
		 (recurse-args opcode-id shfop shfarg cond-type 
			       (cons v-reg args)
			       (cdr ranges) (add1 v-reg))]

		[else
		 (for ([arg (car ranges)])
		      (recurse-args opcode-id shfop shfarg cond-type 
				    (cons arg args)
				    (cdr ranges) v-reg))
                 ]))
	     (for ([opcode-id inst-pool])
		  ;;(pretty-display "here1")
		  (let ([opcode-name (vector-ref inst-id opcode-id)])
		    (unless 
		     (equal? opcode-name `nop)
		     (when debug 
			   (pretty-display `(opcode-name ,opcode-name
							 ,(send machine get-arg-ranges-enum 
								opcode-name #f live-in))))
		     (let* ([shf? (member opcode-name inst-with-shf)]
			    [arg-ranges (vector->list 
					 (send machine get-arg-ranges-enum opcode-name #f live-in))]
			    [cond-bound (if (= z -1) 1  cond-type-len)])
		       ;;(pretty-display "here2")
		       (when debug (pretty-display `(iterate ,shf? ,arg-ranges ,cond-bound)))
		       (for ([cond-type cond-bound])
			    (if shf?
				(begin
				  (recurse-args opcode-id 0 #f cond-type (list) arg-ranges regs)
				  (for* ([shfop (range 1 shf-inst-len)]
					 [shfarg (send machine get-shfarg-range shfop live-in)])
					;;(pretty-display "here3")
					(recurse-args opcode-id shfop shfarg cond-type (list) 
						      arg-ranges regs)
					(when debug
					      (pretty-display `(call-recurse ,opcode-name ,opcode-id 
									     ,shfop ,shfarg 
									     ,cond-type 
									     ,arg-ranges)))
					))
				(recurse-args opcode-id 0 #f cond-type (list) arg-ranges regs)))))))
	     (yield (list #f #f #f)))))

    
    ))
