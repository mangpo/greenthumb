#lang racket

(require "../ast.rkt" "../enumerative.rkt" "../machine.rkt" "../ops-racket.rkt"
	 "arm-ast.rkt" "arm-machine.rkt" "arm-simulator-racket.rkt" "arm-validator.rkt")
(require racket/generator)

(provide arm-enumerative%)

(define arm-enumerative%
  (class enumerative%
    (super-new)
    (inherit-field machine printer simulator validator generate-inst)
    (override len-limit window-size reset-generate-inst 
	      get-register-mapping get-renaming-iterator
	      abstract)

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
      ;; (define inst-choice '(add and#))
      ;; (define inst-pool (map (lambda (x) (vector-member x inst-id)) inst-choice))
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
			    [arg-ranges 
                             (if regs
                                 (vector->list 
                                  (send machine get-arg-ranges-enum opcode-name #f live-in))
                                 (vector->list
                                  (send machine get-arg-ranges opcode-name #f live-in)))]
			    [cond-bound (if (= z -1) 1  cond-type-len)])
		       ;;(pretty-display "here2")
		       (when debug (pretty-display `(iterate ,shf? ,arg-ranges ,cond-bound)))
		       (for ([cond-type cond-bound])
			    (if shf?
				(begin
				  (recurse-args opcode-id 0 #f cond-type (list) 
                                                arg-ranges regs)
				  (for* ([shfop (range 1 shf-inst-len)]
					 [shfarg (send machine get-shfarg-range shfop live-in)])
					;;(pretty-display "here3")
					(recurse-args opcode-id shfop shfarg cond-type (list) 
						      arg-ranges regs)
					(when debug
					      (pretty-display `(call-recurse ,opcode-name 
                                                                             ,opcode-id 
									     ,shfop 
                                                                             ,shfarg 
									     ,cond-type 
									     ,arg-ranges)))
					))
				(recurse-args opcode-id 0 #f cond-type (list) 
                                              arg-ranges regs)))))))
	     (yield (list #f #f #f)))))

    (define (get-register-mapping org-nregs states-vec-spec states-vec liveout-vec)
      ;;(pretty-display `(get-register-mapping ,states-vec-spec ,states-vec ,liveout-vec))
      (define all-nregs (send machine get-nregs))
      (define regs-spec (map (lambda (x) (vector-ref x 0)) states-vec-spec)) ;; list
      (define regs (map (lambda (x) (vector-ref x 0)) states-vec))
      (define liveout (vector-ref liveout-vec 0))
      (define mapping (make-vector org-nregs #f))
      (define break #f)

      (for ([i org-nregs] #:break break)
      	   (when 
      	    (vector-ref liveout i)
      	    (vector-set! mapping i (list))
      	    (let ([regs-spec-i (map (lambda (x) (vector-ref x i)) regs-spec)])
      	      (for ([j all-nregs])
      		   (let* ([regs-j (map (lambda (x) (vector-ref x j)) regs)]
      			  [compare
      			   (for/and ([spec regs-spec-i]
      			   	     [my regs-j])
      			   	    (= spec my))])
      		     (when compare
      		     	   (vector-set! mapping i (cons j (vector-ref mapping i)))))))
      	    (when (empty? (vector-ref mapping i))
      		  (set! break #t))))
      
      ;;(pretty-display `(get-register-mapping ,(and (not break) mapping)))
      (and (not break) mapping))
    
    (define (check-later-use prog org index len)
      (define (inner x)
	(define opcode (inst-op x))
	(define opcode-name (send machine get-inst-name opcode))
	(define args (inst-args x))
	(define class-id (send machine get-class-id opcode-name))
        
	(define-syntax-rule (check x ...)
	  (check-main (list x ...)))
        
        (define (check-main fs)
	  (for/and ([f fs] 
                    [arg args])
		   ;;(pretty-display `(check-main ,f ,arg ,reg))
            (or (not f) (not (= arg org)))))
        
        (define reg #t) (define imm #f) (define id #f)
        
        (cond
	 [(equal? class-id 0) (check reg reg reg)]
	 [(equal? class-id 1) (check reg reg imm)]
	 ;;[(equal? class-id 2) (check #f reg imm)]
	 [(equal? class-id 2) (check reg reg)]
	 [(equal? class-id 3) (check reg imm)]
	 [(equal? class-id 4) (check reg reg reg reg)]
	 [(equal? class-id 5) (check reg reg imm imm)]
	 [(equal? class-id 6) (check reg id imm)]
	 [(member opcode-name '(bfc)) (check reg imm imm)]
	 [(equal? opcode-name `nop) #t]
	 [else (raise (format "rename: undefined for ~a" opcode-name))]))
      
      (for/and ([i (range (add1 index) len)])
        (inner (vector-ref prog i))))


    (define (rename prog assigned)
      ;(pretty-display `(rename ,assigned))
      (define len (vector-length prog))
      (define valid #t)
      (define table (make-vector (send machine get-nregs) #f))
      (for ([x assigned]
	    [i (length assigned)])
	   (when x (vector-set! table x i)))

      (define (rename-inst x index)
	(define opcode (inst-op x))
	(define opcode-name (send machine get-inst-name opcode))
	(define class-id (send machine get-class-id opcode-name))
	(define args (inst-args x))
	(define cond-type (inst-cond x))
	(define shfop (inst-shfop x))
	(define shfarg (inst-shfarg x))

	(define-syntax-rule (make-inst x ...)
	  (make-inst-main (list x ...)))
	
	(define (make-inst-main fs)
	  (define new-args 
	    (for/vector ([f fs] 
			 [arg args])
			(if f 
			    (let ([to (vector-ref table arg)])
			      (if to 
                                  (let ([res (or (= arg to) (check-later-use prog to index len))])
				    ;;(pretty-display `(make-inst-main ,res ,arg, to))
                                    (set! valid (and valid res))
                                    to)
                                  arg))
			    arg)))
	  
	  (arm-inst opcode new-args shfop shfarg cond-type))

	(define reg #t) (define imm #f) (define id #f)

	(cond
	 [(equal? class-id 0) (make-inst reg reg reg)]
	 [(equal? class-id 1) (make-inst reg reg imm)]
	 ;;[(equal? class-id 2) (make-inst reg reg imm)]
	 [(equal? class-id 2) (make-inst reg reg)]
	 [(equal? class-id 3) (make-inst reg imm)]
	 [(equal? class-id 4) (make-inst reg reg reg reg)]
	 [(equal? class-id 5) (make-inst reg reg imm imm)]
	 [(equal? class-id 6) (make-inst reg id imm)]
	 [(member opcode-name '(bfc)) (make-inst reg imm imm)]
	 [(equal? opcode-name `nop) inst]
	 [else (raise (format "rename: undefined for ~a" opcode-name))]))

      (let ([new-x
             (for/vector ([x prog]
                          [i (vector-length prog)]
                          #:break (not valid))
			 ;;(pretty-display `(rename-inst ,i))
               (rename-inst x i))])
        ;; (unless valid
        ;;   (pretty-display "Rename: invalid")
        ;;   (send printer print-syntax (send printer decode prog))
        ;;   )
        (and valid new-x))
      )

    (define (get-renaming-iterator prog mapping)
      ;(pretty-display `(get-renaming-iterator ,mapping))
      (generator
       ()
       (define (recurse index assigned mapping)
	 (cond
	  [(empty? mapping)
           (define renamed (rename prog (reverse assigned)))
           (when renamed (yield renamed))]
	  [(car mapping)
	   (for ([choice (car mapping)])
		(recurse (add1 index) (cons choice assigned) (cdr mapping)))]

	  [else (recurse (add1 index) (cons #f assigned) (cdr mapping))]
	  ))
       (recurse 0 (list) (vector->list mapping))
       (yield #f)))

    ;; TODO: memory, z
    (define (abstract state-vec live-list k)
      (define regs (vector-ref state-vec 0))
      (define mems (vector-ref state-vec 1))
      (define z (vector-ref state-vec 2))
      (define fp (vector-ref state-vec 3))
      (vector
       (for/vector ([r regs] [i (vector-length regs)])
		   (and (member i live-list) (modulo r k)))
       (make-vector (vector-length mems) #f)
       -1 fp))

    ))
