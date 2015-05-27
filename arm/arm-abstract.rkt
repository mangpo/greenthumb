#lang s-exp rosette

(require "../ast.rkt" "../validator.rkt" 
	 "arm-machine.rkt" "arm-simulator-rosette.rkt")

(define arm-abstract%
  (class object%
    (super-new)
    (define machine (new arm-machine%))
    (send machine set-config (list 5 0 4)) ;; TODO: memory
    (define simulator (new arm-simulator-rosette% [machine machine]))

    (define inst-id (get-field inst-id machine))
    (define fp (send machine get-fp))
    (define k 2)
    (define mask (sub1 (arithmetic-shift 1 k)))

    (define (is-possible? my-inst in-regs out-regs in-mask out-mask)
      ;(pretty-display `(is-possible? ,my-inst ,in-regs ,out-regs ,in-mask ,out-mask))
      (define start-state (send machine get-state sym-input #f)) ;; TODO: arm specific
      (define end-state (send simulator interpret (vector my-inst) start-state #:dep #f))
      
      (define start-regs (progstate-regs start-state))
      (define end-regs (progstate-regs end-state))

      (define (assert-constraints)
	(for ([r start-regs]
	      [m in-mask]
	      [v in-regs])
	     (when m (assert (= (bitwise-and r mask) v))))

	(for ([r end-regs]
	      [m out-mask]
	      [v out-regs])
	     (when m (assert (= (bitwise-and r mask) v)))))
      
      (with-handlers* 
       ([exn:fail? 
	 (lambda (e) 
	   (if (equal? (exn-message e) "solve: no satisfying execution found")
	       #f
	       (raise e)))])
       (solve (assert-constraints))))

    (define/public (abstract-behavior my-inst)
      (define in (make-vector 5 #f))
      (define out (make-vector 5 #f))
      (define yes 0)
      (define no 0)

      (define (recurse-regs in-list out-list in-res out-res)
	(cond
	 [(empty? out-list)
	  ;; TODO: z
	  (define ans
	    (is-possible? my-inst 
			  (list->vector (reverse in-res))
			  (list->vector (reverse out-res))
			  in out))
          (if ans (set! yes (add1 yes)) (set! no (add1 no)))
	  (pretty-display `(possible ,(reverse in-res) ,(reverse out-res) ,ans))
	  ]

	 [(empty? in-list)
	  (if (car out-list)
	      (for ([i (arithmetic-shift 1 k)]) ;; modulo with k = 6
		   (recurse-regs in-list (cdr out-list) in-res (cons i out-res)))
	      (recurse-regs in-list (cdr out-list) in-res (cons #f out-res)))]

	 [else
	  (if (car in-list)
	      (for ([i (arithmetic-shift 1 k)]) ;; modulo with k = 6
		   (recurse-regs (cdr in-list) out-list (cons i in-res) out-res))
	      (recurse-regs (cdr in-list) out-list (cons #f in-res) out-res))]))
	  

      ;; TODO: when z != -1
      (pretty-display `(my-inst ,(inst-op my-inst) ,(inst-args my-inst)))
      (define arg-types (send machine get-arg-types 
                              (vector-ref inst-id (inst-op my-inst))))
      (pretty-display `(arg-types ,arg-types))
      (for ([arg (inst-args my-inst)]
	    [type arg-types])
	   (cond
	    [(equal? type `reg-o) (vector-set! out arg #t)]
	    [(equal? type `reg-i) (vector-set! in arg #t)]
	    [(equal? type `reg-io) (vector-set! out arg #t) (vector-set! in arg #t)]))
      (pretty-display `(start ,(vector->list in) ,(vector->list out)))
      (recurse-regs (vector->list in) (vector->list out) (list) (list))
      (pretty-display `(stat ,yes ,no))
      )
    ))
      

(require "arm-printer.rkt" "arm-parser.rkt")
      
(define machine (new arm-machine%))
(send machine set-config (list 5 0 4))
(define printer (new arm-printer% [machine machine]))
(define parser (new arm-parser%))
(define abst (new arm-abstract%))
(define my-inst 
  (vector-ref (send printer encode (send parser ast-from-string "add r3, r0, r1, asr 1"))
	      0))
(send abst abstract-behavior my-inst)
