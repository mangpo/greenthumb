#lang s-exp rosette

(require "../ast.rkt" "../validator.rkt" "arm-ast.rkt"
	 "arm-machine.rkt" "arm-simulator-rosette.rkt")

(require "arm-printer.rkt" "arm-parser.rkt")

(require rosette/solver/smt/z3)
(require rosette/solver/kodkod/kodkod)

(provide arm-abstract%)

(define arm-abstract%
  (class object%
    (super-new)
    (init-field k [all-yes 0] [all-no 0])
    (define machine (new arm-machine%))
    (send machine set-config (list 5 0 4)) ;; TODO: memory
    (define simulator (new arm-simulator-rosette% [machine machine]))

    (define inst-id (get-field inst-id machine))
    (define shf-inst-id (get-field shf-inst-id machine))
    (define fp (send machine get-fp))
    (define mask (sub1 (arithmetic-shift 1 k)))

    (current-bitwidth (get-field bit machine))

    (define (is-possible? my-inst in-regs out-regs in-mask out-mask start-regs end-regs)
      ;(pretty-display `(is-possible? ,my-inst ,in-regs ,out-regs ,in-mask ,out-mask))

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
      (define opcode-name (vector-ref inst-id (inst-op my-inst)))
      (define in (make-vector 5 #f))
      (define out (make-vector 5 #f))
      (define yes 0)
      (define no 0)

      (define mapping (make-hash))
      (define holder #f)
      (define all #t)

      ;; (define start-state (send machine get-state sym-input #f)) ;; TODO: arm specific
      ;; (define end-state (send simulator interpret (vector my-inst) start-state #:dep #f))
      ;; (define start-regs (progstate-regs start-state))
      ;; (define end-regs (progstate-regs end-state))
      (define start-state #f)
      (define end-state #f)
      (define start-regs #f)
      (define end-regs #f)

      (define (reset)
        (when
         (= (modulo (+ yes no) 1000) 0)
         (with-output-to-file "progress.log" #:exists 'append
           (thunk (pretty-display 
                   (format "stat, ~a, ~a, ~a, ~a" 
                           (+ yes no) yes no (quotient (current-memory-use) 1000000))))))
         
        (when
         (= (modulo (+ yes no) 10000) 0)
         (unsafe-clear-terms!)
         (if (member opcode-name '(smull umull smmul smmla smmls))
             (current-solver (new z3%))
             (current-solver (new kodkod%)))
         (set! start-state (send machine get-state sym-input #f))
         (set! end-state (send simulator interpret (vector my-inst) start-state #:dep #f))
         (set! start-regs (progstate-regs start-state))
         (set! end-regs (progstate-regs end-state))))

        (define (recurse-regs in-list out-list in-res out-res)
	(cond
	 [(empty? out-list)
          (reset)
	  ;; TODO: z
	  (define ans
	    (is-possible? my-inst 
			  (list->vector (reverse in-res))
			  (list->vector (reverse out-res))
			  in out start-regs end-regs))
          ;; Put into holder
          (if ans 
              (begin
                (set! yes (add1 yes)) 
                (set! holder (cons (filter number? out-res) holder))
                )
              (begin
                (set! no (add1 no))
                (set! all #f)
                )
              )
	  (pretty-display `(possible ,(reverse in-res) ,(reverse out-res) ,ans))
	  ]

	 [(and (empty? in-list) (empty? out-res))
          (set! holder (list))
	  (if (car out-list)
	      (for ([i (arithmetic-shift 1 k)]) ;; modulo with k = 6
		   (recurse-regs in-list (cdr out-list) in-res (cons i out-res)))
	      (recurse-regs in-list (cdr out-list) in-res (cons #f out-res)))
          ;; Put holder into mapping
          (if all
              (hash-set! mapping (filter number? in-res) #t)
              (hash-set! mapping (filter number? in-res) holder))
          ]

	 [(empty? in-list)
	  (if (car out-list)
	      (for ([i (arithmetic-shift 1 k)]) ;; modulo with k = 6
		   (recurse-regs in-list (cdr out-list) in-res (cons i out-res)))
	      (recurse-regs in-list (cdr out-list) in-res (cons #f out-res)))
          ]

	 [else
	  (if (car in-list)
	      (for ([i (arithmetic-shift 1 k)]) ;; modulo with k = 6
		   (recurse-regs (cdr in-list) out-list (cons i in-res) out-res))
	      (recurse-regs (cdr in-list) out-list (cons #f in-res) out-res))]))
	  

      ;; TODO: when z != -1
      ;(pretty-display `(my-inst ,(inst-op my-inst) ,(inst-args my-inst)))
      (define arg-types (send machine get-arg-types opcode-name))
      (define shfop (inst-shfop my-inst))
      (when (and shfop (member (vector-ref shf-inst-id shfop) '(asr lsl lsr)))
            (vector-set! in 0 #t))


      ;(pretty-display `(arg-types ,arg-types))
      (for ([arg (inst-args my-inst)]
	    [type arg-types])
	   (cond
	    [(equal? type `reg-o) (vector-set! out arg #t)]
	    [(equal? type `reg-i) (vector-set! in arg #t)]
	    [(equal? type `reg-io) (vector-set! out arg #t) (vector-set! in arg #t)]))
      ;(pretty-display `(start ,(vector->list in) ,(vector->list out)))
      (recurse-regs (vector->list in) (vector->list out) (list) (list))
      (pretty-display `(stat ,yes ,no))
      (set! all-yes (+ all-yes yes))
      (set! all-no (+ all-no no))

      (unsafe-clear-terms!)
      mapping
      )

    (define/public (test)
      (define printer (new arm-printer% [machine machine]))
      (define parser (new arm-parser%))
      (define my-inst 
        (vector-ref (send printer encode 
                          (send parser ast-from-string "add r3, r1, r2, asr r0"))
                    0))
      ;;(send abst abstract-behavior my-inst)
      (define input-state (progstate (vector 7 10 13 #f #f)
                                     (vector) -1 4))
      (define output-state (progstate (vector #f #f #f 13 #f)
                                      (vector) -1 4))
      (define start-state (send machine get-state sym-input #f)) ;; TODO: arm specific
      (define end-state (send simulator interpret (vector my-inst) start-state #:dep #f))
      
      (define start-regs (progstate-regs start-state))
      (define end-regs (progstate-regs end-state))
      (solve (begin 
               (assert (= (bitwise-and (vector-ref start-regs 0) mask) 7))
               (assert (= (bitwise-and (vector-ref start-regs 1) mask) 10))
               (assert (= (bitwise-and (vector-ref start-regs 2) mask) 13))
               (assert (= (bitwise-and (vector-ref end-regs 3) mask) 14)))))

      
    ))
      

;; (define machine (new arm-machine%))
;; (define abst (new arm-abstract% [k 4]))
;; (define printer (new arm-printer% [machine machine]))
;; (define parser (new arm-parser%))
;; (define my-inst 
;;   (vector-ref (send printer encode 
;;                     (send parser ast-from-string "add r3, r1, r2, asr r0"))
;;               0))
;; (send abst abstract-behavior my-inst)
;; (send abst test)
