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

    (define/public (gen-abstract-behavior my-inst)
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
	 (send (current-solver) shutdown)
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

      (save-to-file my-inst mapping)
      )

    (define behavior (make-hash))

    (define/public (load-abstract-behavior)
      (define i (open-input-file "abstract.csv"))
      (define current-mapping (make-hash))
      (define key-str #f)
      
      (define (loop line)
	(when 
	 (string? line)
	 (define tokens (string-split line ","))
	 (define prog-str (first tokens))
	 (define in (map string->number (string-split (second tokens) " ")))
	 (define out-list 
	   (if (equal? (third tokens) "#t")
	       #t
	       (for/list ([out-str (string-split (third tokens) ";")])
			 (map string->number (string-split out-str " ")))))

	 (unless (equal? prog-str key-str)
		 (when key-str
		       (hash-set! behavior (map string->number (string-split key-str " "))
				  current-mapping))
		 (set! key-str prog-str)
		 (set! current-mapping (make-hash)))
	 (hash-set! current-mapping in out-list)
	 (loop (read-line i))))
      
      (loop (read-line i))
      (close-input-port i)
      (hash-set! behavior (string-split key-str " ") current-mapping)
      (pretty-display `(summary ,(hash-count behavior)))
      )
	     

    (define (save-to-file my-inst res)
      (define-values (x in out) (get-inst-in-out my-inst))
      (define x-str (string-join (map number->string x) " "))
      (with-output-to-file "abstract.csv" #:exists 'append
        (thunk
	 (for ([pair (hash->list res)])
	      (let* ([in (car pair)]
		     [in-str (string-join (map number->string in) " ")]
		     [out-list (cdr pair)])
		(display (format "~a,~a," x-str in-str))
		(if (list? out-list)
		    (let ([out-str-list
			   (map (lambda (x) (string-join (map number->string x) " ")) 
				out-list)])
		      (pretty-display (string-join out-str-list ";")))
		    (pretty-display out-list))))
	 )))

    (define (get-inst-in-out x)
      (define opcode (inst-op x))
      (define args (inst-args x))
      (define shfop (inst-shfop x))
      (define shfarg (inst-shfarg x))
      (unless shfop (set! shfop 0))

      (define inst-type (list shfop opcode))
      (define in (list))
      (define out (list))
      (when (> shfop 0)
	    (if (member (vector-ref shf-inst-id shfop) '(asr lsl lsr))
		(set! in (cons shfarg in))
		(set! inst-type (cons shfarg inst-type))))
      
      (for ([arg args]
	    [type (send machine get-arg-types (vector-ref inst-id opcode))])
	   (cond
	    [(equal? type `reg-o) (set! out (cons arg out))]
	    [(equal? type `reg-i) (set! in (cons arg in))]
	    [(equal? type `reg-io) (set! in (cons arg in)) (set! out (cons arg out))]
	    [else (set! inst-type (cons arg inst-type))]))

      (values (reverse inst-type) (reverse in) (reverse out)))



    (define/public (interpret-inst my-inst state)
      ;; TODO z
      (define opcode-name (vector-ref inst-id (inst-op my-inst)))
      (define cond-type (arm-inst-cond my-inst))

      (define regs (progstate-regs state))
      (define z (progstate-z state))
      (define fp (progstate-fp state))
      (define (exec)
	(define-values (x regs-in regs-out) (get-inst-in-out my-inst))
	(define regs-in-val (for/list ([r regs-in]) (vector-ref regs r)))
	(define mapping (hash-ref behavior x))
	(define regs-out-val-list (hash-ref mapping regs-in-val))
	(if (equal? regs-out-val-list #t)
	    #t
	    (for/list ([regs-out-val regs-out-val-list])
		      (let ([new-regs (vector-copy (progstate-regs state))]
			    [new-memory (vector-copy (progstate-memory state))])
			(for ([r regs-out]
			      [v regs-out-val])
			     (vector-set! new-regs r v))
			(progstate new-regs new-memory z fp)))))

      (define (same)
	(list
	 (progstate (vector-copy (progstate-regs state))
		    (vector-copy (progstate-memory state))
		    z fp)))

        (cond
         [(or (equal? z -1) (equal? cond-type 0) 
	      (member opcode-name '(tst cmp tst# cmp#)))
          (exec)]

	 [(equal? cond-type 1) ;; eq
	  (if (equal? z 0) (exec) (same))]

	 [(equal? cond-type 2) ;; ne
	  (if (member z (list 1 2 3)) (exec) (same))]

	 [(equal? cond-type 3) ;; ls
	  (if (member z (list 0 2)) (exec) (same))]

	 [(equal? cond-type 4) ;; hi
	  (if (equal? z 3) (exec) (same))]

	 [(equal? cond-type 5) ;; cc
	  (if (equal? z 2) (exec) (same))]

	 [(equal? cond-type 6) ;; cs
	  (if (member z (list 0 3)) (exec) (same))]
	 
         [else (raise (format "illegal cond-type ~a" cond-type))]
         ))

    (define/public (test)
      (current-solver (new z3%))
      (define printer (new arm-printer% [machine machine]))
      (define parser (new arm-parser%))
      (define my-inst 
        (vector-ref (send printer encode 
                          (send parser ast-from-string "smull r3, r4, r0, r1"))
                    0))
      ;;(send abst abstract-behavior my-inst)
      (define start-state (send machine get-state sym-input #f)) ;; TODO: arm specific
      (define end-state (send simulator interpret (vector my-inst) start-state #:dep #f))
      
      (define start-regs (progstate-regs start-state))
      (define end-regs (progstate-regs end-state))
      (solve (begin 
               (assert (= (bitwise-and (vector-ref start-regs 0) mask) 7))
               (assert (= (bitwise-and (vector-ref start-regs 1) mask) 8))
               (assert (= (bitwise-and (vector-ref end-regs 3) mask) 2))
               (assert (= (bitwise-and (vector-ref end-regs 4) mask) 2)))))

      
    ))
      

(define abst (new arm-abstract% [k 4]))
(define machine (new arm-machine%))
(send machine set-config (list  5 0 4))
(define printer (new arm-printer% [machine machine]))
(define parser (new arm-parser%))
(define my-inst 
  (vector-ref (send printer encode 
                    (send parser ast-from-string "ubfx r1, r0, 0, 1"))
              0))

;; (send abst gen-abstract-behavior my-inst)
;; (send abst test)

(define t0 (current-seconds))
(send abst load-abstract-behavior)
(define t1 (current-seconds))
(pretty-display `(time ,(- t1 t0)))
(define input-state (progstate (vector 3 0 0 0 0)
                               (vector) -1 4))
(define output-states
  (send abst interpret-inst my-inst input-state))
(pretty-display output-states)
(when (list? output-states)
      (for ([output-state output-states])
	   (send machine display-state output-state)))
