#lang s-exp rosette

(require 
 ;; ISA independent
 "ast.rkt" ;; "liveness.rkt"
 ;; ISA dependent
 "neon/interpret.rkt" "neon/machine.rkt" "neon/print.rkt" ;; "GA/compress.rkt"
 "neon/solver-support.rkt"
 )

(require rosette/solver/z3/z3)
(require rosette/solver/kodkod/kodkod)

(provide superoptimize 
         proper-machine-config
	 ;; optimize 
         ;; linear-search binary-search 
         ;; optimize-cost
         counterexample 
         generate-input-states
         )


(define (sym-input)
  (define-symbolic* input number?)
  input
  )

(define (interpret-spec spec start-state assumption)
  (assume start-state assumption)
  ;; (pretty-display "interpret spec")
  (define res (interpret spec start-state))
  ;; (pretty-display "done interpret spec")
  res
  )

;; code: non-encoded concrete code
;; config: machine config
(define (proper-machine-config code config)
  (define encoded-code (encode code))
  (define (solve-until-valid config)
    (set-machine-config config)
    (current-solver (new kodkod%))
    (configure [bitwidth bit] [loop-bound 20])
    (define state (default-state (sym-input)))
    (with-handlers* 
     ([exn:fail? 
       (lambda (e)
         (if  (equal? (exn-message e) "solve: no satisfying execution found")
              (let ([new-config (config-adjust config)])
                (if (config-exceed-limit? new-config)
                    (raise "Cannot find inputs to the program for the memory size < 1000.
1) Try increasing memory size when calling (set-machine-config).
2) Some operation in interpret.rkt might not be legal for Rosette's symbolic object.")
                    (solve-until-valid new-config)))
              (raise e)))])
     (solve (interpret encoded-code state))
     config))

  (solve-until-valid config))

(define (generate-inputs-inner n spec start-state assumption)
  ;; (pretty-display `(generate-inputs-inner ,n ,assumption))
  ;; (print-struct spec)
  ;; (display-state start-state)
  (current-solver (new kodkod%))
  (configure [bitwidth bit] [loop-bound 20])
  (define const-range 
    (list->vector
     (cons (- (arithmetic-shift 1 (sub1 bit)))
           (for/list ([i (sub1 bit)]) (arithmetic-shift 1 i)))))
  (define const-range-len (vector-length const-range))

  (define (generate-one-input random-f)
    (make-hash 
     (for/list ([v sym-vars]) 
               (let ([val (random-f)])
                 (cons v val)))))

  (define sym-vars (get-sym-vars start-state))

  ;; All 0s
  (define input-zero (list (make-hash (for/list ([v sym-vars]) (cons v 0)))))

  (define m (quotient n 2))
  ;; Random
  (define input-random
    (for/list ([i m])
              (generate-one-input 
               (lambda () (let ([rand (random (min 4294967087 (<< 1 bit)))])
                            (if (>= rand (<< 1 (sub1 bit)))
                                (- rand (<< 1 bit))
                                rand))))))

  ;; Random in const list
  (define input-random-const
    (for/list ([i (- n m 1)])
              (generate-one-input 
               (lambda () 
                 (vector-ref const-range (random const-range-len))))))

  (define inputs (append input-zero input-random input-random-const))

  ;; Construct cnstr-inputs.
  (define cnstr-inputs (list))
  (define first-solve #t)
  (define (loop [extra #t] [count n])
    (define (assert-extra-and-interpret)
      ;; Assert that the solution has to be different.
      (assert extra)
      (interpret-spec spec start-state assumption))
    (define sol (solve (assert-extra-and-interpret)))
    (define restrict-pairs (list))
    (set! first-solve #f)
    (for ([pair (solution->list sol)])
         ;; Filter only the ones that matter.
         (when (hash-has-key? (car inputs) (car pair))
               (set! restrict-pairs (cons pair restrict-pairs))))
    (unless (empty? restrict-pairs)
          (set! cnstr-inputs (cons restrict-pairs cnstr-inputs))
          (when (> count 1)
                (loop 
                 (and extra (ormap (lambda (x) (not (equal? (car x) (cdr x)))) restrict-pairs))
                 (sub1 count)))))

  (with-handlers* 
   ([exn:fail? 
     (lambda (e)
       (if  (equal? (exn-message e) "solve: no satisfying execution found")
            (if first-solve
                (raise "Cannot construct valid inputs.")
                (when debug (pretty-display "no more!")))
            (raise e)))])
   (loop))

  (set! cnstr-inputs (list->vector (reverse cnstr-inputs)))
  (define cnstr-inputs-len (vector-length cnstr-inputs))
  (when debug (pretty-display `(cnstr-inputs ,cnstr-inputs-len ,cnstr-inputs)))

  ;; Modify inputs with cnstr-inputs
  (when (> cnstr-inputs-len 0)
        (for ([i n]
              [input inputs])
             (let ([cnstr-input (vector-ref cnstr-inputs (modulo i cnstr-inputs-len))])
               (for ([pair cnstr-input])
                    (hash-set! input (car pair) (cdr pair))))))

  (values sym-vars 
          (map (lambda (x) (sat (make-immutable-hash (hash->list x)))) inputs)))

(define (generate-input-states n spec assumption)
  (define start-state (default-state (sym-input)))
  (define-values (sym-vars sltns)
    (generate-inputs-inner n spec start-state assumption))
  (map (lambda (x) (evaluate-state start-state x)) sltns))

;; Superoptimize program given
;; spec: program specification (naive code)
;; sketch: skeleton of the output program
;; constraint: constraint on the output state
;; cost: upperbound (exclusive) of the cost of the output program
(define (superoptimize spec sketch constraint 
                       [cost #f]
                       #:assume-interpret [assume-interpret #t]
                       #:assume [assumption (no-assumption)])
  (pretty-display (format "SUPERPOTIMIZE: assume-interpret = ~a" assume-interpret))
  ;; (print-struct spec)
  ;; (print-struct sketch)
  ;; (pretty-display constraint)
  ;; (pretty-display assumption)

  ;(current-solver (new z3%))
  (current-solver (new kodkod%))
  (configure [bitwidth bit] [loop-bound 20])
  (define start-state (default-state (sym-input)))
  (define spec-state #f)
  (define sketch-state #f)

  ;; (pretty-display ">>>>>>>>>>> START >>>>>>>>>>>>>")
  ;; (display-state start-state)

  ;; (define (interpret-spec-test)
  ;;   (define test-state (default-state 0))
  ;;   (assume test-state assumption)
  ;;   (pretty-display "interpret spec test")
  ;;   (display-state (interpret spec test-state)))

  ;; (interpret-spec-test)

  (define (interpret-spec!)
    (pretty-display "========== interpret spec")
    (set! spec-state (interpret-spec spec start-state assumption)))

  (define (compare-spec-sketch)
    (pretty-display "=========== interpret sketch")
    (set! sketch-state (interpret sketch start-state spec-state))
    
    ;; (pretty-display ">>>>>>>>>>> SPEC >>>>>>>>>>>>>")
    ;; (display-state spec-state)
    ;; (pretty-display ">>>>>>>>>>> SKETCH >>>>>>>>>>>>>")
    ;; (display-state sketch-state)
    ;; (pretty-display ">>>>>>>>>>> FORALL >>>>>>>>>>>>>")
    ;; (pretty-display (get-sym-vars start-state))
    (pretty-display "check output")
    (assert-output spec-state sketch-state constraint cost))
  
  ;; Collect input variables and contruct their init values.
  (define-values (sym-vars inputs)
    (generate-inputs-inner 2 spec start-state assumption))
  ;;(pretty-display `(inputs ,inputs))

  (define model 
    (timeout
     3600
     (synthesize 
      #:forall sym-vars
      #:init inputs
      #:assume (if assume-interpret (interpret-spec!) (assume start-state assumption))
      #:guarantee (compare-spec-sketch))
     )
    )

  (define final-program (decode-sym sketch model))
  (define final-cost (evaluate (progstate-cost sketch-state) model))

  (pretty-display ">>> superoptimize-output")
  (print-struct final-program)
  (pretty-display (format "limit cost = ~a" cost))
  (pretty-display (format "old cost = ~a" (progstate-cost spec-state)))
  (pretty-display (format "new cost = ~a" final-cost))
  (pretty-display "=====================================")
  (clear-asserts)
  (values final-program final-cost)
  )


(define (counterexample spec program constraint
                     #:assume [assumption (no-assumption)])
  (when debug 
        (pretty-display (format "program-eq? START bit = ~a" bit))
        (pretty-display "spec:")
        (print-struct spec)
        (pretty-display "program:")
        (print-struct program)
        ;; (pretty-display "constraint:")
        ;; (display-state constraint)
        ;; (pretty-display (format "assumption: ~a" assumption))
        )
        
  (current-solver (new kodkod%))
  (clear-asserts)
  (configure [bitwidth bit] [loop-bound 20])
  (define start-state (default-state (sym-input)))
  (define spec-state #f)
  (define program-state #f)

  (define (interpret-spec!)
    (set! spec-state (interpret-spec spec start-state assumption)))

  (define (compare)
    ;; (pretty-display "eq: interpret program")
    (set! program-state (interpret program start-state spec-state))
    
    ;; (pretty-display ">>>>>>>>>>> SPEC >>>>>>>>>>>>>")
    ;; (display-state spec-state)
    ;; (pretty-display ">>>>>>>>>>> PROG >>>>>>>>>>>>>")
    ;; (display-state program-state)

    ;; (pretty-display "check output")
    ;; (pretty-display constraint)
    (assert-output spec-state program-state 
                   (struct-copy progstate constraint [cost #f])
                   #f))

  (with-handlers* ([exn:fail? 
                    (lambda (e)
                      (when debug (pretty-display "program-eq? SAME"))
                      (if (equal? (exn-message e) "verify: no counterexample found")
                          #f
                          (raise e)))])
    (let ([model (verify #:assume (interpret-spec!) #:guarantee (compare))])
      (when debug (pretty-display "program-eq? DIFF"))
      (let ([state (evaluate-state start-state model)])
        ;; (pretty-display model)
        ;; (display-state state)
        ;; (raise "done")
        state)
      )))
