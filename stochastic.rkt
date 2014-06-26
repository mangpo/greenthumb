#lang racket

(require 
 ;; ISA independent
 "ast.rkt" "controller.rkt"
 ;; ISA dependent
 "vpe/state.rkt" "vpe/print.rkt"
 "vpe/interpret-racket.rkt" "vpe/stochastic-support.rkt"
 )

(provide stochastic-optimize)

;;;;;;;;;;;;;;;;;;;;; Parameters ;;;;;;;;;;;;;;;;;;;
(define w-error 9999)
(define beta 1)
(define opcode-mass 0.25)
(define operand-mass 0.25)
(define swap-mass 0.25)
(define inst-mass 0.25)
(define nop-mass 0.8)
(define ntests 8)

;;;;;;;;;;;;;;;;;;;;; Stat ;;;;;;;;;;;;;;;;;;;
(define propose-stat (make-vector 5 0)) ;; opcode, operand, swap, inst, nop
(define accept-stat (make-vector 5 0))
(define name-stat '#(opcode operand swap inst nop))

(define current-mutate #f)

(define start-time (current-seconds))
(define iter-count 0)
(define best-correct-program #f)
(define best-correct-cost #f)
(define best-correct-time #f)
(define best-program #f)
(define best-cost #f)

(define-syntax-rule (inc-propose x)
  (begin
    (vector-set! propose-stat x (add1 (vector-ref propose-stat x)))
    (set! current-mutate x)))

(define-syntax-rule (inc-accept)
  (vector-set! accept-stat current-mutate 
               (add1 (vector-ref accept-stat current-mutate))))

(define (print-stat)
  (define time (- (current-seconds) start-time))
  (pretty-display "---------------------------------------------------------")
  (pretty-display (format "iterations:\t~a" iter-count))
  (pretty-display (format "elapsed-time:\t~a" time))
  (pretty-display (format "iterations/s:\t~a" (exact->inexact (/ iter-count time))))
  (pretty-display (format "best-cost:\t~a" best-cost))
  (pretty-display (format "best-correct-cost:\t~a" best-correct-cost))
  (pretty-display (format "best-correct-time:\t~a" best-correct-time))
  (newline)
  (define proposed (foldl + 0 (vector->list propose-stat)))
  (define accepted (foldl + 0 (vector->list accept-stat)))
  (pretty-display (format "Mutate\tProposed\t\tAccepted\t\tAccepted/Proposed"))
  (for ([i 5])
    (pretty-display (format "~a\t~a\t~a\t~a" 
                            (vector-ref name-stat i)
                            (exact->inexact (/ (vector-ref propose-stat i) proposed))
                            (exact->inexact (/ (vector-ref accept-stat i) proposed))
                            (exact->inexact (/ (vector-ref accept-stat i) (vector-ref propose-stat i))))))
  (newline)
  (pretty-display (format "acceptance-rate:\t~a" 
                          (exact->inexact (/ accepted proposed)))))

(define (print-stat-to-file file)
  (with-output-to-file #:exists 'truncate file
    print-stat))
  
  
;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;
(define (stochastic-optimize spec info constraint 
                             #:assume [assumption (no-assumption)]
                             #:synthesize [syn-mode #f]
                             #:name [name "temp"])
  (init-stochastic)
  ;; Generate testcases
  (when debug 
        (pretty-display ">>> Phase 1: genenrate input states"))
  (define inputs (generate-input-states ntests spec info assumption #:bit 32))

  (when debug
        (for ([i inputs])
             (display-state i))
        (pretty-display ">>> Phase 2: genenrate output states"))
  (define outputs (map (lambda (x) (interpret spec x)) inputs))

  ;; MCMC sampling
  (define sketch (random-insts (vector-length spec)))
  (when debug
        (pretty-display ">>> Phase 3: stochastic search")
        (pretty-display "sketch:")
        (print-struct sketch))
  (mcmc-main spec (if syn-mode sketch spec) 
             inputs outputs constraint info assumption name)
  )

(define (random-insts n)
  (for/vector ([i n])
       (define opcode-id (random (vector-length inst-id)))
       (define opcode-name (vector-ref inst-id opcode-id))
       (define args (random-args-from-op opcode-name))
       (inst opcode-id args)))

(define (random-from-list-ex lst ex)
  (let ([new-lst (remove ex lst)])
    (list-ref new-lst (random (length new-lst)))))

(define (random-from-vec-ex vec ex)
  (define len (vector-length vec))
  (define (inner)
    (define sample (vector-ref vec (random len)))
    (if (eq? sample ex)
        (inner)
        sample))
  (if (= len 1)
      (vector-ref vec 0)
      (inner)))

(define (random-from-vec vec)
  (vector-ref vec (random (vector-length vec))))

(define (mutate p)
  (define type (random))
  (define new-p (vector-copy p))
  (define vec-len (vector-length new-p))
  (define index (random vec-len))
  (define entry (vector-ref new-p index))
  (define opcode-id (inst-op entry))
  (define opcode-name (vector-ref inst-id opcode-id))
  (define nop (get-nop-opcode))
  (when debug (pretty-display (format " >> mutate prop = ~a, index = ~a" type index)))
  (cond
   ;; opcode
   [(and (not (equal? opcode-name nop)) (< type opcode-mass))
    (define class (get-class opcode-name))
    (when debug
          (pretty-display (format " >> mutate opcode"))
          (pretty-display (format " --> org = ~a ~a" opcode-name opcode-id))
          (pretty-display (format " --> class = ~a" class)))
    (when class
      (define new-opcode-name (random-from-list-ex class opcode-name))
      (define new-opcode-id (vector-member new-opcode-name inst-id))
      (when debug
            (pretty-display (format " --> new = ~a ~a" new-opcode-name new-opcode-id)))
      (vector-set! new-p index (inst new-opcode-id (inst-args entry))))
    (inc-propose 0)
    ]

   ;; operand
   [(and (not (equal? opcode-name nop)) (< type (+ opcode-mass operand-mass)))
    (when debug (pretty-display " >> mutate operand"))
    (define ranges (get-arg-ranges opcode-name))
    (when (> (vector-length ranges) 0)
          (define args (vector-copy (inst-args entry)))
          (define change (random (vector-length ranges)))
          (define valid-vals (vector-ref ranges change))
          (define new-val (random-from-vec-ex valid-vals (vector-ref args change)))
          (when debug
                (pretty-display (format " --> org = ~a ~a" opcode-name args))
                (pretty-display (format " --> new = [~a]->~a)" change new-val)))
          (vector-set! args change new-val)
          (vector-set! new-p index (inst opcode-id args)))
    (inc-propose 1)
    ]
   
   ;; swap
   [(if (not (equal? opcode-name nop))
        (< type (+ opcode-mass operand-mass swap-mass))
        (< type (/ swap-mass (+ swap-mass inst-mass))))
    (define index2 (random-from-list-ex (range vec-len) index))
    (when debug
          (pretty-display " >> mutate swap")
          (pretty-display (format " --> swap = ~a" index2)))
    (vector-set! new-p index (vector-ref new-p index2))
    (vector-set! new-p index2 entry)
    (inc-propose 2)
    ]
   
   ;; brand new instruction
   [else
    (define new-opcode-name
      (if (and (not (equal? opcode-name nop)) (< (random) nop-mass))
          (begin (inc-propose 4) nop)
          (begin (inc-propose 3) (random-from-vec-ex inst-id opcode-name))))
    (define new-opcode-id (vector-member new-opcode-name inst-id))
    (when debug
          (pretty-display (format " >> mutate instruction ~a" new-opcode-name)))
    (define new-args (random-args-from-op new-opcode-name))
    (vector-set! new-p index (inst new-opcode-id new-args))
    ])

  new-p)

(define (random-args-from-op opcode-name)
  (define ranges (get-arg-ranges opcode-name))
  (when debug (pretty-display (format " --> ranges ~a" ranges)))
  (for/vector ([i (vector-length ranges)])
              (random-from-vec (vector-ref ranges i))))
  

(define (mcmc-main target init inputs outputs constraint info assumption driver-name)
  (pretty-display ">>> start MCMC sampling")
  (define syn-mode #t)
  (set! best-correct-program target)
  (set! best-correct-cost (performance-cost target))
  (set! best-program #f)
  (set! best-cost (sub1 (arithmetic-shift 1 (sub1 bit))))

  (define (cost-one-input program input output)
    (with-handlers* ([exn:break? (lambda (e) (raise e))]
                     [exn? (lambda (e) w-error)])
      (let ([program-out (interpret program input)])
        (correctness-cost output program-out constraint))
      )
    )

  (define (cost-all-inputs program)
    (define correct (foldl (lambda (input output res) 
                             (+ res (cost-one-input program input output))) 
                           0 inputs outputs))
    (when (= correct 0)
          (if (program-eq? target program info constraint #:bit 32 #:assume assumption)
              (set! syn-mode #f)
              (set! correct 1)))

    (define total-cost (if syn-mode correct (+ (performance-cost program) correct)))
    (when (< total-cost best-cost)
          (set! best-cost total-cost)
          (set! best-program program))
    (when (and (= correct 0) (< total-cost best-correct-cost))
          (set! best-correct-cost total-cost)
          (set! best-correct-program program)
          (set! best-correct-time (- (current-seconds) start-time))
          (with-output-to-file #:exists 'truncate (format "~a.best" driver-name)
            (thunk
             (pretty-display (format "best-correct-cost: ~a" best-correct-cost))
             (pretty-display (format "best-correct-time: ~a" best-correct-time))
             (print-struct best-correct-program)))
          )
    total-cost
    )

  (define (accept current-cost proposal-cost)
    (define k (* beta (- proposal-cost current-cost)))
    (define accept-rate (min 1 (exp (- k))))
    (when debug (pretty-display (format "accept probability: ~a" accept-rate)))
    (< (random) accept-rate)
    )

  ;; Main loop
  (define (iter current current-cost)
    (set! iter-count (add1 iter-count))
    (when (= (modulo iter-count 1000) 0)
          (print-stat)
          (print-stat-to-file (format "~a.stat" driver-name))
          )
    (define proposal (mutate current))
    (when debug
          (pretty-display (format "================ Propose (syn=~a) =================" syn-mode))
          (print-struct proposal))
    (define proposal-cost (cost-all-inputs proposal))
    (when debug
          (pretty-display (format "current cost: ~a" current-cost))
          (pretty-display (format "proposal cost: ~a" proposal-cost)))

    (when (accept current-cost proposal-cost)
          (when debug
                (pretty-display "================ ACCEPT! =================")
                (print-struct proposal))
          (set! current proposal)
          (set! current-cost proposal-cost)
          (inc-accept)
          )
    (iter current current-cost))

  (with-handlers ([exn:break? (lambda (e) 
                                (print-stat)
                                (print-stat-to-file (format "~a.stat" driver-name))
                                (print-struct best-correct-program)
                                best-correct-program)])
                 (timeout 10 (iter init (cost-all-inputs init)))))