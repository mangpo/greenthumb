#lang racket

(require 
 ;; ISA independent
 "ast.rkt" "controller.rkt"
 ;; ISA dependent
 "vpe/interpret.rkt" "vpe/state.rkt" "vpe/print.rkt"
 )

(provide stochastic-optimize)

(define w-error 9999)
(define beta 1)
(define opcode-mass 0.25)
(define operand-mass 0.25)
(define swap-mass 0.25)
(define inst-mass 0.25)
(define nop-mass 0.8)

  
(define (stochastic-optimize spec sketch info constraint 
                             #:assume [assumption (no-assumption)])
  (init-stochastic)
  ;; Generate testcases
  (pretty-display ">>> Phase 1: genenrate input states")
  (define inputs (generate-input-states 4 spec info assumption #:bit 32))
  (for ([i inputs])
       (display-state i))
  (pretty-display ">>> Phase 2: genenrate output states")
  (define outputs (map (lambda (x) (interpret spec x)) inputs))

  ;; MCMC sampling
  (pretty-display ">>> Phase 3: stochastic search")
  (mcmc-main spec sketch inputs outputs constraint info assumption)
  )

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
  (pretty-display (format " >> mutate prop = ~a, index = ~a" type index))
  (cond
   ;; opcode
   [(and (not (equal? opcode-name nop)) (< type opcode-mass))
    (pretty-display (format " >> mutate opcode"))
    (define class (get-class opcode-name))
    (pretty-display (format " --> org = ~a ~a" opcode-name opcode-id))
    (pretty-display (format " --> class = ~a" class))
    (when class
      (define new-opcode-name (random-from-list-ex class opcode-name))
      (define new-opcode-id (vector-member new-opcode-name inst-id))
      (pretty-display (format " --> new = ~a ~a" new-opcode-name new-opcode-id))
      (vector-set! new-p index (inst new-opcode-id (inst-args entry))))]

   ;; operand
   [(and (not (equal? opcode-name nop)) (< type (+ opcode-mass operand-mass)))
    (pretty-display " >> mutate operand")
    (define ranges (get-arg-ranges opcode-name))
    (when (> (vector-length ranges) 0)
          (define args (vector-copy (inst-args entry)))
          (define change (random (vector-length ranges)))
          (define valid-vals (vector-ref ranges change))
          (define new-val (random-from-vec-ex valid-vals (vector-ref args change)))
          (pretty-display (format " --> org = ~a ~a" opcode-name args))
          (pretty-display (format " --> new = [~a]->~a)" change new-val))
          (vector-set! args change new-val)
          (vector-set! new-p index (inst opcode-id args))
          )
    ]
   
   ;; swap
   [(if (not (equal? opcode-name nop))
        (< type (+ opcode-mass operand-mass swap-mass))
        (< type (/ swap-mass (+ swap-mass inst-mass))))
    (pretty-display " >> mutate swap")
    (define index2 (random-from-list-ex (range vec-len) index))
    (pretty-display (format " --> swap = ~a" index2))
    (vector-set! new-p index (vector-ref new-p index2))
    (vector-set! new-p index2 entry)]
   
   ;; brand new instruction
   [else
    (define new-opcode-name
      (if (and (not (equal? opcode-name nop)) (< (random) nop-mass))
          nop
          (random-from-vec-ex inst-id opcode-name)))
    (define new-opcode-id (vector-member new-opcode-name inst-id))
    (pretty-display (format " >> mutate instruction ~a" new-opcode-name))
    (define ranges (get-arg-ranges new-opcode-name))
    (define new-args (make-vector (vector-length ranges)))
    (pretty-display (format " --> ranges ~a" ranges))
    (for ([i (in-range (vector-length ranges))])
         (vector-set! new-args i (random-from-vec (vector-ref ranges i))))
    (vector-set! new-p index (inst new-opcode-id new-args))
    ])

  new-p)

(define (mcmc-main target init inputs outputs constraint info assumption)
  (pretty-display ">>> start MCMC sampling")
  (define best-program target)
  (define best-cost #f)

  (define (cost-one-input program input output)
    ;(pretty-display ">>> cost-one-intput")
    (with-handlers [(exn? (lambda (e) w-error))]
      (let ([program-out (interpret program input)])
        (correctness-cost output program-out constraint))))

  (define (cost-all-inputs program)
    (define correct (foldl (lambda (input output res) 
                             (+ res (cost-one-input program input output))) 
                           0 inputs outputs))
    (when (= correct 0)
          (unless (program-eq? target program info constraint 
                               #:bit 32 #:assume assumption)
                  (set! correct 1)))
    (+ (performance-cost program) correct))

  (define (accept current-cost proposal-cost)
    (define k (* beta (- proposal-cost current-cost)))
    (define accept-rate (min 1 (exp (- k))))
    (pretty-display (format "accept probability: ~a" accept-rate))
    (< (random) accept-rate)
    )

  ;; Main loop
  (define (iter current current-cost)
    (define proposal (mutate current))
    (pretty-display "================ Propose =================")
    (print-struct proposal)
    (define proposal-cost (cost-all-inputs proposal))
    (pretty-display (format "current cost: ~a" current-cost))
    (pretty-display (format "proposal cost: ~a" proposal-cost))

    (when (accept current-cost proposal-cost)
          (pretty-display "================ ACCEPT! =================")
          (print-struct proposal)
          (set! current proposal)
          (set! current-cost proposal-cost)
          )
    (iter current current-cost))

  ;; (with-handlers ([exn:break? (lambda (e) best-program)])
  ;;   (timeout 10 (iter init (cost-all-inputs init)))))
  (iter init (cost-all-inputs init)))
