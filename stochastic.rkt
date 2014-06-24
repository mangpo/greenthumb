#lang racket

(require 
 ;; ISA independent
 "ast.rkt" "controller.rkt"
 ;; ISA dependent
 "vpe/interpret.rkt" "vpe/state.rkt" "vpe/print.rkt"
 )

(define w-error 9999)
(define beta 1)
(define opcode-mass 0.25)
(define operand-mass 0.25)
(define swap-mass 0.25)
(define inst-mass 0.25)
(define nop-mass 0.8)

  
(define (stochastic-optimize spec sketch info constraint assumption)
  ;; Generate testcases
  (define inputs (generate-inputs 4 spec info assumption))
  (define outputs (map (lambda (x) (interpret spec x)) inputs))
  ;; MCMC sampling

  (mcmc-main spec spec inputs outputs constraint info assumption)
  )

(define (mutate p)
  (define type (random))
  (define new-p (vector-copy p))
  (define index (random (vector-length new-p)))
  (define entry (vector-ref new-p index))
  (define opcode-id (inst-op entry))
  (define opcode-name (vector-ref inst-id opcode-id))
  (cond
   [(< type opcode-mass)
    (define class (get-class opcode-name))
    (when class
      (define new-opcode-name (list-ref (remove opcode-name class) 
                                        (random (sub1 (length class)))))
      (define new-opcode-id (vector-member inst-id new-opcode-name))
      (vector-set! new-p index (inst new-opcode-id (inst-args entry))))]

   [(< type (+ opcode-mass operand-mass))
    (define ranges (get-arg-ranges opcode-name))
    (when ranges
          (define args (inst-args entry))
          (define change (random (vector-length ranges)))
          (define valid-vals (vector-ref ranges change))
          (define new-val (vector-ref valid-vals (random (length valid-vals))))
          (vector-set! args change new-val))
    ]
   
    
    ;;TODO
   )

  new-p)

(define (mcmc-main target init inputs outputs constraint info assumption)
  (define best-program target)
  (define best-cost #f)

  (define (cost-one-input program input output)
    (with-handlers [(exn? w-error)]
      (let* ([program-out (interpret program input)]
             [correct (correctness-cost output program-out constraint)])
        (if (= correct 0)
            (if (program-eq? target program info constraint 
                             #:bit 32 #:assume assumption)
                (begin
                  (pretty-display "Found correct program!")
                  (print-struct program)
                  0)
                1)
            correct))))

  (define (cost-all-inputs program)
    (+ (performance-cost program)
       (foldl (lambda (x res) (+ res (cost-one-input program))) 
              inputs outputs)))

  (define (accept current-cost proposal)
    (define proposal-cost (cost-all-inputs proposal))
    (define k (* beta (- proposal-cost current-cost)))
    (define accept-rate (min 1 (exp (- k))))
    (< (random) accept-rate)
    )

  ;; Main loop
  (define (iter current current-cost)
    (define proposal (mutate current))
    (when (accept current-cost proposal)
          (set! current proposal))
    (iter current))

  (with-handlers ([exn:break? (lambda (e) best-program)])
    (timeout 3600 (iter init (cost-all-inputs init)))))
