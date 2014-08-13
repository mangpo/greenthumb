#lang racket

(require 
 ;; ISA independent
 "ast.rkt" "solver.rkt" "stat.rkt"
 ;; ISA dependent
 "neon/machine.rkt" "neon/print.rkt"
 "neon/interpret.rkt" "neon/stochastic-support.rkt"
 )

(provide stochastic-optimize)

;;;;;;;;;;;;;;;;;;;;; Parameters ;;;;;;;;;;;;;;;;;;;
(define w-error 9999)
(define beta 1)

(define opcode-mass 0.35)
(define operand-mass 0.35)
(define swap-mass 0.15)
(define inst-mass 0.15)
(define nop-mass 0.8)

(define ntests 16)
  
  
;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;
(define (stochastic-optimize spec constraint 
                             syn-mode name time-limit size
                             #:assume [assumption (no-assumption)])
  (init-operand-ranges)
  ;; Generate testcases
  (when debug 
        (pretty-display ">>> Phase 1: generate input states"))
  (define inputs (generate-input-states ntests spec assumption))

  (when debug
        (for ([i inputs])
             (display-state i))
        (pretty-display ">>> Phase 2: generate output states"))
  (define outputs (map (lambda (x) (interpret spec x)) inputs))
  (when debug
        (for ([i outputs])
             (display-state i)))

  ;; MCMC sampling
  (define-syntax-rule (get-sketch) 
    (random-insts (if size size (vector-length spec))))
  (define stat (new stat% 
                    [best-correct-program spec] 
                    [best-correct-cost (performance-cost spec)]
                    [name name]))
  (mcmc-main spec (if syn-mode (get-sketch) spec) 
             inputs outputs constraint assumption stat time-limit)
  )

(define (remove-nops code)
  (list->vector 
   (filter (lambda (x) (not (equal? (inst-op x) nop-id)))
           (vector->list code))))

(define (random-insts n)
  (for/vector ([i n]) (random-instruction)))

;; Generic across architectures
(define (mutate-swap index entry p stat)
  (define new-p (vector-copy p))
  (define index2 (random-from-list-ex (range (vector-length p)) index))
  (when debug
        (pretty-display " >> mutate swap")
        (pretty-display (format " --> swap = ~a" index2)))
  (vector-set! new-p index (vector-ref new-p index2))
  (vector-set! new-p index2 entry)
  (send stat inc-propose 2)
  new-p)

;; Generic across architectures
(define (mutate-opcode index entry p stat)
  (define opcode-id (inst-op entry))
  (define opcode-name (vector-ref inst-id opcode-id))
  (define class-id (get-class-id opcode-name))
  (define class (and class-id (vector-ref classes class-id)))
  (when debug
        (pretty-display (format " >> mutate opcode"))
        (pretty-display (format " --> org = ~a ~a" opcode-name opcode-id))
        (pretty-display (format " --> class = ~a" class)))
  (cond
   [class
    (define new-opcode-name (random-from-list-ex class opcode-name))
    (define new-opcode-id (vector-member new-opcode-name inst-id))
    (define new-p (vector-copy p))
    (when debug
          (pretty-display (format " --> new = ~a ~a" new-opcode-name new-opcode-id)))
    (vector-set! new-p index (inst-copy entry [op new-opcode-id]))
    (send stat inc-propose 0)
    new-p]

   [else (mutate p stat)]))

(define (mutate-operand index entry p stat)
  (define opcode-id (inst-op entry))
  (define opcode-name (vector-ref inst-id opcode-id))
  (define args (vector-copy (inst-args entry)))
  (define ranges (get-arg-ranges opcode-name entry))
  (cond
   [(> (vector-length ranges) 0)
    (define args (vector-copy (inst-args entry)))
    (define change (random (vector-length ranges)))
    (define valid-vals (vector-ref ranges change))
    (define new-val 
      (if (vector? valid-vals)
          (random-from-vec-ex valid-vals (vector-ref args change))
          (mutate-operand-specific opcode-name args change)))
      
    (define new-p (vector-copy p))
    (when debug
          (pretty-display (format " --> org = ~a ~a" opcode-name args))
          (pretty-display (format " --> new = [~a]->~a)" change new-val)))
    (vector-set! args change new-val)
    (vector-set! new-p index (inst-copy entry [args args]))
    (send stat inc-propose 1)
    new-p]

   [else (mutate p stat)]))

(define (mutate-instruction index entry p stat)
  (define new-p (vector-copy p))
  (define opcode-id (inst-op entry))
  (define opcode-name (vector-ref inst-id opcode-id))
  (define new-opcode-id
    (if (and (not (equal? opcode-id nop-id)) (< (random) nop-mass))
        (begin (send stat inc-propose 4) 
               nop-id)
        (begin (send stat inc-propose 3) 
               (random-from-list-ex (range (vector-length inst-id)) opcode-id))))
  (when debug
        (pretty-display (format " >> mutate instruction ~a" (vector-ref inst-id new-opcode-id))))
  (define new-entry (random-instruction new-opcode-id))
  (vector-set! new-p index new-entry)
  new-p)

(define (mutate p stat)
  (define vec-len (vector-length p))
  (define index (random vec-len))
  (define entry (vector-ref p index))
  (define opcode-id (inst-op entry))
  (define opcode-name (vector-ref inst-id opcode-id))
  (define type (get-mutate-type opcode-name))
  (when debug 
        (pretty-display 
         (format " >> mutate = ~a, index = ~a" type index)))

  (cond
   [(equal? type `opcode)      (mutate-opcode index entry p stat)]
   [(equal? type `operand)     (mutate-operand index entry p stat)]
   [(equal? type `instruction) (mutate-instruction index entry p stat)]
   [(equal? type `swap)        (mutate-swap index entry p stat)]
   [else                       (mutate-other index entry p stat type)]))

;; (define (mutate p stat)
;;   (define type (random))
;;   (define new-p (vector-copy p))
;;   (define vec-len (vector-length new-p))
;;   (define index (random vec-len))
;;   (define entry (vector-ref new-p index))
;;   (define opcode-id (inst-op entry))
;;   (define opcode-name (vector-ref inst-id opcode-id))
;;   (define nop (get-nop-opcode))
;;   (define new #f)
;;   (when debug (pretty-display (format " >> mutate prop = ~a, index = ~a" type index)))
;;   (cond
;;    ;; opcode
;;    [(and (not (equal? opcode-name nop)) (< type opcode-mass))
;;     (define class-id (get-class-id opcode-name))
;;     (define class (and class-id (vector-ref classes class-id)))
;;     (when debug
;;           (pretty-display (format " >> mutate opcode"))
;;           (pretty-display (format " --> org = ~a ~a" opcode-name opcode-id))
;;           (pretty-display (format " --> class = ~a" class)))
;;     (when class
;;       (define new-opcode-name (random-from-list-ex class opcode-name))
;;       (define new-opcode-id (vector-member new-opcode-name inst-id))
;;       (when debug
;;             (pretty-display (format " --> new = ~a ~a" new-opcode-name new-opcode-id)))
;;       (vector-set! new-p index (inst new-opcode-id (inst-args entry)))
;;       (send stat inc-propose 0)
;;       (set! new #t)
;;       )
;;     ]

;;    ;; operand
;;    [(and (not (equal? opcode-name nop)) (< type (+ opcode-mass operand-mass)))
;;     (when debug (pretty-display " >> mutate operand"))
;;     (define ranges (get-arg-ranges opcode-name))
;;     (when (> (vector-length ranges) 0)
;;           (define args (vector-copy (inst-args entry)))
;;           (define change (random (vector-length ranges)))
;;           (define valid-vals (vector-ref ranges change))
;;           (define new-val (random-from-vec-ex valid-vals (vector-ref args change)))
;;           (when debug
;;                 (pretty-display (format " --> org = ~a ~a" opcode-name args))
;;                 (pretty-display (format " --> new = [~a]->~a)" change new-val)))
;;           (vector-set! args change new-val)
;;           (vector-set! new-p index (inst opcode-id args))
;;           (send stat inc-propose 1)
;;           (set! new #t)
;;           )
;;     ]
   
;;    ;; swap
;;    [(if (not (equal? opcode-name nop))
;;         (< type (+ opcode-mass operand-mass swap-mass))
;;         (< type (/ swap-mass (+ swap-mass inst-mass))))
;;     (define index2 (random-from-list-ex (range vec-len) index))
;;     (when debug
;;           (pretty-display " >> mutate swap")
;;           (pretty-display (format " --> swap = ~a" index2)))
;;     (vector-set! new-p index (vector-ref new-p index2))
;;     (vector-set! new-p index2 entry)
;;     (send stat inc-propose 2)
;;     (set! new #t)
;;     ]
   
;;    ;; brand new instruction
;;    [else
;;     (define new-opcode-name
;;       (if (and (not (equal? opcode-name nop)) (< (random) nop-mass))
;;           (begin (send stat inc-propose 4) nop)
;;           (begin (send stat inc-propose 3) (random-from-vec-ex inst-id opcode-name))))
;;     (define new-opcode-id (vector-member new-opcode-name inst-id))
;;     (when debug
;;           (pretty-display (format " >> mutate instruction ~a" new-opcode-name)))
;;     (define new-args (random-args-from-op new-opcode-name))
;;     (vector-set! new-p index (inst new-opcode-id new-args))
;;     (set! new #t)
;;     ])

;;   (if new
;;       new-p
;;       (mutate p stat))
;;       )

  

(define (mcmc-main target init inputs outputs constraint assumption 
                   stat time-limit)
  (pretty-display ">>> start MCMC sampling")
  (pretty-display ">>> Phase 3: stochastic search")
  (pretty-display "start-program:")
  (print-struct init)
  (define syn-mode #t)

  (define (cost-one-input program input output)
    (with-handlers* ([exn:break? (lambda (e) (raise e))]
                     [exn? (lambda (e) 
                             (when debug (pretty-display "Error!"))
                             w-error)])
      (let* ([t1 (current-milliseconds)]
             [program-out (interpret program input)]
             [t2 (current-milliseconds)]
             [ret (correctness-cost output program-out constraint stat)]
             [t3 (current-milliseconds)]
             )
        (send stat simulate (- t2 t1))
        (send stat check (- t3 t2))
        ret
        )))

  (define (cost-all-inputs program okay-cost)
    (define correct 0)
    (define change-mode #f)
    (for ([input inputs]
          [output outputs])
         (when correct
               (set! correct (+ correct (cost-one-input program input output)))
               (when (> correct okay-cost)
                     (set! correct #f))))

    (define ce #f)
    (when (equal? correct 0)
          (send stat inc-validate)
          (define t1 (current-milliseconds))
          (set! ce (counterexample target program constraint #:assume assumption))
          (if ce 
              (begin
                (set! correct 1)
                (set! inputs (cons ce inputs))
                (set! outputs (cons (interpret target ce) outputs))
                (pretty-display (format "Add counterexample. Total = ~a." (length inputs)))
                (display-state ce)
                )
              (begin
                (send stat inc-correct)
                (when syn-mode (set! change-mode #t) (set! syn-mode #f))
                ;(send stat inc-correct)
                ))
              
          (define t2 (current-milliseconds))
          (send stat validate (- t2 t1))
          )
    
    (if (number? correct)
         (let ([total-cost 
                (if syn-mode correct (+ (performance-cost program) correct))])
           (when debug (pretty-display `(total-cost ,total-cost)))
           (when (< total-cost (get-field best-cost stat))
                 (send stat update-best program total-cost)
                 )
           (when (and (= correct 0) (< total-cost (get-field best-correct-cost stat)))
                 (pretty-display "NEW! best-correct-program")
                 (pretty-display "program-eq? --> true")
                 (pretty-display "target:")
                 (print-struct target)
                 (pretty-display "candidate:")
                 (print-struct program)
                 (send stat update-best-correct program total-cost)
                 (pretty-display "here 1")
                 )
           (if (or (<= total-cost okay-cost) change-mode) 
                ;; return (correctness-cost . correct)
               (cons total-cost (= correct 0))
               (cons #f #f)))
         (cons #f #f))
    )

  (define (accept-cost current-cost)
    (- current-cost (/ (log (random)) beta)))

  ;; Main loop
  (define (iter current current-cost)
    (pretty-display `(iter begin))
    (send stat inc-iter)
    (define t1 (current-milliseconds))
    (pretty-display `(mutate begin))
    (define proposal (mutate current stat))
    (pretty-display `(mutate end))
    (define t2 (current-milliseconds))
    (send stat mutate (- t2 t1))
    (when debug
          (pretty-display (format "================ Current (syn=~a) =================" syn-mode))
          (print-struct current)
          ;; (define cost (cost-all-inputs current (arithmetic-shift 1 32)))
          ;; (pretty-display (format "actual cost: ~a" cost))
          (pretty-display (format "================ Propose (syn=~a) =================" syn-mode))
          (print-struct proposal)
          )
    (define n-inputs (length inputs))
    (define okay-cost (accept-cost current-cost))
    (pretty-display `(simulate begin))
    (define cost-correct (cost-all-inputs proposal okay-cost))
    (pretty-display `(simulate end))
    (define proposal-cost (car cost-correct))
    (when debug
          (pretty-display (format "current cost: ~a" current-cost))
          (pretty-display (format "okay cost: ~a" okay-cost))
          (pretty-display (format "proposal cost: ~a" proposal-cost)))

    (if proposal-cost
        (begin
          (when debug
                (pretty-display "================ ACCEPT! =================")
                (print-struct proposal))
          (send stat inc-accept)
          (when (> proposal-cost current-cost) 
                (send stat inc-accept-higher))
          ;; Adjust cost due to new counterexample
          (when (> (length inputs) n-inputs)
                (when debug (display (format "Adjust proposal cost from ~a " proposal-cost)))
                (set! proposal-cost 
                      (sub1 (+ proposal-cost (cost-one-input proposal (car inputs) (car outputs)))))
                (when debug (pretty-display (format "to ~a." proposal-cost)))
                )
          (iter (if (cdr cost-correct) (remove-nops proposal) proposal) 
                proposal-cost))
        (begin
          ;; Adjust cost due to new counterexample
          (when (> (length inputs) n-inputs)
                (when debug (display (format "Adjust current cost from ~a " current-cost)))
                (set! current-cost (+ current-cost (cost-one-input current (car inputs) (car outputs))))
                (when debug (pretty-display (format "to ~a." current-cost)))
                )
          (iter current current-cost))
        ))


  (with-handlers ([exn:break? (lambda (e) 
                                (send stat print-stat-to-file)
                                )])
         
    (timeout time-limit
             (iter init (car (cost-all-inputs init (arithmetic-shift 1 32)))))
    )
  )
