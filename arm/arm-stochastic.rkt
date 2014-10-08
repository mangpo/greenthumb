#lang racket

(require "../stochastic.rkt"
         "../ast.rkt"
         "../machine.rkt" "arm-machine.rkt" 
         "arm-simulator-racket.rkt" "arm-solver.rkt")

(provide arm-stochastic%)

(define arm-stochastic%
  (class stochastic%
    (super-new)
    (inherit-field machine printer solver simulator stat mutate-dist)
    (override correctness-cost get-arg-ranges)

    (set! solver (new arm-solver% [machine machine] [printer printer]))
    (set! simulator (new arm-simulator-racket% [machine machine]))

    (define bit (get-field bit machine))
    (define inst-id (get-field inst-id machine))
    (define nregs (send machine get-nregs))
    (define nmems (send machine get-nmems))

    (define reg-range (list->vector (range nregs)))
    (define const-range
          (list->vector
           (append (range -16 17) (list (sub1 bit))
                   ;; 24 #x3fffffff)
                   (for/list ([i (range 5 (sub1 bit))]) 
                             (arithmetic-shift 1 i))
                   (list (- (arithmetic-shift 1 (sub1 bit)))))))
    
    (define bit-range (list->vector (range bit)))
    (define mem-range (list->vector (range nmems)))


    ;; nargs, ranges
    (define (get-arg-ranges opcode-name entry)
      (define class-id (send machine get-class-id opcode-name))
      ;;(pretty-display `(get-arg-ranges ,opcode-name ,class-id))
      (cond
       [(equal? class-id 0) (vector reg-range reg-range reg-range)]
       [(equal? class-id 1) (vector reg-range reg-range const-range)]
       [(equal? class-id 2) (vector reg-range reg-range bit-range)]
       [(equal? class-id 3) (vector reg-range reg-range)]
       [(equal? class-id 4) (vector reg-range const-range)]
       [(equal? class-id 5) (vector reg-range reg-range reg-range reg-range)]
       [(equal? class-id 6) (vector reg-range reg-range bit-range bit-range)]
       [(equal? opcode-name `bfc) (vector reg-range bit-range bit-range)]
       [else (vector)]))

    ;; state1: reference
    ;; state2: proposal
    (define (correctness-cost state1 state2 constraint)
      (define (diff-cost x y)
        (define (pop-count a)
          (set! a (- a (bitwise-and (arithmetic-shift a -1) #x55555555)))
          ;;(pretty-display a)
          (set! a (+ (bitwise-and a #x33333333)
                     (bitwise-and (arithmetic-shift a -2) #x33333333)))
          ;;(pretty-display a)
          (set! a (bitwise-and (+ a (arithmetic-shift a -4)) #x0f0f0f0f))
          (set! a (+ a (arithmetic-shift a -8)))
          (set! a (+ a (arithmetic-shift a -16)))
          (bitwise-and a #x3f))

        (pop-count (bitwise-xor (bitwise-and x #xffffffff) 
                                (bitwise-and y #xffffffff))))
      
      (define regs (progstate-regs constraint))
      (define memory (progstate-memory constraint))
      (define len (vector-length memory))

      (define regs1 (progstate-regs state1))
      (define memory1 (progstate-memory state1))
      (define dep (progstate+-extra state1))
      (define regs1-dep (progstate-regs dep))
      (define memory1-dep (progstate-memory dep))

      (define regs2 (progstate-regs state2))
      (define memory2 (progstate-memory state2))
      (define inter (progstate+-extra state2))
      
      (define correctness 0)
      (define relax #f)
      (define misalign-penalty 1)
      (define misalign 0)
      (if relax
          (for ([r regs]
                [i (vector-length regs)])
               (when r
                     (let* ([r1 (vector-ref regs1 i)]
                            [cost-r (diff-cost r1 (vector-ref regs2 i))]
                            [best-j i])
                       (for ([j (vector-length regs)])
                            (let* ([r2 (vector-ref regs2 j)]
                                   [this-cost (diff-cost r1 r2)])
                              (when (< this-cost cost-r)
                                    (set! cost-r this-cost)
                                    (set! best-j j)
                                    )))
                       (set! correctness (+ correctness cost-r))
                       (unless (= best-j i) (set! misalign (+ misalign misalign-penalty)))
                       )))
          
          (for ([r regs]
                [r1 regs1]
                [r2 regs2]
                [r1-dep regs1-dep]
                [i (vector-length regs)])
               (when r 
                     ;;(pretty-display `(correctness reg ,i))
                     (let ([my-cost (diff-cost r1 r2)])
                       (set! correctness (+ correctness (adjust my-cost r1 r1-dep inter)))
                       ;; (set! correctness (+ correctness my-cost))
                       ))))

      (for ([m memory]
            [m1 memory1]
            [m2 memory2]
            [m1-dep memory1-dep])
           (when m 
                 (let ([my-cost (diff-cost m1 m2)])
                   (set! correctness (+ correctness (adjust my-cost m1 m1-dep inter))))
                   ;;(set! correctness (+ correctness my-cost))
                   ))

      ;; (when debug
      ;;       (pretty-display `(correct ,(vector-ref regs1 0) ,(vector-ref regs2 0) ,correctness)))
      
      ;; (when (= correctness 0)
      ;;       (if (= misalign 0)
      ;;           (void)
      ;;           (send stat inc-misalign)))
      ;; (+ correctness misalign)
      (+ correctness misalign)
      )
    ))




  
