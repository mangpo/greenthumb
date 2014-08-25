#lang racket

(require "../stochastic.rkt"
         "../ast.rkt" "neon-ast.rkt"
         "../machine.rkt" "neon-machine.rkt" 
         "neon-simulator-racket.rkt" "neon-solver.rkt")

(provide neon-stochastic%)

(define neon-stochastic%
  (class stochastic%
    (super-new)
    (inherit-field machine printer solver simulator stat mutate-dist)
    (override correctness-cost 
              get-arg-ranges 
              inst-copy-with-op inst-copy-with-args
              random-instruction
              get-mutations mutate-operand-specific mutate-other)

    (set! mutate-dist 
      #hash((opcode . 1) (operand . 1) (swap . 1) (instruction . 1) (byte . 1) (type . 1)))
    (set! solver (new neon-solver% [machine machine] [printer printer]))
    (set! simulator (new neon-simulator-racket% [machine machine]))

    (define inst-id (get-field inst-id machine))
    (define nregs-d (send machine get-nregs-d))
    (define nregs-r (send machine get-nregs-r))
    (define nmems (send machine get-nmems))
    
    (define dreg-range (list->vector (range nregs-d)))
    (define qreg-range (list->vector (range nregs-d (+ nregs-d (quotient nregs-d 2)))))
    (define rreg-range (list->vector (range nregs-r)))
    (define const-range 
          (list->vector
           (append (range -16 17) (list 31 65)
                   ;; 24 #xffff #x3fff 490 655 -500)
                   (for/list ([i (range 5 31)]) 
                             (arithmetic-shift 1 i))
                   (list (- (arithmetic-shift 1 31))))))
    (define index-range (list->vector (range 1 8)))
    
    (define (inst-copy-with-op x op) (struct-copy neon-inst x [op #:parent inst op]))
    (define (inst-copy-with-args x args) (struct-copy neon-inst x [args #:parent inst args]))
    
    ;; TODO: better way to define this
    (define (get-arg-ranges opcode-name entry)
      (define args (inst-args entry))
      (cond
       [(member opcode-name '(nop))
        (vector)]

       [(member opcode-name '(vld1 vld1! vld2 vld2!)) 
        (vector #f rreg-range)]

       [(member opcode-name '(vmov# vand#)) ;; TODO: different const-range for mvni
        (if (< (vector-ref args 0) nregs-d)
            (vector dreg-range const-range)
            (vector qreg-range const-range))]

       [(member opcode-name '(vmov vtrn vzip vuzp))
        (if (< (vector-ref args 0) nregs-d)
            (vector dreg-range dreg-range)
            (vector qreg-range qreg-range))]

       [(member opcode-name '(vmla vand vadd vsub vhadd vhsub))
        (if (< (vector-ref args 0) nregs-d)
            (vector dreg-range dreg-range dreg-range)
            (vector qreg-range qreg-range qreg-range))]
       
       [(member opcode-name '(vmla@ vext#))
        (define byte (inst-byte entry))
        (define index-range (list->vector (range (quotient 8 byte))))
        (if (< (vector-ref args 0) nregs-d)
            (vector dreg-range dreg-range dreg-range index-range)
            (vector qreg-range qreg-range qreg-range index-range))]
       
       [(member opcode-name '(vmlal))
        (vector qreg-range dreg-range dreg-range)]
       
       [(member opcode-name '(vmlal@))
        (define byte (inst-byte entry))
        (define index-range (list->vector (range (quotient 8 byte))))
        (vector qreg-range dreg-range dreg-range index-range)]

       [(member opcode-name '(vshr#))
        (if (< (vector-ref args 0) nregs-d)
            (vector dreg-range dreg-range const-range)
            (vector qreg-range qreg-range const-range))]

       ))

    (define (random-type-from-op opcode-name)
      (cond
       [(member opcode-name '(vmla vmla@ vmlal vmlal@ vhadd vhsub vshr#)) (random 2)]
       [else #f]))

    (define (random-instruction [opcode-id (random (vector-length inst-id))])
      (define opcode-name (vector-ref inst-id opcode-id))
      ;;(define byte (arithmetic-shift 1 (random 4)))
      (define byte (if (= (random 2) 0) 2 4)) ;; TODO, no 8 or 64 bit
      (define type-id (random-type-from-op opcode-name))
      (cond
       [(member opcode-name '(nop)) (neon-inst opcode-id (vector) byte type-id)]

       [(member opcode-name '(vld1 vld1!))
        (define ranges
          (get-arg-ranges opcode-name (neon-inst opcode-id #f byte type-id)))
        (define prop (random))
        (define n
          (cond
           [(<= prop 0.5) 1]
           [(<= prop 0.75) 2]
           [(<= prop 0.875) 3]
           [else 4]))
        (define skip (add1 (random 2)))
        (define distance (* (sub1 n) skip))
        (define first-reg (random (- nregs-d distance)))
        (define ld-regs (for/vector ([i n]) (+ first-reg (* i skip))))
        (define args
          (for/vector ([i (vector-length ranges)])
                      (and (vector-ref ranges i)
                           (random-from-vec (vector-ref ranges i)))))
        (vector-set! args 0 (cons n ld-regs))
        (neon-inst opcode-id args byte type-id)
        ]

       [(member opcode-name '(vld2 vld2!))
        (define ranges
          (get-arg-ranges opcode-name (neon-inst opcode-id #f byte type-id)))
        (define prop (random))
        (define n
          (cond
           [(<= prop 0.75) 2]
           [else 4]))
        (define skip (random 2))
        (define distance (* (sub1 n) skip))
        (define first-reg (random (- nregs-d distance)))
        (define ld-regs (for/vector ([i n]) (+ first-reg (* i skip))))
        (define args
          (for/vector ([i (vector-length ranges)])
                      (and (vector-ref ranges i)
                           (random-from-vec (vector-ref ranges i)))))
        (vector-set! args 0 (cons n ld-regs))
        (neon-inst opcode-id args byte type-id)
        ]

       [else
        (define first-arg (random (+ nregs-d (quotient nregs-d 2))))
        (define ranges
          (get-arg-ranges opcode-name 
                          (neon-inst opcode-id (vector first-arg) byte type-id)))
        (define args
          (for/vector ([i (vector-length ranges)])
                      (random-from-vec (vector-ref ranges i))))
        (neon-inst opcode-id args byte type-id)]))

    (define (mutate-operand-specific opcode-name args index)
      (when debug `(mutate-operand-specific ,opcode-name ,args ,index))
      ;; TODO: redundant
      (define (random-from-list-ex lst ex)
        (let ([new-lst (remove ex lst)])
          (if (empty? new-lst)
              ex
              (list-ref new-lst (random (length new-lst))))))
      (cond
       [(and (member opcode-name '(vld1 vld1! vld2 vld2!)) (= index 0))
        (define dest (vector-ref args 0))
        (define len (car dest))
        (define ld-regs (cdr dest))
        (define first-reg (vector-ref ld-regs 0))
        (define distance (- (vector-ref ld-regs (sub1 len)) first-reg))
        (when debug (pretty-display `(pick-first-reg ,nregs-d ,distance)))
        (define new-first-reg 
          (random-from-list-ex (range (- nregs-d distance)) first-reg))
        (define diff (- new-first-reg first-reg))
        (cons len (for/vector ([reg ld-regs]) (+ reg diff)))
        ]
       [else (raise (format "mutate-operand-specific: undefined for ~a at index ~a." opcode-name index))]))

    (define (mutate-type index entry p)
      (define opcode-id (inst-op entry))
      (define opcode-name (vector-ref inst-id opcode-id))
      (define new-p (vector-copy p))
      (define type (inst-type entry))
      (define new-type (if (equal? type 0) 1 0)) ;; 0 = s, 1 = u
      (define new-entry (struct-copy neon-inst entry [type new-type]))
      (vector-set! new-p index new-entry)
      (send stat inc-propose `type)
      new-p)

    (define (mutate-byte index entry p)
      (define opcode-id (inst-op entry))
      (define opcode-name (vector-ref inst-id opcode-id))
      (define new-p (vector-copy p))
      (define byte (inst-byte entry))
      (define new-byte (random-from-list-ex (list 2 4) byte)) ;; TODO no 64 bit
      (define new-entry (struct-copy neon-inst entry [byte new-byte]))
      (vector-set! new-p index new-entry)
      (send stat inc-propose `byte)
      new-p)

    (define (mutate-other index entry p type)
      (cond
       [(equal? type `type) (mutate-type index entry p)]
       [(equal? type `byte) (mutate-byte index entry p)]
       [else (raise (format "mutate-other: undefined mutate ~a" type))]))

    (define (get-mutations opcode-name)
      (define mutations '(instruction swap))
      
      (unless (equal? opcode-name `nop)
              ;; operand
              (set! mutations (cons `operand mutations))
              ;; opcode
              (when (send machine get-class-id opcode-name)
                    (set! mutations (cons `opcode mutations)))
              ;; byte
              (when (member opcode-name '(vld2 vld2! vmla vmla@ vmlal vmlal@ vadd vsub vhadd vhsub vshr# vext# vtrn vzip vuzp))
                    (set! mutations (cons `byte mutations)))
              ;; type
              (when (member opcode-name '(vmla vmla@ vmlal vmlal@ vhadd vhsub vshr#))
                    (set! mutations (cons `type mutations))))
      mutations)

    (define (correctness-cost state1 state2 constraint)
      (define (diff-cost-8 x y)
        (define (pop-count a)
          (set! a (- a (bitwise-and (arithmetic-shift a -1) #x55)))
          ;;(pretty-display a)
          (set! a (+ (bitwise-and a #x33)
                     (bitwise-and (arithmetic-shift a -2) #x33)))
          ;;(pretty-display a)
          (bitwise-and (+ a (arithmetic-shift a -4)) #x0f))

        (pop-count (bitwise-xor (bitwise-and x #xff)
                                (bitwise-and y #xff))))

      (define (diff-cost-32 x y)
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

        (pop-count (bitwise-xor x y)))

      
      (define dregs (progstate-dregs constraint))
      (define rregs (progstate-rregs constraint))
      (define memory (progstate-memory constraint))

      (define dregs1 (progstate-dregs state1))
      (define rregs1 (progstate-rregs state1))
      (define memory1 (progstate-memory state1))

      (define dregs2 (progstate-dregs state2))
      (define rregs2 (progstate-rregs state2))
      (define memory2 (progstate-memory state2))

      (define correctness 0)
      ;; (for ([x dregs]
      ;;       [x1 dregs1]
      ;;       [x2 dregs2])
      ;;      (when x (set! correctness (+ correctness (diff-cost x1 x2)))))

      ;; (for ([x rregs]
      ;;       [x1 rregs1]
      ;;       [x2 rregs2])
      ;;      (when x (set! correctness (+ correctness (diff-cost x1 x2)))))

      ;; (for ([x memory]
      ;;       [x1 memory1]
      ;;       [x2 memory2])
      ;;      (when x (set! correctness (+ correctness (diff-cost x1 x2)))))
      
      (define (32bit x index)
        (bitwise-ior 
         (bitwise-and (vector-ref x index) #xff)
         (arithmetic-shift (bitwise-and (vector-ref x (+ index 1)) #xff) 8)
         (arithmetic-shift (bitwise-and (vector-ref x (+ index 2)) #xff) 16)
         (arithmetic-shift (bitwise-and (vector-ref x (+ index 3)) #xff) 24)))

      (define (count-vector-32 vec vec1 vec2)
        (for ([i (quotient (vector-length vec) 4)])
             (let ([index (* i 4)])
               (when (vector-ref vec index) 
                     (set! correctness 
                           (+ correctness 
                              (diff-cost-32 (32bit vec1 index) (32bit vec2 index))))))))

      (define (count-vector-8 vec vec1 vec2)
        (for ([x vec]
              [x1 vec1]
              [x2 vec2])
             (when x (set! correctness (+ correctness (diff-cost-8 x1 x2))))))

      (count-vector-32 dregs dregs1 dregs2)
      (count-vector-8  rregs rregs1 rregs2)
      (count-vector-32 memory memory1 memory2)    

      correctness)

    ))

