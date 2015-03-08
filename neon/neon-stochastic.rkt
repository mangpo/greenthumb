#lang racket

(require "../stochastic.rkt"
         "../ast.rkt" "neon-ast.rkt"
         "../machine.rkt" "neon-machine.rkt" 
         "neon-simulator-racket.rkt" "neon-validator.rkt")

(provide neon-stochastic%)

(define neon-stochastic%
  (class stochastic%
    (super-new)
    (inherit-field machine printer validator simulator stat mutate-dist)
    (override correctness-cost 
              inst-copy-with-op inst-copy-with-args
              random-instruction
              get-mutations mutate-operand-specific mutate-other)

    (set! mutate-dist 
      #hash((opcode . 2) (operand . 2) (swap . 2) (instruction . 2) (byte . 1) (type . 1)))
    (set! validator (new neon-validator% [machine machine] [printer printer]))
    (set! simulator (new neon-simulator-racket% [machine machine]))

    (define inst-id (get-field inst-id machine))
    (define nregs-d (send machine get-nregs-d))
    (define nregs-r (send machine get-nregs-r))
    (define nmems (send machine get-nmems))

    (define-syntax-rule (get-arg-ranges opcode-name entry)
      (send machine get-arg-ranges opcode-name entry #f))
    
    (define (inst-copy-with-op x op) (struct-copy neon-inst x [op #:parent inst op]))
    (define (inst-copy-with-args x args) (struct-copy neon-inst x [args #:parent inst args]))
    (define (random-type-from-op opcode-name)
      (cond
       [(member opcode-name '(vmla vmla@ vmlal vmlal@ vhadd vhsub vshr#)) (random 2)]
       [else #f]))

    (define (random-instruction live-in [opcode-id (random-from-list (get-field inst-pool machine))])
      ;;(pretty-display `(random-instruction ,opcode-id ,(send machine get-inst-name opcode-id)))
      (define opcode-name (vector-ref inst-id opcode-id))
      ;;(define byte (arithmetic-shift 1 (random 4)))
      (define byte (if (= (random 2) 0) 2 4)) ;; TODO, no 8 or 64 bit
      (define type-id (random-type-from-op opcode-name))
      (cond
       [(member opcode-name '(nop)) (neon-inst opcode-id (vector) byte type-id)]

       [(member opcode-name '(vld1 vld1! vst1 vst1!))
        (define ranges
          (get-arg-ranges opcode-name (neon-inst opcode-id #f byte type-id)))
        (define prop (random))
        (define n
          (cond
           [(<= prop 0.5) 1]
           [(<= prop 1) 2]
           ;;[(<= prop 0.875) 3]
           ;;[else 4]
           ))
        (define skip 1);;(add1 (random 2)))
        (define distance (* (sub1 n) skip))
        (when (>= distance nregs-d)
              (set! skip 1)
              (set! distance (sub1 n)))
        (define first-reg (random (- nregs-d distance)))
        (define ld-regs (for/vector ([i n]) (+ first-reg (* i skip))))
        (define args
          (for/vector ([i (vector-length ranges)])
                      (and (vector-ref ranges i)
                           (random-from-vec (vector-ref ranges i)))))
        (vector-set! args 0 (cons n ld-regs))
        (neon-inst opcode-id args byte type-id)
        ]

       [(member opcode-name '(vld2 vld2! vst2 vst2!))
        (define ranges
          (get-arg-ranges opcode-name (neon-inst opcode-id #f byte type-id)))
        (define prop (random))
        (define n
          (cond
           [(<= prop 0.75) 2]
           [else 4]))
        (define skip 1);;(add1 (random 2)))
        (define distance (* (sub1 n) skip))
        (when (>= distance nregs-d)
              (set! skip 1)
              (set! distance (sub1 n)))
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
        ;;(pretty-display `(ranges ,first-arg ,ranges))
        (define args
          (for/vector ([i (vector-length ranges)])
                      (random-from-vec (vector-ref ranges i))))
        (neon-inst opcode-id args byte type-id)]))

    (define (mutate-operand-specific opcode-name args index live-in)
      (when debug `(mutate-operand-specific ,opcode-name ,args ,index))

      (cond
       [(and (member opcode-name '(vld1 vld1! vld2 vld2! vst1 vst1! vst2 vst2!)) (= index 0))
        ;; TODO: need to mutate, skip, and length too
        (define len-pos (random 2))
        (define dest (vector-ref args 0))
        (define len (car dest))
        (define ld-regs (cdr dest))
        (define first-reg (vector-ref ld-regs 0))
        (define skip 1)
        (cond
         [(= len-pos 0) ;; mutate length
          (define new-len 
            (if (member opcode-name '(vld1 vld1! vst1 vst1!))
                (random-from-list-ex (list 1 2) len)
                (random-from-list-ex (list 2 4) len)))
          (when debug (pretty-display `(mutate-length ,len ,new-len)))
          (define new-first-reg
            (if (< new-len len)
                (vector-ref ld-regs (random (add1 (- len new-len))))
                (let ([start (max 0 (- first-reg (* (- new-len len) skip)))]
                      [end (min (- nregs-d new-len) first-reg)])
                  (+ start (random (add1 (- end start)))))))
          (cons new-len (for/vector ([i new-len]) (+ new-first-reg (* i skip))))
          ]
         [else ;; mutate position
          (define distance (- (vector-ref ld-regs (sub1 len)) first-reg))
          (when debug (pretty-display `(pick-first-reg ,nregs-d ,distance)))
          (define new-first-reg 
            (random-from-list-ex (range (- nregs-d distance)) first-reg))
          (define diff (- new-first-reg first-reg))
          (cons len (for/vector ([reg ld-regs]) (+ reg diff)))
          ])
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
              (when (member opcode-name '(vld2 vld2! vst2 vst2! vmla vmla@ vmlal vmlal@ vadd vsub vhadd vhsub vshr# vext# vmov# vand# vorr# vtrn vzip vuzp vswp))
                    (set! mutations (cons `byte mutations)))
              ;; type
              (when (member opcode-name '(vmlal vmlal@ vhadd vhsub vshr#))
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

