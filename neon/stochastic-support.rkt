#lang racket

(require "../ast.rkt" "machine.rkt")
(provide init-operand-ranges 
         correctness-cost performance-cost
         get-arg-ranges nop-id
         random-instruction
         get-mutate-type mutate-operand-specific mutate-other mutate-dist)


(define mutate-dist 
  #hash((opcode . 1) (operand . 1) (swap . 0) (instruction . 1) (byte . 1) (type . 1)))

(define nop-id (vector-member `nop inst-id))

(define dreg-range #f)
(define qreg-range #f)
(define rreg-range #f)
(define const-range #f)
;;(define bit-range #f)
;;(define mem-range #f)
(define index-range #f)

(define (init-operand-ranges)
  (set! dreg-range (list->vector (range nregs-d)))
  (set! qreg-range (list->vector (range nregs-d (+ nregs-d (quotient nregs-d 2)))))
  (set! rreg-range (list->vector (range nregs-r)))
  (set! const-range 
        (list->vector
         (append (range -16 17) (list 31 65)
                 ;; 24 #xffff #x3fff 490 655 -500)
                 (for/list ([i (range 5 31)]) 
                           (arithmetic-shift 1 i))
                 (list (- (arithmetic-shift 1 31))))))
  (set! index-range (list->vector (range 1 8))))

;; TODO: better way to define this
(define (get-arg-ranges opcode-name entry)
  (define args (inst-args entry))
  (cond
   [(member opcode-name '(nop))
    (vector)]

   [(member opcode-name '(vld1 vld1! vld2 vld2!)) 
    (vector #f rreg-range)]

   [(member opcode-name '(vmovi vandi)) ;; TODO: different const-range for mvni
    (if (< (vector-ref args 0) nregs-d)
        (vector dreg-range const-range)
        (vector qreg-range const-range))]

   [(member opcode-name '(vmov))
    (if (< (vector-ref args 0) nregs-d)
        (vector dreg-range dreg-range)
        (vector qreg-range qreg-range))]

   [(member opcode-name '(vmla vand))
    (if (< (vector-ref args 0) nregs-d)
        (vector dreg-range dreg-range dreg-range)
        (vector qreg-range qreg-range qreg-range))]
   
   [(member opcode-name '(vmlai vexti))
    (define byte (inst-byte entry))
    (define index-range (list->vector (range (quotient 8 byte))))
    (if (< (vector-ref args 0) nregs-d)
        (vector dreg-range dreg-range dreg-range index-range)
        (vector qreg-range qreg-range qreg-range index-range))]
   
   [(member opcode-name '(vmlal))
    (vector qreg-range dreg-range dreg-range)]
   
   [(member opcode-name '(vmlali))
    (define byte (inst-byte entry))
    (define index-range (list->vector (range (quotient 8 byte))))
    (vector qreg-range dreg-range dreg-range index-range)]))

(define (random-type-from-op opcode-name)
  (cond
   [(member opcode-name '(vmla vmlai vmlal vmlali)) (random 2)]
   [else #f]))

(define (random-instruction [opcode-id (random (vector-length inst-id))])
  (define opcode-name (vector-ref inst-id opcode-id))
  (define byte (arithmetic-shift 1 (random 4)))
  (define type-id (random-type-from-op opcode-name))
  (cond
   [(member opcode-name '(nop)) (inst opcode-id (vector) byte type-id)]

   [(member opcode-name '(vld1 vld1!))
    (define ranges
      (get-arg-ranges opcode-name (inst opcode-id #f byte type-id)))
    (define prop (random))
    (define n
      (cond
       [(<= prop 0.5) 1]
       [(<= prop 0.75) 2]
       [(<= prop 0.875) 3]
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
    (inst opcode-id args byte type-id)
    ]

   [(member opcode-name '(vld2 vld2!))
    (define ranges
      (get-arg-ranges opcode-name (inst opcode-id #f byte type-id)))
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
    (inst opcode-id args byte type-id)
    ]

   [else
    (define first-arg (random (+ nregs-d (quotient nregs-d 2))))
    (define ranges
      (get-arg-ranges opcode-name 
                      (inst opcode-id (vector first-arg) byte type-id)))
    (define args
      (for/vector ([i (vector-length ranges)])
                  (random-from-vec (vector-ref ranges i))))
    (inst opcode-id args byte type-id)]))

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

(define (mutate-type index entry p stat)
  (define opcode-id (inst-op entry))
  (define opcode-name (vector-ref inst-id opcode-id))
  (cond
   [(member opcode-name '(vmla vmlai vmlal vmlali))
    (define new-p (vector-copy p))
    (define type (inst-type entry))
    (define new-type (if (= type 0) 1 0)) ;; 0 = s, 1 = u
    (define new-entry (inst-copy entry [type new-type]))
    (vector-set! new-p index new-entry)
    new-p]
   [else (raise (format "mutate-type: undefined for ~a " opcode-name))]))

(define (mutate-byte index entry p stat)
  (define opcode-id (inst-op entry))
  (define opcode-name (vector-ref inst-id opcode-id))
  (define new-p (vector-copy p))
  (define byte (inst-byte entry))
  (define new-byte (random-from-list-ex (list 1 2 4 8) byte))
  (define new-entry (inst-copy entry [byte new-byte]))
  (vector-set! new-p index new-entry)
  new-p)

(define (mutate-other index entry p stat type)
  (cond
   [(equal? type `type) (mutate-type index entry p stat)]
   [(equal? type `byte) (mutate-byte index entry p stat)]
   [else (raise (format "mutate-other: undefined mutate ~a" type))]))

;; TODO: pre-build the table
(define (get-mutate-type opcode-name)
  (define mutations '(instruction swap))
  
  (pretty-display `(get-mutate-type ,opcode-name))
  (unless (equal? opcode-name `nop)
          (pretty-display `(not-nop ,opcode-name))
    ;; operand
    (set! mutations (cons `operand mutations))
    ;; opcode
    (when (member opcode-name '(vld1 vld1! vld2 vld2! vmovi vandi))
          (set! mutations (cons `opcode mutations)))
    ;; byte
    (when (member opcode-name '(vld2 vld2! vmla vmlai vmlal vmlali vexti))
          (set! mutations (cons `byte mutations)))
    ;; type
    (when (member opcode-name '(vmla vmlai vmlal vmlali))
          (set! mutations (cons `type mutations))))

  (pretty-display `(mutations ,mutations))
  (define sum 0)
  (define prop (map (lambda (x) 
                      (let ([v (hash-ref mutate-dist x)])
                        (set! sum (+ sum v))
                        sum))
                    mutations))
  (set! prop (map (lambda (x) (exact->inexact (/ x sum))) prop))
  (define rand (random))
  (define (loop name-list prop-list)
    (if (<= rand (car prop-list))
        (car name-list)
        (loop (cdr name-list) (cdr prop-list))))
  (loop mutations prop))

(define (correctness-cost state1 state2 constraint stat)
  (define (diff-cost x y)
    (define (pop-count a)
      (define count 0)
      (define mask 1)
      (for ([i 8])
           (when (> (bitwise-and a mask) 0) (set! count (add1 count)))
           (set! mask (* mask 2)))
      count)

    (pop-count (bitwise-xor (bitwise-and x #xff)
                            (bitwise-and y #xff))))

  
  (define dregs (progstate-dregs constraint))
  (define rregs (progstate-rregs constraint))
  (define memory (progstate-memory constraint))

  (define dregs1 (progstate-dregs state1))
  (define rregs1 (progstate-rregs state1))
  (define memory1 (progstate-memory state1))
  (define cost1 (progstate-cost state1))

  (define dregs2 (progstate-dregs state2))
  (define rregs2 (progstate-rregs state2))
  (define memory2 (progstate-memory state2))
  (define cost2 (progstate-cost state2))

  (define correctness 0)
  (for ([x dregs]
        [x1 dregs1]
        [x2 dregs2])
       (when x (set! correctness (+ correctness (diff-cost x1 x2)))))

  (for ([x rregs]
        [x1 rregs1]
        [x2 rregs2])
       (when x (set! correctness (+ correctness (diff-cost x1 x2)))))

  (for ([x memory]
        [x1 memory1]
        [x2 memory2])
       (when x (set! correctness (+ correctness (diff-cost x1 x2)))))

  correctness)

(define (performance-cost code)
  (vector-length code))

