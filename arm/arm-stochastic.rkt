#lang racket

(require "../stochastic.rkt"
         "../inst.rkt"
         "../machine.rkt" "arm-machine.rkt")

(provide arm-stochastic%)

(define arm-stochastic%
  (class stochastic%
    (super-new)
    (inherit-field machine stat mutate-dist live-in)
    (inherit random-args-from-op mutate pop-count32 correctness-cost-base inst-copy-with-op)
    (override correctness-cost 
              )
    (set! mutate-dist
          #hash((opcode . 2) (operand . 1) (swap . 1) (instruction . 1)))
	  

    (define bit (get-field bitwidth machine))


    ;; Mutate opcode.
    ;; index: index to be mutated
    ;; entry: instruction at index in p
    ;; p: entire program
    (define/override (mutate-opcode index entry p)
      (define opcode-id (inst-op entry))
      (define opcode-name (send machine get-opcode-name opcode-id))
      (define op-types
        (filter identity (for/list ([op opcode-id] [index (in-naturals)]) (and (>= op 0) index))))
      (define op-type (random-from-list op-types))
      (define checks (remove op-type (range (vector-length opcode-id))))
      (define class
        (filter
         (lambda (x) (for/and ([index checks]) (= (vector-ref x index) (vector-ref opcode-id index))))
        (send machine get-class-opcodes opcode-id)))
      (when debug
            (pretty-display (format " >> mutate opcode"))
            (pretty-display (format " --> org = ~a ~a" opcode-name opcode-id))
            (pretty-display (format " --> op-type = ~a" op-type))
            (pretty-display (format " --> class = ~a" class)))
      (cond
       [class
        (define new-opcode-id (random-from-list-ex class opcode-id))
        (define new-p (vector-copy p))
        (when debug
              (pretty-display (format " --> new = ~a ~a" (send machine get-opcode-name new-opcode-id) new-opcode-id)))
        (vector-set! new-p index (inst-copy-with-op entry new-opcode-id))
        (send stat inc-propose `opcode)
        new-p]

       [else (mutate p)]))


    (define (diff-cost x y)
      (pop-count32 (bitwise-xor (bitwise-and x #xffffffff) 
                                (bitwise-and y #xffffffff))))
    
    ;; Compute correctness cost sum of all bit difference in live variables.
    ;; state1: expected in progstate format
    ;; state2: actual in progstate format
    (define (correctness-cost state1 state2 constraint)
      (+ (correctness-cost-base (progstate-regs state1)
                                (progstate-regs state2)
                                (progstate-regs constraint)
                                diff-cost)
         (if (progstate-memory constraint)
             (send (progstate-memory state1) correctness-cost
                   (progstate-memory state2) diff-cost bit)
             0)
         (if (and (progstate-z constraint)
                  (not (equal? (progstate-z state1) (progstate-z state2))))
             1
             0)))
    ))




  
