#lang s-exp rosette

(require "../solver.rkt"
         "../ast.rkt" "neon-ast.rkt" 
         "../machine.rkt" "neon-machine.rkt" "neon-simulator-rosette.rkt")
(provide neon-solver%)

(define neon-solver%
  (class solver%
    (super-new)
    (inherit-field printer machine simulator)
    (inherit sym-op sym-arg)
    (override get-sym-vars evaluate-state
              encode-sym decode-sym sym-insts
              assume assert-output
              assume-relax)

    (set! simulator (new neon-simulator-rosette% [machine machine]))

    (define (sym-insts size)
      (encode-sym (for/vector ([i size]) (neon-inst #f #f #f #f))))

    (define (get-sym-vars state)
      (define lst (list))
      (define-syntax-rule (add x)
        (when (term? x)
              (set! lst (cons x lst))))

      (for ([x (progstate-dregs state)]) (add x))
      (for ([x (progstate-rregs state)]) (add x))
      (for ([x (progstate-memory state)]) (add x))
      lst)

    (define (evaluate-state state sol)
      (define dregs (vector-copy (progstate-dregs state)))
      (define rregs (vector-copy (progstate-rregs state)))
      (define memory (vector-copy (progstate-memory state)))

      (define-syntax-rule (eval x model)
        (let ([ans (evaluate x model)])
          (if (term? ans) 0 ans)))
      
      (for ([i (vector-length dregs)]
            [dreg dregs])
           (vector-set! dregs i (bitwise-and #xff (eval dreg sol))))
      
      (for ([i (vector-length rregs)]
            [rreg rregs])
           (vector-set! rregs i (eval rreg sol)))
      
      (for ([i (vector-length memory)]
            [mem memory])
           (vector-set! memory i (bitwise-and #xff (eval mem sol))))

      (progstate dregs rregs memory))

    (define (encode-sym code)

      (define (sym-byte)
        (define-symbolic* byte number?)
        (assert (and (>= byte 1) (<= byte 8)))
        byte)

      (define (sym-type)
        ;;(pretty-display `(sym-const ,(get-field ntypes machine)))
        (define-symbolic* type number?)
        (assert (and (>= type 0) (< type (get-field ntypes machine))))
        type)

      (define (sym-const)
        (define-symbolic* const number?)
        (assert (and (>= const -16) (<= const 16)))
        const)

      (define (first-arg op)
        (define ld-st
          (ormap (lambda (x) (equal? op (send machine get-inst-id x))) '(vld1 vld2 vld1! vld2! vst1 vst1! vst2 vst2!)))
        (if ld-st
            (cons (sym-arg) (vector (sym-arg) (sym-arg) (sym-arg) (sym-arg)))
            (sym-arg)))

      (define (encode-inst-sym x)
        ;; (pretty-display `(encode-inst-sym ,(inst-op x)))
        (if (inst-op x)
            ;; Concrete instruction
            (send printer encode-inst x)
            ;; Hole
            (let ([op (sym-op)])
              (neon-inst 
               op
               (vector (first-arg op) (sym-arg) (sym-arg) (sym-const))
               (sym-byte)
               (sym-type))
              )))

      (for/vector ([x code]) (encode-inst-sym x)))
    

    (define (decode-sym code model)
      (define (decode-inst-sym x)
        (send 
         printer decode-inst
         (neon-inst (evaluate (inst-op x) model)
                    (vector-map (lambda (a) (evaluate a model)) (inst-args x))
                    (evaluate (inst-byte x) model)
                    (evaluate (inst-type x) model))))

      (for/vector ([x code]) (decode-inst-sym x)))

    (define (assert-output state1 state2 constraint)
      (when debug (pretty-display "start assert-output"))
      (define dregs (progstate-dregs constraint))
      (define rregs (progstate-rregs constraint))
      (define memory (progstate-memory constraint))

      (define dregs1 (progstate-dregs state1))
      (define rregs1 (progstate-rregs state1))
      (define memory1 (progstate-memory state1))

      (define dregs2 (progstate-dregs state2))
      (define rregs2 (progstate-rregs state2))
      (define memory2 (progstate-memory state2))
      
      (for ([d dregs]
            [d1 dregs1]
            [d2 dregs2])
           (when d (assert (equal? d1 d2))))
      
      (for ([r rregs]
            [r1 rregs1]
            [r2 rregs2])
           (when r (assert (equal? r1 r2))))

      (for ([m memory]
            [m1 memory1]
            [m2 memory2])
           (when m (assert (equal? m1 m2))))

      (when debug (pretty-display "end assert-output"))
      )

    ;; Default: no assumption
    (define (assume state assumption)
      (define dregs (progstate-dregs state))
      (define rregs (progstate-rregs state))
      (define memory (progstate-memory state))
      (for ([x dregs]) (assert (and (>= x 0) (< x 256))))
      (for ([x rregs]) (assert (and (>= x 0) (< x 256))))
      (for ([x memory]) (assert (and (>= x 0) (< x 256))))
      (when assumption
            (raise "No support for assumption")))

    (define (assume-relax state assumption)
      (when assumption
            (raise "No support for assumption")))
      

    ))
    
    
