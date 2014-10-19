#lang s-exp rosette

(require "../solver.rkt"
         "../ast.rkt" "arm-ast.rkt"
         "../machine.rkt" "arm-machine.rkt" "arm-simulator-rosette.rkt")
(provide arm-solver%)

(define arm-solver%
  (class solver%
    (super-new)
    (inherit-field printer machine simulator)
    (inherit sym-op sym-arg)
    (override get-sym-vars evaluate-state
              encode-sym-inst evaluate-inst
              assume assert-output 
              len-limit window-size)

    (set! simulator (new arm-simulator-rosette% [machine machine]))

    (define (len-limit) 2)
    (define (window-size) 4)

    (define (get-sym-vars state)
      (define lst (list))
      (define (add x)
        (when (term? x)
              (set! lst (cons x lst))))

      (for ([r (progstate-regs state)]) (add r))
      (for ([m (progstate-memory state)]) (add m))
      lst)

    (define (evaluate-state state sol)
      (define regs (vector-copy (progstate-regs state)))
      (define memory (vector-copy (progstate-memory state)))

      (define-syntax-rule (eval x model)
        (let ([ans (evaluate x model)])
          (if (term? ans) 0 ans)))
      
      (for ([i (vector-length regs)]
            [reg regs])
           (vector-set! regs i (eval reg sol)))
      
      (for ([i (vector-length memory)]
            [mem memory])
           (vector-set! memory i (eval mem sol)))

      (progstate regs memory))

    (define (encode-sym-inst x)
      (if (inst-op x)
          (send printer encode-inst x)
          (arm-inst (sym-op) 
                    (vector (sym-arg) (sym-arg) (sym-arg) (sym-arg))
                    (sym-op)
                    (sym-arg)
                    (sym-op))))

    (define (evaluate-inst x model)
      (arm-inst (evaluate (inst-op x) model)
                (vector-map 
                 (lambda (a) (evaluate a model)) (inst-args x))
                (evaluate (inst-shfop x) model)
                (evaluate (inst-shfarg x) model)
                (evaluate (inst-cond x) model)))

    (define (assert-output state1 state2 constraint)
      (when debug (pretty-display "start assert-output"))
      (define regs (progstate-regs constraint))
      (define memory (progstate-memory constraint))

      (define regs1 (progstate-regs state1))
      (define memory1 (progstate-memory state1))

      (define regs2 (progstate-regs state2))
      (define memory2 (progstate-memory state2))
      
      (for ([r regs]
            [r1 regs1]
            [r2 regs2])
           (when r (assert (equal? r1 r2))))

      (for ([m memory]
            [m1 memory1]
            [m2 memory2])
           (when m (assert (equal? m1 m2))))

      (when debug (pretty-display "end assert-output"))
      )


    ;; Default: no assumption
    (define (assume state assumption)
      (when assumption
            (raise "No support for assumption")))

    ))
