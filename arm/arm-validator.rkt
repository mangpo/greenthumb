#lang s-exp rosette

(require "../validator.rkt"
         "../ast.rkt" "arm-ast.rkt"
         "../machine.rkt" "arm-machine.rkt" "arm-simulator-rosette.rkt")
(provide arm-validator%)

(define arm-validator%
  (class validator%
    (super-new)
    (inherit-field printer machine simulator)
    (inherit sym-op sym-arg encode-sym)
    (override encode-sym-inst) 
              ;; get-sym-vars evaluate-state
              ;; assume assert-state-eq)

    (set! simulator (new arm-simulator-rosette% [machine machine]))

    ;; (define (get-sym-vars state)
    ;;   (define lst (list))
    ;;   (define (add x)
    ;;     (when (term? x)
    ;;           (set! lst (cons x lst))))

    ;;   (for ([r (progstate-regs state)]) (add r))
    ;;   (for ([m (progstate-memory state)]) (add m))
    ;;   (add (progstate-z state))
    ;;   lst)

    ;; ;; Used for generate input and counterexample.
    ;; (define (evaluate-state state sol)
    ;;   (define-syntax-rule (eval x model)
    ;;     (let ([ans (evaluate x model)])
    ;;       (if (term? ans) 0 ans)))

    ;;   (define regs (vector-copy (progstate-regs state)))
    ;;   (define memory (vector-copy (progstate-memory state)))
    ;;   ;; CAUTION: input state sets z to be 0.
    ;;   (define z (eval (progstate-z state) sol))
    ;;   (define fp (progstate-fp state))
      
    ;;   (for ([i (vector-length regs)]
    ;;         [reg regs])
    ;;        (vector-set! regs i (eval reg sol)))
      
    ;;   (for ([i (vector-length memory)]
    ;;         [mem memory])
    ;;        (vector-set! memory i (eval mem sol)))

    ;;   (progstate regs memory z fp))

    (define (encode-sym-inst x)
      (if (inst-op x)
          (send printer encode-inst x)
          (arm-inst (sym-op) 
                    (vector (sym-arg) (sym-arg) (sym-arg) (sym-arg))
                    (sym-op)
                    (sym-arg)
                    (sym-op))))

    ;; (define (assert-state-eq state1 state2 constraint)
    ;;   (when debug (pretty-display "start assert-output"))
    ;;   (define regs (progstate-regs constraint))
    ;;   (define memory (progstate-memory constraint))
    ;;   (define z (progstate-z constraint))

    ;;   (define regs1 (progstate-regs state1))
    ;;   (define memory1 (progstate-memory state1))
    ;;   (define z1 (progstate-z state1))

    ;;   (define regs2 (progstate-regs state2))
    ;;   (define memory2 (progstate-memory state2))
    ;;   (define z2 (progstate-z state2))
      
    ;;   (for ([r regs]
    ;;         [r1 regs1]
    ;;         [r2 regs2])
    ;;        (when r (assert (equal? r1 r2))))

    ;;   (for ([m memory]
    ;;         [m1 memory1]
    ;;         [m2 memory2])
    ;;        (when m (assert (equal? m1 m2))))

    ;;   (when z (assert (equal? z1 z2)))

    ;;   (when debug (pretty-display "end assert-output"))
    ;;   )


    ;; ;; Default: no assumption
    ;; (define (assume state assumption)
    ;;   (when assumption
    ;;         (raise "No support for assumption")))

    ))
