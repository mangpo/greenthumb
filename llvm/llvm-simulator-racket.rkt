#lang racket

(require "../simulator-racket.rkt" "../ops-racket.rkt" "../inst.rkt" "llvm-machine.rkt")
(provide llvm-simulator-racket%)

(define llvm-simulator-racket%
  (class simulator-racket%
    (super-new)
    (init-field machine)
    (override interpret performance-cost get-constructor)

    (define (get-constructor) llvm-simulator-racket%)

    (define bit (get-field bitwidth machine))
    (define nop-id (get-field nop-id machine))
    (define opcodes (get-field opcodes machine))

    (define-syntax-rule (bvop op)     
      (lambda (x y) (finitize-bit (op x y))))
    
    (define-syntax-rule (finitize-bit x) (finitize x bit))
    (define (shl a b) (<< a b bit))
    (define (ushr a b) (>>> a b bit))

    (define bvadd  (bvop +))
    (define bvsub  (bvop -))
    (define bvshl  (bvop shl))
    (define bvshr  (bvop >>))
    (define bvushr (bvop ushr))

    (define (clz x)
      (let ([mask (shl 1 (sub1 bit))]
            [count 0]
            [still #t])
        (for ([i bit])
             (when still
                   (let ([res (bitwise-and x mask)])
                     (set! x (shl x 1))
                     (if (= res 0)
                         (set! count (add1 count))
                         (set! still #f)))))
        count))
    
    ;; Interpret a given program from a given state.
    ;; state: initial progstate
    (define (interpret program state [ref #f])
      (define out (vector-copy (progstate-var state)))
      (define out-vec4
        (for/vector ([vec (progstate-vec4 state)])
                    (vector-copy vec)))
      (define mem (progstate-memory state))
      (set! mem (and mem (send* mem clone (and ref (progstate-memory ref)))))

      (define (interpret-step step)
        (define op (inst-op step))
        (define args (inst-args step))

        (define (apply-scalar f val1 val2)
          (cond
           [(vector? val1)
            (for/vector ([v1 val1] [v2 val2]) (f v1 v2))]
           [else (f val1 val2)]))

        ;; sub add
        (define (rrr f [out out])
          (define d (vector-ref args 0))
          (define a (vector-ref args 1))
          (define b (vector-ref args 2))
          (define val (apply-scalar f (vector-ref out a) (vector-ref out b)))
          (vector-set! out d val))
        
        ;; subi addi
        (define (rri f [out out])
          (define d (vector-ref args 0))
          (define a (vector-ref args 1))
          (define b (vector-ref args 2))
          (define val (apply-scalar f (vector-ref out a) b))
          (vector-set! out d val))
        
        ;; subi addi
        (define (rir f)
          (define d (vector-ref args 0))
          (define a (vector-ref args 1))
          (define b (vector-ref args 2))
          (define val (f a (vector-ref out b)))
          (vector-set! out d val))

        ;; count leading zeros
        (define (rr f)
          (define d (vector-ref args 0))
          (define a (vector-ref args 1))
          (define val (f (vector-ref out a)))
          (vector-set! out d val))

        (define (load)
          (define d (vector-ref args 0))
          (define a (vector-ref args 1))
          (vector-set! out d (send* mem load (vector-ref out a))))

        (define (store)
          (define val (vector-ref args 0))
          (define addr (vector-ref args 1))
          (send* mem store (vector-ref out addr) (vector-ref out val))) 
      
        (define-syntax inst-eq
          (syntax-rules ()
            ((inst-eq x) (equal? x (vector-ref opcodes op)))
            ((inst-eq a b ...) (or (inst-eq a) (inst-eq b) ...))))
        
        (cond
         ;; rrr
         [(inst-eq `nop) (void)]
         [(inst-eq `add) (rrr bvadd)]
         [(inst-eq `sub) (rrr bvsub)]
         
         [(inst-eq `and) (rrr bitwise-and)]
         [(inst-eq `or)  (rrr bitwise-ior)]
         [(inst-eq `xor) (rrr bitwise-xor)]
         
         [(inst-eq `lshr) (rrr bvushr)]
         [(inst-eq `ashr) (rrr bvshr)]
         [(inst-eq `shl)  (rrr bvshl)]

         ;; rrr (vector)
         [(inst-eq `add_v4) (rrr bvadd out-vec4)]
         
         ;; rri
         [(inst-eq `add#) (rri bvadd)]
         [(inst-eq `sub#) (rri bvsub)]
         
         [(inst-eq `and#) (rri bitwise-and)]
         [(inst-eq `or#)  (rri bitwise-ior)]
         [(inst-eq `xor#) (rri bitwise-xor)]

         [(inst-eq `lshr#) (rri bvushr)]
         [(inst-eq `ashr#) (rri bvshr)]
         [(inst-eq `shl#)  (rri bvshl)]
         
         ;; rri (vector)
         [(inst-eq `add_v4#) (rri bvadd out-vec4)]
         
         ;; rir
         ;; [(inst-eq `_add) (rir bvadd)]
         [(inst-eq `_sub) (rir bvsub)]
         
         ;; [(inst-eq `_and) (rir bitwise-and)]
         ;; [(inst-eq `_or)  (rir bitwise-ior)]
         ;; [(inst-eq `_xor) (rir bitwise-xor)]

         [(inst-eq `_lshr) (rir bvushr)]
         [(inst-eq `_ashr) (rir bvshr)]
         [(inst-eq `_shl)  (rir bvshl)]
         
         [(inst-eq `ctlz)  (rr clz)]

         [(inst-eq `store) (store)]
         [(inst-eq `load)  (load)]

         [else (assert #f (format "simulator: undefine instruction ~a" op))]))
      
      (for ([x program])
           (interpret-step x))

      (vector out out-vec4 mem)
      )

    (define (performance-cost program)
      (define cost 0)
      (for ([x program])
	   (unless (= (inst-op x) nop-id) (set! cost (add1 cost))))
      cost)
    
    ))
