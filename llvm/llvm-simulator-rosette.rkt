#lang rosette

(require "../simulator-rosette.rkt" "../ops-rosette.rkt" "../inst.rkt" "llvm-machine.rkt")
(provide llvm-simulator-rosette%)

(define llvm-simulator-rosette%
  (class simulator-rosette%
    (super-new)
    (init-field machine)
    (override interpret performance-cost get-constructor)

    (define (get-constructor) llvm-simulator-rosette%)

    (define bit (get-field bitwidth machine))
    (define nop-id (get-field nop-id machine))
    (define opcodes (get-field opcodes machine))

    (define-syntax-rule (binop op)
      (lambda (x y)
        (bitvector->integer
         (op (integer->bitvector x (bitvector bit))
             (integer->bitvector y (bitvector bit))))))
    
    (define iadd (binop bvadd))
    (define isub (binop bvsub))
    (define imul (binop bvmul))
    (define isdiv (binop bvsdiv))
    (define iudiv (binop bvudiv))
    (define iand (binop bvand))
    (define ior  (binop bvor))
    (define ixor (binop bvxor))
    (define ilshr (binop bvlshr))
    (define iashr (binop bvashr))
    (define ishl (binop bvshl))
    
    (define (clz xx)
      (let ([x (integer->bitvector xx (bitvector bit))]
            [mask (integer->bitvector (arithmetic-shift 1 (sub1 bit)) (bitvector bit))]
            [count 0]
            [still #t])
        (for ([i bit])
             (when still
                   (let ([res (bvand x mask)])
                     (set! x (bvshl x (bv 1 bit)))
                     (if (equal? res (bv 0 bit))
                         (set! count (add1 count))
                         (set! still #f)))))
        count))

    
    ;; Interpret a given program from a given state.
    ;; state: initial progstate
    (define (interpret program state [ref #f])
      (define out (vector-copy (progstate-var state)))
      (define out-vec4
        (for/vector ([vec (progstate-vec4 state)])
                    (and vec (vector-copy vec))))
      (define mem (progstate-memory state))
      (set! mem (and mem (send* mem clone (and ref (progstate-memory ref)))))

      (define (interpret-step step)
        (define op (inst-op step))
        (define args (inst-args step))
        ;;(pretty-display `(step ,op ,args ,(equal? `nop `lshr#))) ;; buggy
        ;;(pretty-display `(step ,op ,args ,(inst-eq `lshr#) ,(vector-ref opcodes op))) ;; okay

        (define (apply-scalar f val1 val2)
          (for/vector ([i (vector-length val1)])
                      (f (vector-ref val1 i) (vector-ref val2 i))))

        ;; sub add
        (define (rrr f)
          (define d (vector-ref args 0))
          (define a (vector-ref args 1))
          (define b (vector-ref args 2))
          (define val (f (vector-ref out a) (vector-ref out b)))
          (vector-set! out d val))

        ;; sub add
        (define (rrr-vec f)
          (define d (vector-ref args 0))
          (define a (vector-ref args 1))
          (define b (vector-ref args 2))
          (define val (apply-scalar f (vector-ref out-vec4 a) (vector-ref out-vec4 b)))
          (vector-set! out-vec4 d val)
          )
        
        ;; subi addi
        (define (rri f)
          (define d (vector-ref args 0))
          (define a (vector-ref args 1))
          (define b (vector-ref args 2))
          (define val (f (vector-ref out a) b))
          (vector-set! out d val))
        
        ;; subi addi
        (define (rri-vec f)
          (define d (vector-ref args 0))
          (define a (vector-ref args 1))
          (define b (vector-ref args 2))
          (define val (apply-scalar f (vector-ref out-vec4 a) b))
          (vector-set! out-vec4 d val))
        
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
         [(inst-eq `add)
          (rrr iadd)]
         [(inst-eq `sub) (rrr isub)]

         [(inst-eq `mul) (rrr imul)]
         [(inst-eq `sdiv) (rrr isdiv)]
         [(inst-eq `udiv) (rrr iudiv)]
         
         [(inst-eq `and) (rrr iand)]
         [(inst-eq `or)  (rrr ior)]
         [(inst-eq `xor) (rrr ixor)]
         
         [(inst-eq `lshr) (rrr ilshr)]
         [(inst-eq `ashr) (rrr iashr)]
         [(inst-eq `shl)  (rrr ishl)]

         ;; rrr (vector)
         [(inst-eq `add_v4) (rrr-vec iadd)]
         
         ;; rri
         [(inst-eq `add#) (rri iadd)]
         [(inst-eq `sub#) (rri isub)]
         
         [(inst-eq `mul#) (rri imul)]

         
         [(inst-eq `and#) (rri iand)]
         [(inst-eq `or#)  (rri ior)]
         [(inst-eq `xor#) (rri ixor)]

         [(inst-eq `lshr#) (rri ilshr)]
         [(inst-eq `ashr#) (rri iashr)]
         [(inst-eq `shl#)  (rri ishl)]
         
         ;; rri (vector)
         [(inst-eq `add_v4#) (rri-vec iadd)]
         
         ;; rir
         [(inst-eq `_add) (rir iadd)]
         [(inst-eq `_sub) (rir isub)]
         [(inst-eq `_mul) (rir imul)]
         
         ;; [(inst-eq `_and) (rir bitwise-and)]
         ;; [(inst-eq `_or)  (rir bitwise-ior)]
         ;; [(inst-eq `_xor) (rir bitwise-xor)]

         [(inst-eq `_lshr) (rir ilshr)]
         [(inst-eq `_ashr) (rir iashr)]
         [(inst-eq `_shl)  (rir ishl)]
         
         [(inst-eq `ctlz) (rr clz)]

         [(inst-eq `store) (store)]
         [(inst-eq `load) (load)]

         [else
          (assert #f (format "simulator: undefine instruction ~a" op))]))
      
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
