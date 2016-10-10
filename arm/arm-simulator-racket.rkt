#lang racket

(require "../simulator.rkt" "../ops-racket.rkt" 
         "../inst.rkt"
         "../machine.rkt" "arm-machine.rkt")
(provide arm-simulator-racket%)

(define arm-simulator-racket%
  (class simulator%
    (super-new)
    (init-field machine)
    (override interpret performance-cost get-constructor)

    (define (get-constructor) arm-simulator-racket%)
        
    (define bit (get-field bitwidth machine))

    (define opcodes (get-field opcodes machine))
    (define base-opcodes (vector-ref opcodes 0))
    (define cond-opcodes (vector-ref opcodes 1))
    (define shf-opcodes (vector-ref opcodes 2))
    (define ninsts (vector-length base-opcodes))

    (define (shl a b) (<< a b bit))
    (define (ushr a b) (>>> a b bit))
    (define (ror a b) (bitwise-ior (>>> a b bit) (<< a (- bit b) bit)))
    (define-syntax-rule (finitize-bit x) (finitize x bit))

    (define byte0 0)
    (define byte1 (quotient bit 4))
    (define byte2 (* 2 byte1))
    (define byte3 (* 3 byte1))
    (define byte4 bit)
    (define byte-mask (sub1 (arithmetic-shift 1 byte1)))
    (define low-mask (sub1 (shl 1 byte2)))
    (define high-mask (shl low-mask byte2))
    (define mask (sub1 (arithmetic-shift 1 bit)))

    ;; helper functions
    (define-syntax-rule (bool->num b) (if b 1 0))

    (define-syntax-rule (bvop op)     
      (lambda (x y) (finitize-bit (op x y))))

    (define-syntax-rule (bvuop op)     
      (lambda (x) (finitize-bit (op x))))

    (define-syntax-rule (bvcmp op) 
      (lambda (x y) (bool->num (op x y))))

    (define-syntax-rule (bvucmp op)   
      (lambda (x y) 
        (bool->num (if (equal? (< x 0) (< y 0)) (op x y) (op y x)))))

    (define-syntax-rule (bvshift op)
      (lambda (x y)
        (finitize-bit (op x (bitwise-and #xff y)))))

    (define-syntax-rule (bvshift# op)
      (lambda (x y)
	;;(pretty-display `(bvshift assert ,y))
	(assert (and (>= y 0) (<= y bit)))
        (finitize-bit (op x y))))

    (define-syntax-rule (bvbit op)
      (lambda (a b)
        (if (and (>= b 0) (< b bit))
            (finitize-bit (op a (shl 1 b)))
            a)))

    (define bvadd (bvop +))
    (define bvsub (bvop -))
    (define bvrsub (bvop (lambda (x y) (- y x))))

    (define bvnot (lambda (x) (finitize-bit (bitwise-not x))))
    (define bvand (bvop bitwise-and))
    (define bvor  (bvop bitwise-ior))
    (define bvxor (bvop bitwise-xor))
    (define bvandn (lambda (x y) (finitize-bit (bitwise-and x (bitwise-not y)))))
    (define bviorn  (lambda (x y) (finitize-bit (bitwise-ior x (bitwise-not y)))))

    (define bvrev (bvuop rev))
    (define bvrev16 (bvuop rev16))
    (define bvrevsh (bvuop revsh))
    (define bvrbit (bvuop rbit))

    (define bvshl  (bvshift shl))
    (define bvshr  (bvshift >>))
    (define bvushr (bvshift ushr))
    (define bvror  (bvshift ror))

    (define bvshl#  (bvshift# shl))
    (define bvshr#  (bvshift# >>))
    (define bvushr# (bvshift# ushr))
    (define bvror#  (bvshift# ror))

    (define uxtah (bvop (lambda (x y) (+ x (bitwise-and y low-mask)))))
    (define uxth (lambda (x) (finitize-bit (bitwise-and x low-mask))))
    (define uxtb (lambda (x) (finitize-bit (bitwise-and x byte-mask))))

    (define bvmul (bvop *))
    (define bvmla (lambda (a b c) (finitize-bit (+ c (* a b)))))
    (define bvmls (lambda (a b c) (finitize-bit (- c (* a b)))))
    (define bvsmmla (lambda (a b c) (finitize-bit (+ c (bvsmmul a b)))))
    (define bvsmmls (lambda (a b c) (finitize-bit (- c (bvsmmul a b)))))

    (define (bvsmmul x y) (smmul x y bit))
    (define (bvummul x y) (ummul x y bit))
    (define bvsdiv (bvop quotient))
    (define (bvudiv n d)
      (if (< d 0)
          (if (< n d) 1 0)
          (let* ([q (shl (quotient (ushr n 2) d) 2)]
                 [r (- n (* q d))])
            (finitize-bit (if (or (> r d) (< r 0)) q (add1 q))))))
      

    (define (movlo to c)
      (finitize-bit (bitwise-ior (bitwise-and to high-mask) c)))
    
    (define (movhi to c)
      (finitize-bit (bitwise-ior (bitwise-and to low-mask) (shl c byte2))))

    (define (setbit d a width shift)
      (assert (and (>= shift 0) (< shift bit)))
      (assert (and (> width 0) (<= (+ width shift) bit)))
      (let* ([mask (sub1 (shl 1 width))]
             [keep (bitwise-and d (bitwise-not (shl mask shift)))]
             [insert (bvshl# (bitwise-and a mask) shift)])
        (finitize-bit (bitwise-ior keep insert))))

    (define (clrbit d width shift)
      (assert (and (>= shift 0) (< shift bit)))
      (assert (and (> width 0) (<= (+ width shift) bit)))
      (let* ([keep (bitwise-not (shl (sub1 (shl 1 width)) shift))])
        (finitize-bit (bitwise-and keep d))))

    (define (ext d a width shift)
      (assert (and (>= shift 0) (< shift bit)))
      (assert (and (> width 0) (<= (+ width shift) bit)))
      (finitize-bit (bitwise-and (>> a shift) (sub1 (shl 1 width)))))

    (define (sext d a width shift)
      (assert (and (>= shift 0) (< shift bit)))
      (assert (and (> width 0) (<= (+ width shift) bit)))
      (let ([keep (bitwise-and (>> a shift) (sub1 (shl 1 width)))])
        (bitwise-ior
         (if (= (bitwise-bit-field keep (sub1 width) width) 1)
             (shl -1 width)
             0)
         (finitize-bit keep))))

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

    (define (rev a)
      (bitwise-ior 
       (shl (bitwise-bit-field a byte0 byte1) byte3)
       (shl (bitwise-bit-field a byte1 byte2) byte2)
       (shl (bitwise-bit-field a byte2 byte3) byte1)
       (bitwise-bit-field a byte3 byte4)))

    (define (rev16 a)
      (bitwise-ior 
       (shl (bitwise-bit-field a byte2 byte3) byte3)
       (shl (bitwise-bit-field a byte3 byte4) byte2)
       (shl (bitwise-bit-field a byte0 byte1) byte1)
       (bitwise-bit-field a byte1 byte2)))

    (define (revsh a)
      (bitwise-ior 
       (if (= (bitwise-bit-field a (sub1 byte1) byte1) 1) high-mask 0)
       (shl (bitwise-bit-field a byte0 byte1) byte1)
       (bitwise-bit-field a byte1 byte2)))

    (define (rbit a)
      (let ([res 0])
        (for ([i bit])
             (set! res (bitwise-ior (shl res 1) (bitwise-and a 1)))
             (set! a (>> a 1)))
        res))

    ;; Flag values
    ;; 0 - eq
    ;; 1 - neq
    ;; 2 - x < y same sign
    ;; 3 - x > y same sign
    ;; 4 - x < 0
    ;; 5 - y < 0
    (define (tst x y) (if (= (bitwise-and x y) 0) 0 1))
    (define (cmp x y)
      (define my-x (finitize-bit x))
      (define my-y (finitize-bit y))
      ;;(pretty-display `(cmp ,my-x ,my-y))
      (cond
       [(= my-x my-y) 0]
       [(or (and (>= my-x 0) (>= my-y 0))
	    (and (< my-x 0) (< my-y 0)))
	(cond
	 [(< my-x my-y) 2]
	 [else 3])]
       [(< my-x 0) 4]
       [else 5]))
    
    ;; Interpret a given program from a given state.
    ;; state: initial progstate
    (define (interpret program state [ref #f])
      (define opcode-pool (get-field opcode-pool machine))
      ;;(pretty-display `(interpret))
      (define regs (vector-copy (progstate-regs state)))
      (define memory #f)
      (define z (progstate-z state))

      (define (interpret-step step)
        (define ops-vec (inst-op step))
        (define args (inst-args step))
        
        (define op (vector-ref ops-vec 0))
        (define cond-type (vector-ref ops-vec 1))
        (define shfop (vector-ref ops-vec 2))
        (define op-name (vector-ref base-opcodes op))
        (define shfop-name (and (>= shfop 0) (vector-ref shf-opcodes shfop)))
        
        ;; (pretty-display `(interpret-step ,ops-vec))

        (define-syntax-rule (inst-eq a ...)
          (or (equal? a op-name) ...))
        (define-syntax-rule (shf-inst-eq a ...)
          (or (equal? a shfop-name) ...))

        (define-syntax-rule (args-ref args i) (vector-ref args i))

        (define (exec)
          (define (opt-shift x)
            (define len (vector-length args))
            (define k (and (> len 0) (vector-ref args (sub1 len))))

            (define val #f)
            (define (rr f)
              (set! val (f (vector-ref regs x) (vector-ref regs k))))

            (define (ri f)
              (set! val (f (vector-ref regs x) k)))

            (cond
             [(or (equal? shfop #f) (equal? shfop -1))
              (set! val (vector-ref regs x))]
             [(shf-inst-eq `lsr) (rr bvushr)]
             [(shf-inst-eq `asr) (rr bvshr)]
             [(shf-inst-eq `lsl) (rr bvshl)]
             [(shf-inst-eq `ror) (rr bvror)]
             
             ;; shift i
             [(shf-inst-eq `lsr#) (ri bvushr#)]
             [(shf-inst-eq `asr#) (ri bvshr#)]
             [(shf-inst-eq `lsl#) (ri bvshl#)]
             [(shf-inst-eq `ror#) (ri bvror#)]
             [else
              (assert #f (format "undefine optional shift: ~a" shfop))]
             )
            val
            )

          ;; sub add
          (define (rrr f [shf #f])
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define b (args-ref args 2))
            (define reg-b-val 
	      (if shf
		  (opt-shift b)
		  (vector-ref regs b)))

            (define val (f (vector-ref regs a) reg-b-val))
            (vector-set! regs d val))

          (define (rrrr f)
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define b (args-ref args 2))
            (define c (args-ref args 3))
            (define val (f (vector-ref regs a) (vector-ref regs b) (vector-ref regs c)))
            (vector-set! regs d val))

          (define (ddrr f-lo f-hi)
            (define d-lo (args-ref args 0))
            (define d-hi (args-ref args 1))
            (assert (not (= d-lo d-hi)))
            (define a (args-ref args 2))
            (define b (args-ref args 3))
            (define val-lo (f-lo (vector-ref regs a) (vector-ref regs b)))
            (define val-hi (f-hi (vector-ref regs a) (vector-ref regs b)))
            (vector-set! regs d-lo val-lo)
            (vector-set! regs d-hi val-hi))

          ;; count leading zeros
          (define (rr f [shf #f])
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define reg-a-val 
	      (if shf
		  (opt-shift a)
		  (vector-ref regs a)))
            (define val (f reg-a-val))
            (vector-set! regs d val))

          ;; mov
          (define (ri f)
            (define d (args-ref args 0))
            (define a (check-imm-mov (args-ref args 1)))
            (define val (f a))
            (vector-set! regs d val))

          ;; movhi movlo
          (define (r!i f)
            (define d (args-ref args 0))
            (define a (check-imm-mov (args-ref args 1)))
            (define val (f (vector-ref regs d) a))
            (vector-set! regs d val))

          ;; subi addi
          (define (rri f)
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define b (check-imm (args-ref args 2)))
            (define val (f (vector-ref regs a) b))
            (vector-set! regs d val))

          ;; lsr
          (define (rrb f)
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define b (args-ref args 2))
            (define val (f (vector-ref regs a) b))
            (vector-set! regs d val))

          ;; store
          (define (str reg-offset)
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define b (args-ref args 2))
            (define index 
              (if reg-offset
                  (+ (vector-ref regs a) (vector-ref regs b))
                  (+ (vector-ref regs a) b)))
            (define val (vector-ref regs d))
            (unless memory
              (set! memory (send* (progstate-memory state) clone
                                 (and ref (progstate-memory ref)))))
            (send* memory store (finitize-bit index) val))

          ;; load
          (define (ldr reg-offset)
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define b (args-ref args 2))
            (define index 
              (if reg-offset
                  (+ (vector-ref regs a) (vector-ref regs b))
                  (+ (vector-ref regs a) b)))
            (unless memory
              (set! memory (send* (progstate-memory state) clone
                                 (and ref (progstate-memory ref)))))
            (define val (send* memory load (finitize-bit index)))
            (vector-set! regs d val))

          ;; setbit
          (define (rrbb f)
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define width (args-ref args 3))
            (define shift (args-ref args 2))
            (define val (f (vector-ref regs d) (vector-ref regs a) width shift))
            (vector-set! regs d val))

          ;; clrbit
          (define (r!bb f)
            (define d (args-ref args 0))
            (define width (args-ref args 2))
            (define shift (args-ref args 1))
            (define val (f (vector-ref regs d) width shift))
            (vector-set! regs d val))

          (define (z=rr f)
            (define a (args-ref args 0))
            (define b (args-ref args 1))
	    (set! z (f (vector-ref regs a) (vector-ref regs b))))

          (define (z=ri f)
            (define a (args-ref args 0))
            (define b (check-imm (args-ref args 1)))
	    (set! z (f (vector-ref regs a) b)))

          (cond
           ;; basic
           [(inst-eq `nop) (void)]
           [(inst-eq `add) (rrr bvadd #t)]
           [(inst-eq `sub) (rrr bvsub #t)]
           [(inst-eq `rsb) (rrr bvrsub #t)]

           [(inst-eq `and) (rrr bitwise-and #t)]
           [(inst-eq `orr) (rrr bitwise-ior #t)]
           [(inst-eq `eor) (rrr bitwise-xor #t)]
           [(inst-eq `bic) (rrr bvandn #t)]
           [(inst-eq `orn) (rrr bviorn #t)]

           ;; basic i
           [(inst-eq `add#) (rri bvadd)]
           [(inst-eq `sub#) (rri bvsub)]
           [(inst-eq `rsb#) (rri bvrsub)]

           [(inst-eq `and#) (rri bitwise-and)]
           [(inst-eq `orr#) (rri bitwise-ior)]
           [(inst-eq `eor#) (rri bitwise-xor)]
           [(inst-eq `bic#) (rri bvandn)]
           [(inst-eq `orn#) (rri bviorn)]
           
           ;; move
           [(inst-eq `mov) (rr identity #t)]
           [(inst-eq `mvn) (rr bvnot #t)]
           
           ;; move i
           [(inst-eq `mov#) (ri identity)]
           [(inst-eq `mvn#) (ri bvnot)]
           [(inst-eq `movt#) (r!i movhi)]
           [(inst-eq `movw#) (r!i movlo)]

           ;; reverse
           [(inst-eq `rev)   (rr bvrev)]
           [(inst-eq `rev16) (rr bvrev16)]
           [(inst-eq `revsh) (rr bvrevsh)]
           [(inst-eq `rbit)  (rr bvrbit)]

           ;; div & mul
           [(inst-eq `mul)  (rrr bvmul)]
           [(inst-eq `mla)  (rrrr bvmla)]
           [(inst-eq `mls)  (rrrr bvmls)]

           [(inst-eq `smmul) (rrr bvsmmul)]
           [(inst-eq `smmla) (rrrr bvsmmla)]
           [(inst-eq `smmls) (rrrr bvsmmls)]

           [(inst-eq `smull) (ddrr bvmul bvsmmul)]
           [(inst-eq `umull) (ddrr bvmul bvummul)]

           [(inst-eq `sdiv) (rrr bvsdiv)]
           [(inst-eq `udiv) (rrr bvudiv)]

           [(inst-eq `uxtah) (rrr uxtah)]
           [(inst-eq `uxth) (rr uxth)]
           [(inst-eq `uxtb) (rr uxtb)]
           
           ;; shift Rd, Rm, Rs
           ;; only the least significant byte of Rs is used.
           [(inst-eq `lsr) (rrr bvushr)]
           [(inst-eq `asr) (rrr bvshr)]
           [(inst-eq `lsl) (rrr bvshl)]
           [(inst-eq `ror) (rrr bvror)]
           
           ;; shift i
           [(inst-eq `lsr#) (rrb bvushr#)]
           [(inst-eq `asr#) (rrb bvshr#)]
           [(inst-eq `lsl#) (rrb bvshl#)]
           [(inst-eq `ror#) (rrb bvror#)]

           ;; bit
           [(inst-eq `bfc)  (r!bb  clrbit)]
           [(inst-eq `bfi)  (rrbb setbit)]

           ;; others
           [(inst-eq `sbfx) (rrbb sext)]
           [(inst-eq `ubfx) (rrbb ext)]
           [(inst-eq `clz)  (rr clz)]

           ;; load/store
           [(inst-eq `ldr#) (ldr #f)]
           [(inst-eq `str#) (str #f)]
           [(inst-eq `ldr)  (ldr #t)]
           [(inst-eq `str)  (str #t)]

           ;; compare
           [(inst-eq `tst) (z=rr tst)]
           [(inst-eq `cmp) (z=rr cmp)]

           [(inst-eq `tst#) (z=ri tst)]
           [(inst-eq `cmp#) (z=ri cmp)]

           [else (assert #f "undefine instruction")]
           ))

	;; z: eq 0, ne 1, < 1, > 3
	;; cond: eq 0, ne 1, ls 2, hi 3, cc 4, cs 5
	(define-syntax-rule (assert-op) (assert (and (>= op 0) (< op ninsts))))
        (cond
         [(or (equal? z -1) (equal? cond-type -1)
              (inst-eq `tst `cmp `tst# `cmp#))
	  (assert (and (>= cond-type -1) (< cond-type (vector-length cond-opcodes))))
          (exec)]

	 [(equal? cond-type 0) ;; eq
	  (if (equal? z 0) (exec) (assert-op))]

	 [(equal? cond-type 1) ;; ne
	  (if (member z (list 1 2 3 4 5)) (exec) (assert-op))]

	 [(equal? cond-type 2) ;; ls (unsigned lower or same)
	  (if (member z (list 0 2 5)) (exec) (assert-op))]

	 [(equal? cond-type 3) ;; hi (unsigned higher)
	  (if (member z (list 3 4)) (exec) (assert-op))]

	 [(equal? cond-type 4) ;; cc/lo (unsigned lower)
	  (if (member z (list 2 5)) (exec) (assert-op))]

	 [(equal? cond-type 5) ;; cs/hs (unsigned higher or same)
	  (if (member z (list 0 3 4)) (exec) (assert-op))]

	 [(equal? cond-type 6) ;; lt (signed less than)
	  (if (member z (list 2 4)) (exec) (assert-op))]

	 [(equal? cond-type 7) ;; ge (signed greater than or equal)
	  (if (member z (list 0 3 5)) (exec) (assert-op))]
	 
         [else (assert #f (format "illegal cond-type ~a" cond-type))]
         )        
        (assert (and (>= shfop -1) (< shfop (vector-length shf-opcodes))))
        )

      (for ([x program])
           (interpret-step x))
      
      (progstate regs (or memory (progstate-memory state)) z))

    (define (performance-cost code)
      (define cost 0)
      (define-syntax-rule (add-cost x) (set! cost (+ cost x)))
      (for ([x code])
           (let* ([ops-vec (inst-op x)]
                  [op (vector-ref ops-vec 0)]
                  [shfop (vector-ref ops-vec 2)]
                  [op-name (vector-ref base-opcodes op)]
                  [shfop-name (and (>= shfop 0) (vector-ref shf-opcodes shfop))]
                  )

             (define-syntax-rule (inst-eq a ...)
               (or (equal? a op-name) ...))
             (define-syntax-rule (shf-inst-eq a ...)
               (or (equal? a shfop-name) ...))

             (cond
              [(inst-eq `nop) (void)]
              [(inst-eq `str `str# `ldr `ldr#) (add-cost 3)]
              [(inst-eq `mul `mla `mls `smmul `smmla `smmls) (add-cost 5)]
              [(inst-eq `smull `umull `sdiv `udiv) (add-cost 6)]
              [(inst-eq `tst `cmp `tst# `cmp#) (add-cost 2)]
              [(inst-eq `sbfx `ubfx `bfc `bfi) (add-cost 2)]
              ;; [(inst-eq `mov) 
              ;;  (cond
              ;;   [(shf-inst-eq `lsr `asr `lsl `ror) (add-cost 2)]
              ;;   [else (add-cost 1)])]

              ;; [(shf-inst-eq `lsr# `asr# `lsl# `ror#) (add-cost 2)]
              
              [(and (inst-eq `add `sub `rsb `and `orr `eor `bic `orn `mov `mvn)
                    (shf-inst-eq `lsr `asr `lsl `ror))
               (add-cost 2)]

              [else (add-cost 1)])
             ))
      (when debug (pretty-display `(performance ,cost)))
      cost)

    (define legal-imm 
      (append (for/list ([i 12]) (arithmetic-shift #xff (* 2 i)))
              (list #xff000000 (- #xff000000))))

    (define-syntax-rule (check-imm x) 
      (assert-return 
       (ormap (lambda (i) (= (bitwise-and x i) (bitwise-and x mask))) legal-imm) 
       "illegal immediate"
       x))

    (define-syntax-rule (check-imm-mov x) 
      (assert-return 
       (or (= (bitwise-and x #xffff) x)
           (ormap (lambda (i) (= (bitwise-and x i) (bitwise-and x mask)))
                  legal-imm))
       "illegal mov immediate"
       x))

    ))
  
