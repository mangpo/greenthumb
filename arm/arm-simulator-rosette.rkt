#lang rosette

(require "../simulator.rkt" "../ops-rosette.rkt" 
         "../inst.rkt"
         "../machine.rkt" "arm-machine.rkt")
(provide arm-simulator-rosette%)

(define arm-simulator-rosette%
  (class simulator%
    (super-new)
    (init-field machine)
    (override interpret performance-cost get-constructor)

    (define (get-constructor) arm-simulator-rosette%)
        
    (define bit (get-field bitwidth machine))

    (define opcodes (get-field opcodes machine))
    (define base-opcodes (vector-ref opcodes 0))
    (define cond-opcodes (vector-ref opcodes 1))
    (define shf-opcodes (vector-ref opcodes 2))
    (define ninsts (vector-length base-opcodes))

    ;(define (shl a b) (<< a b bit))
    ;(define (ushr a b) (>>> a b bit))
    (define (ror a b) (bvor (bvlshr a b) (bvshl a (bvsub (bv bit bit) b))))

    (define byte0 0)
    (define byte1 (quotient bit 4))
    (define byte2 (* 2 byte1))
    (define byte3 (* 3 byte1))
    (define byte4 bit)
    
    (define byte-mask (bv (sub1 (arithmetic-shift 1 byte1)) bit))
    (define low-mask  (bv (sub1 (arithmetic-shift 1 byte2)) bit))
    (define high-mask (bvshl low-mask (bv byte2 bit)))
    (define mask      (bv (sub1 (arithmetic-shift 1 bit)) bit))

    ;; helper functions
    (define-syntax-rule (bool->num b) (if b 1 0))
    (define-syntax-rule (i->bv x) (integer->bitvector x (bitvector bit)))
    (define-syntax-rule (bv->i x) (bitvector->integer x))
    (define-syntax-rule (finitize-bit x) (bv->i (i->bv x)))

    (define-syntax-rule (binop op)
      (lambda (x y)
        (bv->i (op (i->bv x) (i->bv y)))))

    (define-syntax-rule (uop op)     
      (lambda (x)
        (bv->i (op (i->bv x)))))

    (define-syntax-rule (tinop op)
      (lambda (x y z)
        (bv->i
         (op (i->bv x) (i->bv y) (i->bv z)))))

    (define-syntax-rule (bvshift op)
      (lambda (x y)
        (bv->i
         (op (i->bv x) (bvand (bv #xff bit) (i->bv y))))))

    (define-syntax-rule (bvshift# op)
      (lambda (x y)
	;;(pretty-display `(bvshift assert ,y))
	(assert (and (>= y 0) (<= y bit)))
        (bv->i (op (i->bv x) (i->bv y)))))

    (define iadd (binop bvadd))
    (define isub (binop bvsub))
    (define irsub (binop (lambda (x y) (bvsub y x))))

    (define inot (uop bvnot))
    (define iand (binop bvand))
    (define ior  (binop bvor))
    (define ixor (binop bvxor))
    (define iandn (binop (lambda (x y) (bvand x (bvnot y)))))
    (define iorn (binop (lambda (x y) (bvor x (bvnot y)))))

    (define irev (uop rev))
    (define irev16 (uop rev16))
    (define irevsh (uop revsh))
    (define irbit (uop rbit))

    (define ishl  (bvshift bvshl))
    (define ishr  (bvshift bvashr))
    (define iushr (bvshift bvlshr))
    (define iror  (bvshift ror))

    (define ishl#  (bvshift# bvshl))
    (define ishr#  (bvshift# bvashr))
    (define iushr# (bvshift# bvlshr))
    (define iror#  (bvshift# ror))

    (define uxtah (binop (lambda (x y) (bvand x (bvand y low-mask)))))
    (define uxth (uop (lambda (x) (bvand x low-mask))))
    (define uxtb (uop (lambda (x) (bvand x byte-mask))))

    (define imul (binop bvmul))
    (define imla (tinop (lambda (a b c) (bvand c (bvmul a b)))))
    (define imls (tinop (lambda (a b c) (bvsub c (bvmul a b)))))
    (define ismmla (tinop (lambda (a b c) (bvadd c (smmul a b)))))
    (define ismmls (tinop (lambda (a b c) (bvsub c (smmul a b)))))

    (define ismmul (binop smmul))
    (define iummul (binop ummul))
    (define isdiv (binop bvsdiv))
    (define iudiv (binop bvudiv))

    (define movlo (binop (lambda (to c) (bvor (bvand to high-mask)
                                              (bvand c low-mask)))))
    (define movhi (binop (lambda (to c) (bvor (bvand to low-mask)
                                              (bvshl c (bv byte2 bit))))))

    (define (setbit d a width shift)
      (define d* (i->bv d))
      (define a* (i->bv a))
      (define width* (i->bv width))
      (define shift* (i->bv shift))
      (assert (and (>= shift 0) (< shift bit)))
      (assert (and (> width 0) (<= (+ width shift) bit)))
      (let* ([mask (bvsub (bvshl (bv 1 bit) width*) (bv 1 bit))]
             [keep (bvand d* (bvnot (bvshl mask shift*)))]
             [insert (bvshl (bvand a* mask) shift*)])
        (bv->i (bvor keep insert))))

    (define (clrbit d width shift)
      (define d* (i->bv d))
      (define width* (i->bv width))
      (define shift* (i->bv shift))
      (assert (and (>= shift 0) (< shift bit)))
      (assert (and (> width 0) (<= (+ width shift) bit)))
      (let* ([mask (bvsub (bvshl (bv 1 bit) width*) (bv 1 bit))]
             [keep (bvnot (bvshl mask shift*))])
        (bv->i (bvand keep d*))))

    (define (ext d a width shift)
      (define d* (i->bv d))
      (define a* (i->bv a))
      (define width* (i->bv width))
      (define shift* (i->bv shift))
      (assert (and (>= shift 0) (< shift bit)))
      (assert (and (> width 0) (<= (+ width shift) bit)))
      (bv->i (bvand (bvashr a* shift*)
                    (bvsub (bv 1 bit) (bvshl bv 1 bit) width*))))

    (define (sext d a width shift)
      (define d* (i->bv d))
      (define a* (i->bv a))
      (define width* (i->bv width))
      (define shift* (i->bv shift))
      (define com (bvsub (bv bit bit) width*))
      (assert (and (>= shift 0) (< shift bit)))
      (assert (and (> width 0) (<= (+ width shift) bit)))
      (let ([keep (bvand (bvashr a* shift*)
                         (bvsub (bv 1 bit) (bvshl bv 1 bit) width*))])
        (bv->i (bvashr (bvshl keep com) com))))

    (define (clz xx)
      (let ([x (i->bv xx)]
            [mask (i->bv (arithmetic-shift 1 (sub1 bit)))]
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

    (define (rev a)
      (concat
       (extract (sub1 byte1) byte0 a)
       (extract (sub1 byte2) byte1 a)
       (extract (sub1 byte3) byte2 a)
       (extract (sub1 byte4) byte3 a)))
    
    (define (rev16 a)
      (concat
       (extract (sub1 byte3) byte2 a)
       (extract (sub1 byte4) byte3 a)
       (extract (sub1 byte1) byte0 a)
       (extract (sub1 byte2) byte1 a)))

    (define (revsh a)
      (sign-extend
       (concat
        (extract (sub1 byte1) byte0 a)
        (extract (sub1 byte2) byte1 a))
       (bitvector bit)))

    (define (rbit aa)
      (let ([a (i->bv aa)]
            [res (bv 0 bit)])
        (for ([i bit])
             (set! res (bvor (bvshl res (bv 1 bit)) (bvand a (bv 1 bit))))
             (set! a (bvlshr a (bv 1))))
        res))

    ;; Flag values
    ;; 0 - eq
    ;; 1 - neq
    ;; 2 - x < y same sign
    ;; 3 - x > y same sign
    ;; 4 - x < 0
    ;; 5 - y < 0
    (define (tst x y) (if (bveq (bvand (i->bv x) (i->bv y)) 0) 0 1))
    (define (cmp x y)
      (define my-x x)
      (define my-y y)
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
        
        ;;(pretty-display `(interpret-step ,ops-vec ,op-name))

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
             [(shf-inst-eq `lsr) (rr iushr)]
             [(shf-inst-eq `asr) (rr ishr)]
             [(shf-inst-eq `lsl) (rr ishl)]
             [(shf-inst-eq `ror) (rr iror)]
             
             ;; shift i
             [(shf-inst-eq `lsr#) (ri iushr#)]
             [(shf-inst-eq `asr#) (ri ishr#)]
             [(shf-inst-eq `lsl#) (ri ishl#)]
             [(shf-inst-eq `ror#) (ri iror#)]
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
            (vector-set! regs d val)
            )

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
           [(inst-eq `add) (rrr iadd #t)]
           [(inst-eq `sub) (rrr isub #t)]
           [(inst-eq `rsb) (rrr irsub #t)]

           [(inst-eq `and) (rrr iand #t)]
           [(inst-eq `orr) (rrr ior #t)]
           [(inst-eq `eor) (rrr ixor #t)]
           [(inst-eq `bic) (rrr iandn #t)]
           [(inst-eq `orn) (rrr iorn #t)]

           ;; basic i
           [(inst-eq `add#) (rri iadd)]
           [(inst-eq `sub#) (rri isub)]
           [(inst-eq `rsb#) (rri irsub)]

           [(inst-eq `and#) (rri iand)]
           [(inst-eq `orr#) (rri ior)]
           [(inst-eq `eor#) (rri ixor)]
           [(inst-eq `bic#) (rri iandn)]
           [(inst-eq `orn#) (rri iorn)]
           
           ;; move
           [(inst-eq `mov) (rr identity #t)]
           [(inst-eq `mvn) (rr inot #t)]
           
           ;; move i
           [(inst-eq `mov#) (ri identity)]
           [(inst-eq `mvn#) (ri inot)]
           [(inst-eq `movt#) (r!i movhi)]
           [(inst-eq `movw#) (r!i movlo)]

           ;; reverse
           [(inst-eq `rev)   (rr irev)]
           [(inst-eq `rev16) (rr irev16)]
           [(inst-eq `revsh) (rr irevsh)]
           [(inst-eq `rbit)  (rr irbit)]

           ;; div & mul
           [(inst-eq `mul)  (rrr imul)]
           [(inst-eq `mla)  (rrrr imla)]
           [(inst-eq `mls)  (rrrr imls)]

           [(inst-eq `smmul) (rrr ismmul)]
           [(inst-eq `smmla) (rrrr ismmla)]
           [(inst-eq `smmls) (rrrr ismmls)]

           [(inst-eq `smull) (ddrr imul ismmul)]
           [(inst-eq `umull) (ddrr imul iummul)]

           [(inst-eq `sdiv) (rrr isdiv)]
           [(inst-eq `udiv) (rrr iudiv)]

           [(inst-eq `uxtah) (rrr uxtah)]
           [(inst-eq `uxth) (rr uxth)]
           [(inst-eq `uxtb) (rr uxtb)]
           
           ;; shift Rd, Rm, Rs
           ;; only the least significant byte of Rs is used.
           [(inst-eq `lsr) (rrr iushr)]
           [(inst-eq `asr) (rrr ishr)]
           [(inst-eq `lsl) (rrr ishl)]
           [(inst-eq `ror) (rrr iror)]
           
           ;; shift i
           [(inst-eq `lsr#) (rrb iushr#)]
           [(inst-eq `asr#) (rrb ishr#)]
           [(inst-eq `lsl#) (rrb ishl#)]
           [(inst-eq `ror#) (rrb iror#)]

           ;; bit
           [(inst-eq `bfc)  (r!bb clrbit)]
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
      (let ([xx (i->bv x)])
        (assert-return 
         (ormap (lambda (i) (bveq (bvand xx (bvand xx (bv i bit))) (bvand xx mask))) legal-imm) 
         "illegal immediate"
         x)))

    (define-syntax-rule (check-imm-mov x)
      (let ([xx (i->bv x)])
        (assert-return 
         (or (bveq (bvand xx (bv #xffff bit)) xx)
             (ormap (lambda (i) (bveq (bvand xx (bv i bit)) (bvand xx mask)))
                    legal-imm))
         "illegal mov immediate"
         x)))

    ))
  
