#lang racket

(require "../simulator.rkt" "../ops-racket.rkt" 
         "../ast.rkt" "arm-ast.rkt"
         "../machine.rkt" "arm-machine.rkt")
(provide arm-simulator-racket%)

(define arm-simulator-racket%
  (class simulator%
    (super-new)
    (init-field machine)
    (override interpret performance-cost)
        
    (define bit (get-field bit machine))
    (define nregs (send machine get-nregs))
    (define nmems (send machine get-nmems))
    (define nop-id (send machine get-inst-id `nop))
    (define inst-id (get-field inst-id machine))
    (define shf-inst-id (get-field shf-inst-id machine))
    (define ninsts (vector-length inst-id))
    (define n-shf-insts (vector-length shf-inst-id))

    (define (shl a b) (<< a b bit))
    (define (ushr a b) (>>> a b bit))
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
        (finitize-bit (op x (bitwise-and byte-mask y)))))

    (define-syntax-rule (bvbit op)
      (lambda (a b)
        (if (and (>= b 0) (< b bit))
            (finitize-bit (op a (shl 1 b)))
            a)))

    (define bvadd (bvop +))
    (define bvsub (bvop -))
    (define bvrsub (bvop (lambda (x y) (- y x))))

    (define bvnot (lambda (x) (bitwise-not (finitize-bit x))))
    (define bvand (bvop bitwise-and))
    (define bvor  (bvop bitwise-ior))
    (define bvxor (bvop bitwise-xor))
    (define bvandn (lambda (x y) (bvnot (bitwise-and x y))))
    (define bviorn  (lambda (x y) (bvnot (bitwise-ior x y))))

    (define bvrev (bvuop rev))
    (define bvrev16 (bvuop rev16))
    (define bvrevsh (bvuop revsh))
    (define bvrbit (bvuop rbit))

    (define bvshl  (bvshift shl))
    (define bvshr  (bvshift >>))
    (define bvushr (bvshift ushr))

    (define bvmul (bvop *))
    (define bvmla (lambda (a b c) (finitize-bit (+ c (* a b)))))
    (define bvmls (lambda (a b c) (finitize-bit (- c (* a b)))))

    (define (movlo to c)
      (finitize-bit (bitwise-ior (bitwise-and to high-mask) c)))
    
    (define (movhi to c)
      (finitize-bit (bitwise-ior (bitwise-and to low-mask) (shl c byte2))))

    (define (setbit d a width shift)
      (let* ([mask (sub1 (shl 1 width))]
             [keep (bitwise-and d (bitwise-not (shl mask shift)))]
             [insert (bvshl (bitwise-and a mask) shift)])
        (finitize-bit (bitwise-ior keep insert))))

    (define (clrbit d width shift)
      (let* ([keep (bitwise-not (shl (sub1 (shl 1 width)) shift))])
        (bitwise-and keep d)))

    (define (ext d a width shift)
      (bitwise-and (>> a shift) (sub1 (shl 1 width))))

    (define (sext d a width shift)
      (let ([keep (bitwise-and (>> a shift) (sub1 (shl 1 width)))])
	(bitwise-ior
	 (if (= (bitwise-bit-field keep (sub1 width) width) 1)
	     (shl -1 width)
	     0)
	 keep)))

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
    
    ;; Interpret a given program from a given state.
    ;; state: initial progstate
    ;; policy: a procedure that enforces a policy to speed up synthesis. Default to nothing.
    (define (interpret program state [is-candidate #f] #:dep [dep #f])
      ;;(pretty-display `(interpret))
      (define regs (vector-copy (progstate-regs state)))
      (define memory (vector-copy (progstate-memory state)))
      (define z 0)

      (define regs-dep (and dep (vector-copy (progstate-regs state))))
      (define memory-dep (and dep (vector-copy (progstate-memory state))))
      (define z-dep #f)
      (define inter (list))
      (define init-vals (append (list 0) (vector->list regs) (vector->list memory)))
      
      (define-syntax add-inter
        (syntax-rules ()
          ((add-inter x) (set! inter (cons x inter)))
          ((add-inter x y ...) (set! inter (append (list x y ...) inter)))))

      (define-syntax-rule (create-node val p)
	(create-node-ex val p init-vals))

      (define (interpret-step step)
        (define op (inst-op step))
        (define args (inst-args step))
        (define cond-type (arm-inst-cond step))
        (define shfop (arm-inst-shfop step))
        ;;(pretty-display `(interpret-step ,z ,op ,args ,cond-type))
        
        (define-syntax-rule (inst-eq x) (equal? op (vector-member x inst-id)))

        (define-syntax-rule (args-ref args i) (vector-ref args i))

        (define (exec cond-dep)
          (define (opt-shift x)
            (define op (arm-inst-shfop step))
            (define k (arm-inst-shfarg step))
            (define-syntax-rule (shf-inst-eq xx) 
              (equal? op (vector-member xx shf-inst-id)))

            (define val #f)
            (define val-dep #f)
            (define (rr f)
              (set! val (f (vector-ref regs x) (vector-ref regs k)))
              (if dep
                  (set! val-dep (create-node val (list (vector-ref regs-dep x)
                                                       (vector-ref regs-dep k))))
                  (add-inter val)))

            (define (ri f)
              (set! val (f (vector-ref regs x) k))
              (if dep
                  (set! val-dep (create-node val (list (vector-ref regs-dep x) k)))
                  (add-inter val)))
            (cond
             [(or (equal? op #f) (shf-inst-eq `nop))
              (set! val (vector-ref regs x))
              (when dep (set! val-dep (vector-ref regs-dep x)))]
             [(shf-inst-eq `lsr) (rr bvushr)]
             [(shf-inst-eq `asr) (rr bvshr)]
             [(shf-inst-eq `lsl) (rr bvshl)]
             
             ;; shift i
             [(shf-inst-eq `lsr#) (ri bvushr)]
             [(shf-inst-eq `asr#) (ri bvshr)]
             [(shf-inst-eq `lsl#) (ri bvshl)]
             [else
              (assert #f (format "undefine optional shift: ~a" op))]
             )
            (cons val val-dep)
            )

          ;; sub add
          (define (rrr f [shf #f])
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define b (args-ref args 2))
            (define reg-b-val-dep 
	      (if shf
		  (opt-shift b)
		  (cons (vector-ref regs b) (and dep (vector-ref regs-dep b)))))

            (define val (f (vector-ref regs a) (car reg-b-val-dep)))
            (vector-set! regs d val)
            (if dep
                (vector-set! regs-dep d (create-node val (list (vector-ref regs-dep a)
                                                               (cdr reg-b-val-dep)
                                                               cond-dep)))
                (add-inter val)))

          (define (rrrr f)
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define b (args-ref args 2))
            (define c (args-ref args 3))
            (define val (f (vector-ref regs a) (vector-ref regs b) (vector-ref regs c)))
            (vector-set! regs d val)
            (if dep
                (vector-set! regs-dep d (create-node val 
                                                     (list (vector-ref regs-dep a)
                                                           (vector-ref regs-dep b)
                                                           (vector-ref regs-dep c)
                                                           cond-dep)))
                (add-inter val)))

          ;; count leading zeros
          (define (rr f [shf #f])
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define reg-a-val-dep 
	      (if shf
		  (opt-shift a)
		  (cons (vector-ref regs a) (and dep (vector-ref regs-dep a)))))
            (define val (f (car reg-a-val-dep)))
            (vector-set! regs d val)
            (if dep
                (vector-set! regs-dep d (create-node val (list (cdr reg-a-val-dep)
                                                               cond-dep)))
                (add-inter val)))

          ;; mov
          (define (ri f)
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define val (f a))
            (vector-set! regs d val)
            (if dep
                (vector-set! regs-dep d (create-node val (list a cond-dep)))
                (add-inter val)))

          ;; movhi movlo
          (define (r!i f)
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define val (f (vector-ref regs d) a))
            (vector-set! regs d val)
            (if dep
                (vector-set! regs-dep d (create-node val (list a (vector-ref regs-dep d)
                                                               cond-dep)))
                (add-inter val)))

          ;; subi addi
          (define (rri f)
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define b (args-ref args 2))
            (define val (f (vector-ref regs a) b))
            (vector-set! regs d val)
            (if dep
                (vector-set! regs-dep d (create-node val (list b (vector-ref regs-dep a)
                                                               cond-dep)))
                (add-inter val)))

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
            (vector-set! memory index val)
            (if dep
                (let* ([val-dep (list (vector-ref regs-dep d))]
                       [index-dep 
                        (if reg-offset
                            (list (vector-ref regs-dep a) (vector-ref regs-dep b))
                            (list (vector-ref regs-dep a)))]) ;; TODO: no b?
                  (vector-set! memory-dep index 
                               (create-node #f (list (create-node val val-dep)
                                                     (create-node index index-dep)
                                                     cond-dep))))
                (add-inter index)))

          ;; load
          (define (ldr reg-offset)
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define b (args-ref args 2))
            (define index 
              (if reg-offset
                  (+ (vector-ref regs a) (vector-ref regs b))
                  (+ (vector-ref regs a) b)))
            (define val (vector-ref memory index))
            (vector-set! regs d val)
            (if dep
                (let* ([val-dep (list (vector-ref memory-dep index))]
                       [index-dep 
                        (if reg-offset
                            (list (vector-ref regs-dep a) (vector-ref regs-dep b))
                            (list (vector-ref regs-dep a)))]) ;; TODO: no b?
                  (vector-set! regs-dep d 
                               (create-node #f (list (create-node val val-dep)
                                                     (create-node index index-dep)
                                                     cond-dep))))
                (add-inter index)))

          ;; setbit
          (define (rrii f)
            (define d (args-ref args 0))
            (define a (args-ref args 1))
            (define width (args-ref args 3))
            (define shift (args-ref args 2))
            (define val (f (vector-ref regs d) (vector-ref regs a) width shift))
            (vector-set! regs d val)
            (if dep
                (vector-set! regs-dep d (create-node val (list (vector-ref regs-dep d)
                                                               (vector-ref regs-dep a)
                                                               cond-dep)))
                (add-inter val)))

          ;; clrbit
          (define (r!ii f)
            (define d (args-ref args 0))
            (define width (args-ref args 2))
            (define shift (args-ref args 1))
            (define val (f (vector-ref regs d) width shift))
            (vector-set! regs d val)
            (if dep
                (vector-set! regs-dep d (create-node val (list (vector-ref regs-dep d)
                                                               cond-dep)))
                (add-inter val)))

          (define (z=rr f)
            (define a (args-ref args 0))
            (define b (args-ref args 1))
            (if (= (f (vector-ref regs a) (vector-ref regs b)) 0)
                (set! z 1)
                (set! z 0))
            (when dep
                  (set! z-dep (create-node #f (list (vector-ref regs-dep a) 
                                                    (vector-ref regs-dep b))))))

          (define (z=ri f)
            (define a (args-ref args 0))
            (define b (args-ref args 1))
            (if (= (f (vector-ref regs a) b) 0)
                (set! z 1)
                (set! z 0))
            (when dep
                  (set! z-dep (create-node #f (list (vector-ref regs-dep a) 
                                                    (create-node b (list)))))))
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
           ;; [(inst-eq `sdiv) (rrr quotient)]
           ;; [(inst-eq `udiv) (rrr (lambda (x y) (quotient (bitwise-and x mask)
           ;;                                               (bitwise-and y mask))))]
           [(inst-eq `mul)  (rrr bvmul)]
           [(inst-eq `mla)  (rrrr bvmla)]
           [(inst-eq `mls)  (rrrr bvmls)]
           
           ;; shift Rd, Rm, Rs
           ;; only the least significant byte of Rs is used.
           [(inst-eq `lsr) (rrr bvushr)]
           [(inst-eq `asr) (rrr bvshr)]
           [(inst-eq `lsl) (rrr bvshl)]
           
           ;; shift i
           [(inst-eq `lsr#) (rri bvushr)]
           [(inst-eq `asr#) (rri bvshr)]
           [(inst-eq `lsl#) (rri bvshl)]

           ;; bit
           [(inst-eq `bfc)  (r!ii  clrbit)]
           [(inst-eq `bfi)  (rrii setbit)]

           ;; others
           [(inst-eq `sbfx) (rrii sext)]
           [(inst-eq `ubfx) (rrii ext)]
           [(inst-eq `clz)  (rr clz)]

           ;; load/store
           [(inst-eq `ldr#) (ldr #f)]
           [(inst-eq `str#) (str #f)]
           [(inst-eq `ldr)  (ldr #t)]
           [(inst-eq `str)  (str #t)]

           ;; compare
           [(inst-eq `tst) (z=rr bitwise-and)]
           [(inst-eq `cmp) (z=rr -)]

           [(inst-eq `tst#) (z=ri bitwise-and)]
           [(inst-eq `cmp#) (z=ri -)]

           [else (assert #f "undefine instruction")]))

        (cond
         [(equal? cond-type -1) (exec #f)]
         [(equal? cond-type z) (exec z-dep)]
         [else (assert (and (or (equal? cond-type #f) (= cond-type 0) (= cond-type 1))
                            (>= op 0) (< op ninsts)))]
         )        
        (assert (or (equal? shfop #f) (and (>= shfop 0) (< shfop n-shf-insts))))
        )

      (for ([x program])
           (interpret-step x))
      ;; (cond
      ;;  [dep
      ;;   (pretty-display "regs-dep")
      ;;   (for ([i (vector-length regs-dep)])
      ;;        (pretty-display (format "----- reg[~a] -----" i))
      ;;        (display-node (vector-ref regs-dep i)))
      ;;   (pretty-display "memory-dep")
      ;;   (for ([i (vector-length memory-dep)])
      ;;        (pretty-display (format "----- mem[~a] -----" i))
      ;;        (display-node (vector-ref memory-dep i)))
      ;;   ]
      ;;  [else
      ;;   (pretty-display `(inter ,inter))])
      
      (progstate+ regs memory (if dep 
				  (progstate regs-dep memory-dep)
				  inter)))

    (define (performance-cost code)
      (define cost 0)
      (for ([x code])
           (cond
            [(equal? (inst-op x) (vector-member `nop inst-id)) (void)]
            [else (set! cost (add1 cost))])
           )
      (when debug (pretty-display `(performance ,cost)))
      cost)
    ))
