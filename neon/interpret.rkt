#lang s-exp rosette

(require "../ast.rkt" "../ops.rkt"
         "machine.rkt")

(provide interpret)

;; vector -> number
(define-syntax-rule (bytes->number vec)
  (foldr (lambda (x res) (+ (<< res 8) x)) 0 (vector->list vec)))

;; number -> list
(define-syntax-rule (number->bytes num len)
  (for/list ([i len])
    (let ([x (bitwise-and num #xff)])
      (set! num (>> num 8))
      x)))

;; vector to list
(define-syntax-rule (bytes->elements vec byte)
  (for/list ([i (quotient (vector-length vec) byte)])
    (bytes->element vec byte i)))

;; TODO: might need fix
(define-syntax-rule (bytes->element vec byte i)
  (bytes->number (vector-copy vec (* i byte) (* (add1 i) byte))))

;; list to vector
(define-syntax-rule (elements->bytes lst byte)
  (list->vector
   (flatten
    (for/list ([x lst]) (number->bytes x byte)))))


;; (define (finite-bit val byte type)
;;   (bitwise-and val (sub1 (arithmetic-shift 1 (* byte 8)))))

(define-syntax-rule (is-d? x) 
  (and (>= x 0) (< x nregs-d)))
  
(define-syntax-rule (is-q? x) 
  (and (>= x nregs-d) (< x (+ nregs-d (quotient nregs-d 2)))))

(define (interpret program state [policy #f])
  (define dregs (vector-copy (progstate-dregs state))) ;; 8-bit unit
  (define rregs (vector-copy (progstate-rregs state))) ;; 32-bit unit?
  (define memory (vector-copy (progstate-memory state))) ;; 8-bit unit

  (define (interpret-step x)
    (define op (inst-op x))
    (define byte (inst-byte x))
    (define type (inst-type x))
    (define args (inst-args x))

    (define dest-byte byte)
    (define 1-byte byte)
    (define adjust (lambda (x bytes) x))

    (define (sign-extend val byte)
      (if (= type (vector-member `u type-id))
          val
          (finitize val (* 8 byte))))
    
    (define (saturate val)
      (cond
       [(= type (vector-member `s type-id))
        (define bound (arithmetic-shift 1 (sub1 (* dest-byte 8))))
        (if (>= val bound) 
            (sub1 bound)
            (if (< val (- bound)) 
                (- bound)
                val))
        ]
       [(= type (vector-member `u type-id))
        (define bound (<< 1 (* dest-byte 8)))
        (if (>= val bound)
            (sub1 bound)
            (if (< val 0)
                0
                val))
        ]
       [else
        (assert #f (format "Saturate only works with type signed (s) or unsigned (u), but ~a is given." (vector-ref type-id type)))]
       ))

    (define (long)
      (set! dest-byte (* 2 byte))
      (set! adjust sign-extend))

    (define (narrow)
      (set! dest-byte (quotient byte 2))
      (set! adjust sign-extend)
      )

    (define (wide)
      (set! dest-byte (* 2 byte))
      (set! 1-byte (* 2 byte))
      (set! adjust sign-extend))
      

    ;; Access registers
    (define (get-dreg id)
      (let* ([bytes 8]
             [my-id id]
             [base (* my-id bytes)])
        (vector-copy-len dregs base bytes)))
    
    (define (get-qreg id)
      (define ret
      (let* ([bytes 16]
             [my-id (- id nregs-d)]
             [base (* my-id bytes)])
        (vector-copy-len dregs base bytes)))
      (pretty-display `(get-qreg return))
      ret)

    (define (set-dreg! id val)
      (let* ([bytes 8]
            [my-id id]
            [base (* my-id bytes)])
        (vector-copy! dregs base val)))

    (define (set-qreg! id val)
      (let ([bytes 16]
            [my-id (- id nregs-d)])
        (let ([base (* my-id bytes)])
          (vector-copy! dregs base val))))

    ;; Load
    (define (load-old stride update)
      (define dest (vector-ref args 0))
      (define n (car dest))
      (define dest-regs (cdr dest)) ;; TODO: check validity
      
      (define dest-index-list
        (for/list ([i (range n)])
                  (map (lambda (x) (+ x (* 8 (vector-ref dest-regs i))))
                       (range 8))))

      (define dest-index-stride (make-vector stride (list)))
      (for* ([iter (in-range (quotient n stride))]
             [i (in-range stride)])
            (let ([lst (car dest-index-list)])
              (set! dest-index-list (cdr dest-index-list))
              (vector-set! dest-index-stride i
                           (append (vector-ref dest-index-stride i) lst))))

      (define indexes (list))
      (for* ([iter (in-range (quotient (length (vector-ref dest-index-stride 0)) byte))]
             [lst-id (in-range stride)])
            (let ([lst (vector-ref dest-index-stride lst-id)])
              (set! indexes (append indexes (take lst byte)))
              (vector-set! dest-index-stride lst-id (drop lst byte))))

      (define r-id (vector-ref args 1))
      (define mem-addr (vector-ref rregs r-id)) ;; check alignment
      
      (for ([i (in-range (* n 8))]
            [reg-addr indexes]) ;; reverse indexes here
           (vector-set! dregs reg-addr (vector-ref memory (+ mem-addr i))))

      (when update (vector-set! rregs r-id (+ mem-addr (* n 8)))))

    (define (load-wrong stride update)
      (define dest (vector-ref args 0))
      (define n (car dest))
      (define dest-regs (cdr dest)) ;; TODO: check validity

      (define r-id (vector-ref args 1))
      (define mem-addr (vector-ref rregs r-id)) ;; check alignment
      
      (define indexes 
        (for/vector ([reg dest-regs] [i 4])
                    (and (< i n) (* reg 8))))
      
      (for ([stride-i (quotient 8 stride)])
           (for ([index indexes]
                 [i 4])
                (when index
                      (for ([iter stride])
                           (vector-set! dregs index (vector-ref memory mem-addr))
                           (set! mem-addr (add1 mem-addr))
                           (set! index (add1 index)))
                      (vector-set! indexes i index))))
      (when update (vector-set! rregs r-id mem-addr)))

    (define (load-unit dest-regs start skip stride mem-addr byte)
      (define indexes 
        (for/vector ([i stride]) (* 8 (vector-ref dest-regs (+ (* i skip) start)))))
      (for ([i (quotient 8 byte)])
           (for ([j stride]
                 [index indexes])
             (for ([k byte])
                  (vector-set! dregs index (vector-ref memory mem-addr))
                  (set! mem-addr (add1 mem-addr))
                  (set! index (add1 index)))
             (vector-set! indexes j index)))
      mem-addr)

    (define (load-byte stride update byte)
      (define dest (vector-ref args 0))
      (define n (car dest))
      (define dest-regs (cdr dest)) ;; TODO: check validity
      (define r-id (vector-ref args 1))
      (define mem-addr (vector-ref rregs r-id))
      (define skip (quotient n stride))
      
      (set! mem-addr (load-unit dest-regs 0 skip stride mem-addr byte))
      (when (< stride n) 
            (set! mem-addr (load-unit dest-regs 1 skip stride mem-addr byte)))
      (when (< (* 2 stride) n) 
            (set! mem-addr (load-unit dest-regs 2 skip stride mem-addr byte)))
      (when (< (* 3 stride) n) 
            (set! mem-addr (load-unit dest-regs 3 skip stride mem-addr byte)))

      (when update (vector-set! rregs r-id mem-addr)))

    (define (load stride update)
      (cond
       [(= byte 1) (load-byte stride update 1)]
       [(= byte 2) (load-byte stride update 2)]
       [(= byte 3) (load-byte stride update 3)]
       [(= byte 4) (load-byte stride update 4)]
       [(= byte 8) (load-byte stride update 8)]
       [else (assert #f "vld: invalid data type")]))

    ;; Special case for vld1 (no stride)
    (define (load1 update)
      (define dest (vector-ref args 0))
      (define n (car dest))
      (define dest-regs (cdr dest)) ;; TODO: check validity
      
      (define r-id (vector-ref args 1))
      (define mem-addr (vector-ref rregs r-id)) ;; check alignment

      (for ([i (in-range n)])
           (let ([d-reg (vector-ref dest-regs i)])
             (vector-copy! 
              dregs (* 8 d-reg) 
              memory (+ mem-addr (* 8 i)) (+ mem-addr (* 8 (add1 i))))))
      (when update (vector-set! rregs r-id (+ mem-addr (* n 8)))))
      
    ;; Operand patterns
    (define (nnn arg-type f [rmw #f])
      (define d (vector-ref args 0))
      (define n (vector-ref args 1))
      (define m (vector-ref args 2))
      (pretty-display `(nnn ,f ,arg-type ,d ,n ,m))
      (cond
       [(= arg-type 0) ;; normal
        (pretty-display "TYPE 0000000000000000000000000000")
        (cond 
         [(and (is-d? d) (is-d? n) (is-d? m))
          (let ([vn (get-dreg n)]
                [vm (get-dreg m)]
                [vd (and rmw (get-dreg d))])
            (pretty-display "TYPE 0: get regs")
            (set-dreg! d (f vd vn vm)))
          ]
         [(and (is-q? d) (is-q? n) (is-q? m))
          (let ([vn (get-qreg n)]
                [vm (get-qreg m)]
                [vd (and rmw (get-qreg d))])
              (set-qreg! d (f vd vn vm)))
          ]
         [else
          (assert #f "Normal: operands mismatch.")])
        ;(pretty-display `(here0 ,f))
        ]
       [(= arg-type 1) ;; long
        (pretty-display "TYPE 1111111111111111111111111111")
        (long)
        (if (and (is-q? d) (is-d? n) (is-d? m))
            (let ([vn (get-dreg n)]
                  [vm (get-dreg m)]
                  [vd (and rmw ((get-qreg d)))])
              (set-qreg! d (f vd vn vm)))
            (assert #f "Long: operands mismatch."))]
       [(= arg-type 2) ;; narrow
        (pretty-display "TYPE 222222222222222222222222222")
        (narrow)
        (if (and (is-d? d) (is-q? n) (is-q? m))
            (let ([vn (get-qreg n)]
                  [vm (get-qreg m)]
                  [vd (and rmw (get-dreg d))])
              (set-dreg! d (f vd vn vm)))
            (assert #f "Long: operands mismatch."))]
       [(= arg-type 3) ;; wide
        (pretty-display "TYPE 333333333333333333333333")
        (wide)
        (if (and (is-q? d) (is-q? n) (is-d? m))
            (let ([vn (get-qreg n)]
                  [vm (get-dreg m)]
                  [vd (and rmw (get-qreg d))])
              (set-qreg! d (f vd vn vm)))
            (assert #f "Wide: operands mismatch."))])
      )

    (define (nn arg-type f [rmw #f])
      (define d (vector-ref args 0))
      (define n (vector-ref args 1))
      (cond
       [(= arg-type 0) ;; normal
        (cond
         [(and (is-d? d) (is-d? n))
          (let ([vn (get-dreg n)]
                [vd (and rmw (get-dreg d))])
              (set-dreg! d (f vd vn)))]
         [(and (is-q? d) (is-q? n))
          (let ([vn (get-qreg n)]
                [vd (and rmw (get-qreg d))])
              (set-qreg! d (f vd vn)))]
         [else
          (assert #f "Normal: operands mismatch.")])]
       [(= arg-type 1) ;; long
        (long)
        (if (and (is-q? d) (is-d? n))
            (let ([vn (get-dreg n)]
                  [vd (and rmw (get-qreg d))])
              (set-qreg! d (f vd vn)))
            (assert #f "Long: operands mismatch."))]
       [(= arg-type 2) ;; narrow
        (narrow)
        (if (and (is-d? d) (is-q? n))
            (let ([vn (get-qreg n)]
                  [vd (and rmw (get-dreg d))])
              (set-dreg! d (f vd vn)))
            (assert #f "Narrow: operands mismatch."))]
       ))

    (define (ni arg-type f [rmw #f])
      (define d (vector-ref args 0))
      (define i (vector-ref args 1))
      (if (is-d? d)
          (let ([vd (and rmw (get-dreg d))])
            (set-dreg! d (f vd (number->bytes i 8))))
          (let ([vd (and rmw (get-qreg d))])
            (set-qreg! d (f vd (number->bytes i 16))))))

    ;; exti
    (define (ext vd vn vm)
      (pretty-display `(ext ,vn ,vm))
      (define imm (vector-ref args 3))
      (pretty-display `(imm ,imm))
      (define shift (* imm byte))
      (pretty-display `(shift ,shift))
      (pretty-display `(len ,(vector-length vn) ,(vector-length vm) ,shift))
      ;(define ret (vector-append (vector-drop vn shift) (vector-take vm shift)))
      (define ret (vector-extract vn vm shift))
      (pretty-display `(ext-ret ,ret))
      ret)

    (define (x:notype vn g)
      (for/vector ([num-n vn]) (g num-n)))

    (define (xn vn g)
      (define en (bytes->elements vn byte))
      (define res (for/list ([num-n en]) 
                            (g (adjust num-n 1-byte))))
      (elements->bytes res dest-byte))

    (define (x-x:notype vn vm f)
      (for/vector ([num-n vn]
                 [num-m vm])
                (f num-n num-m)))

    (define (xd-xn-xm-i vd vn vm f)
      (define ed (bytes->elements vd dest-byte)) ;; list
      (define en (bytes->elements vn byte))
      (define num-m (bytes->element vm byte (vector-ref args 3)))
      (define res
        (for/list ([num-d ed]
                   [num-n en])
                  (f num-d (adjust num-n 1-byte) (adjust num-m byte))))
      (elements->bytes res dest-byte))

    (define (xd-xn-xm vd vn vm f)
      (define ed (bytes->elements vd dest-byte)) ;; list
      (define en (bytes->elements vn byte))
      (define em (bytes->elements vm byte))
      (define res
        (for/list ([num-d ed]
                   [num-n en]
                   [num-m em])
                  (f num-d (adjust num-n 1-byte) (adjust num-m byte))))
      (elements->bytes res dest-byte))

    (define (mla vd vn vm)
      (let ([f (if (= (vector-length args) 3) 
                   xd-xn-xm
                   xd-xn-xm-i)])
        (f vd vn vm (lambda (d n m) (+ d (* n m))))))


    (define (mov-simple vd vn) (x:notype vn identity))
    (define (mvn-simple vd vn) (x:notype vn (lambda (x) (bitwise-xor x #xff))))
    (define (mov vd vn)   (xn vn identity))
    (define (qmov vd vn)  (xn vn saturate))

    (define (vand vd vn vm)  (x-x:notype vn vm bitwise-and))

    (define-syntax-rule (inst-eq? x) (= op (vector-member x inst-id)))
    (define-syntax-rule (type-eq? x) (= type (vector-member x inst-type)))

    (cond
     [(inst-eq? `vld1)  (pretty-display "vld1") (load1 #f)]
     [(inst-eq? `vld1!) (pretty-display "vld1!") (load1 #t)]
     [(inst-eq? `vld2)  (load 2 #f)]
     [(inst-eq? `vld2!) (load 2 #t)]

     [(inst-eq? `vexti) (pretty-display "vexti") (nnn 0 ext)]
     ;; [(inst-eq? `vmla)  (nnn 0 mla #t)]
     ;; [(inst-eq? `vmlal) (nnn 1 mla #t)]

     ;; [(inst-eq? `vmov)   (nn 0 mov-simple)]
     ;; [(inst-eq? `vmovi)  (ni 0 mov-simple)] ;; TODO: constraint on constant
     ;; [(inst-eq? `vmvn)   (nn 0 mvn-simple)]
     ;; [(inst-eq? `vmvni)  (ni 0 mvn-simple)] ;; TODO: constraint on constant
     ;; [(inst-eq? `vmovl)  (nn 1 mov)]
     ;; [(inst-eq? `vmovn)  (nn 2 mov)]
     ;; [(inst-eq? `vqmovn) (nn 2 qmov)]
     ;; ;[(inst-eq? `vqmovun) (nn 2 qmov)]

     ;; [(inst-eq? `vand)   (nnn 0 vand)]
     ;; [(inst-eq? `vandi)  (pretty-display "vandi") (ni 0 vand #t)]

     ))
  
  (for ([x program])
       (interpret-step x))
  (progstate dregs rregs memory 0))