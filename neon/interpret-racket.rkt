#lang racket

(require "../ast.rkt" "../ops-racket.rkt"
         "machine.rkt")

(provide interpret performance-cost)

(define-syntax-rule (byte-guard byte lam)
  (cond
   [(= byte 1) (lam 1)]
   [(= byte 2) (lam 2)]
   [(= byte 4) (lam 4)]
   [(= byte 8) (lam 8)]
   [else (assert #f "Invalid number of bytes")]))

;; vector -> number
(define-syntax-rule (bytes->number vec)
  (foldr (lambda (x res) (+ (<< res 8) x)) 0 (vector->list vec)))

;; number -> list
(define-syntax-rule (number->bytes num len)
  (for/list ([i len])
    (let ([x (bitwise-and num #xff)])
      (set! num (>> num 8))
      x)))

;; TODO: fix this
;; vector to list
(define (bytes->elements vec byte)
  ;;(pretty-display `(bytes->elements ,byte ,(vector-length vec)))
  (for/list ([i (quotient (vector-length vec) byte)])
            (bytes->element vec byte i)))

;; TODO: might need fix
(define-syntax-rule (bytes->element vec byte i)
  (bytes->number (vector-copy-len vec (* i byte) byte)))

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
    ;(pretty-display `(interpret-step ,(vector-ref inst-id op)))

    ;; (define dest-byte byte)
    ;; (define 1-byte byte)
    (define d-byte identity)
    (define n-byte identity)
    (define adjust (lambda (x bytes) x))

    (define (sign-extend val byte)
      ;(pretty-display `(sign-extend ,type ,(vector-member `u type-id) ,val))
      (if (= type (vector-member `u type-id))
          val
          (finitize val (* 8 byte))))
    
    (define (saturate val)
      (cond
       [(= type (vector-member `s type-id))
        (define bound (arithmetic-shift 1 (sub1 (* (d-byte byte) 8))))
        (if (>= val bound) 
            (sub1 bound)
            (if (< val (- bound)) 
                (- bound)
                val))
        ]
       [(= type (vector-member `u type-id))
        (define bound (<< 1 (* (d-byte byte) 8)))
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
      (set! d-byte (lambda (x) (* x 2)))
      (set! adjust sign-extend))

    (define (narrow)
      (set! d-byte (lambda (x) (quotient x 2)))
      (set! adjust sign-extend)
      )

    (define (wide)
      (set! d-byte (lambda (x) (* x 2)))
      (set! n-byte (lambda (x) (* x 2)))
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
          (vector-copy! dregs base val)))
      ;;(pretty-display `(set-qreg! end))
      )

    ;; Load
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

    (define (load stride update)
      (define dest (vector-ref args 0))
      (define n (car dest))
      (define dest-regs (cdr dest)) ;; TODO: check validity
      (define r-id (vector-ref args 1))
      (define mem-addr (vector-ref rregs r-id))
      (define skip (quotient n stride))
      
      (byte-guard 
       byte 
       (lambda (byte)
         (set! mem-addr (load-unit dest-regs 0 skip stride mem-addr byte))
         (when (< stride n) 
               (set! mem-addr (load-unit dest-regs 1 skip stride mem-addr byte)))
         (when (< (* 2 stride) n) 
               (set! mem-addr (load-unit dest-regs 2 skip stride mem-addr byte)))
         (when (< (* 3 stride) n) 
               (set! mem-addr (load-unit dest-regs 3 skip stride mem-addr byte)))))

      (when update (vector-set! rregs r-id mem-addr)))

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
      ;;(pretty-display `(nnn ,f ,arg-type ,d ,n ,m))
      (cond
       [(= arg-type 0) ;; normal
        ;;(pretty-display "TYPE 0000000000000000000000000000")
        (cond 
         [(and (is-d? d) (is-d? n) (is-d? m))
          (let ([vn (get-dreg n)]
                [vm (get-dreg m)]
                [vd (and rmw (get-dreg d))])
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
        ]
       [(= arg-type 1) ;; long
        ;;(pretty-display "TYPE 1111111111111111111111111111")
        (long)
        (if (and (is-q? d) (is-d? n) (is-d? m))
            (let ([vn (get-dreg n)]
                  [vm (get-dreg m)]
                  [vd (and rmw (get-qreg d))])
              (set-qreg! d (f vd vn vm)))
            (assert #f "Long: operands mismatch."))]
       [(= arg-type 2) ;; narrow
        ;;(pretty-display "TYPE 222222222222222222222222222")
        (narrow)
        (if (and (is-d? d) (is-q? n) (is-q? m))
            (let ([vn (get-qreg n)]
                  [vm (get-qreg m)]
                  [vd (and rmw (get-dreg d))])
              (set-dreg! d (f vd vn vm)))
            (assert #f "Long: operands mismatch."))]
       [(= arg-type 3) ;; wide
        ;;(pretty-display "TYPE 333333333333333333333333")
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
      ;(pretty-display `(ext ,vn ,vm))
      (define imm (vector-ref args 3))
      (define shift (* imm byte))
      (vector-extract vn vm shift))

    (define (x:notype vn g)
      (for/vector ([num-n vn]) (g num-n)))

    (define (xn vn g)
      (byte-guard
       byte
       (lambda (byte)
         (define en (bytes->elements vn byte))
         (define res (for/list ([num-n en]) 
                               (g (adjust num-n (n-byte byte)))))
         (elements->bytes res (d-byte byte)))))

    (define (x-x:notype vn vm f)
      (for/vector ([num-n vn]
                   [num-m vm])
                  (f num-n num-m)))

    (define (xd-xn-xm-i vd vn vm f)
      ;(pretty-display `(xd-xn-xm-i))
      (byte-guard
       byte
       (lambda (byte)
         (define ed (bytes->elements vd (d-byte byte))) ;; list
         (define en (bytes->elements vn (n-byte byte)))
         (define index (vector-ref args 3))
         (assert (< (* byte index) (vector-length vm)))
         (define num-m (bytes->element vm byte index))
         (define res
           (for/list ([num-d ed]
                      [num-n en])
                     (f num-d (adjust num-n (n-byte byte)) (adjust num-m byte))))
         ;(pretty-display `(xd-xn-xm-i-res ,res))
         (elements->bytes res (d-byte byte)))))

    (define (xd-xn-xm vd vn vm f)
      (byte-guard
       byte
       (lambda (byte)
         (define ed (bytes->elements vd (d-byte byte))) ;; list
         (define en (bytes->elements vn (n-byte byte)))
         (define em (bytes->elements vm byte))
         (define res
           (for/list ([num-d ed]
                      [num-n en]
                      [num-m em])
                     (f num-d (adjust num-n (n-byte byte)) (adjust num-m byte))))
         (elements->bytes res (d-byte byte)))))

    (define (mla vd vn vm)
      (xd-xn-xm vd vn vm (lambda (d n m) (+ d (* n m)))))

    (define (mlai vd vn vm)
      (xd-xn-xm-i vd vn vm (lambda (d n m) (+ d (* n m)))))


    (define (mov-simple vd vn) (x:notype vn identity))
    (define (mvn-simple vd vn) (x:notype vn (lambda (x) (bitwise-xor x #xff))))
    (define (mov vd vn)   (xn vn identity))
    (define (qmov vd vn)  (xn vn saturate))

    (define (vand vd vn vm)  (x-x:notype vn vm bitwise-and))

    (define-syntax-rule (inst-eq? x) (= op (vector-member x inst-id)))
    (define-syntax-rule (type-eq? x) (= type (vector-member x inst-type)))

    (cond
     [(inst-eq? `nop) (void)]

     [(inst-eq? `vld1)   (load1 #f)]
     [(inst-eq? `vld1!)  (load1 #t)]
     [(inst-eq? `vld2)   (load 2 #f)]
     [(inst-eq? `vld2!)  (load 2 #t)]

     [(inst-eq? `vext#)  (nnn 0 ext)]
     [(inst-eq? `vmla)   (nnn 0 mla #t)]
     [(inst-eq? `vmla#)  (nnn 0 mlai #t)]
     [(inst-eq? `vmlal)  (nnn 1 mla #t)]
     [(inst-eq? `vmlal#) (nnn 1 mlai #t)]

     [(inst-eq? `vmov)   (nn 0 mov-simple)]
     [(inst-eq? `vmovi)  (ni 0 mov-simple)] ;; TODO: constraint on constant
     ;; [(inst-eq? `vmvn)   (nn 0 mvn-simple)]
     ;; [(inst-eq? `vmvni)  (ni 0 mvn-simple)] ;; TODO: constraint on constant
     ;; [(inst-eq? `vmovl)  (nn 1 mov)]
     ;; [(inst-eq? `vmovn)  (nn 2 mov)]
     ;; [(inst-eq? `vqmovn) (nn 2 qmov)]
     ;; ;[(inst-eq? `vqmovun) (nn 2 qmov)]

     [(inst-eq? `vand)   (nnn 0 vand)]
     [(inst-eq? `vandi)  (ni 0 vand #t)]
     

     ))
  
  (for ([x program])
       (interpret-step x))
  (progstate dregs rregs memory))

(define (performance-cost code)
  (define cost 0)
  (for ([x code])
    (unless (= (inst-op x) (vector-member `nop inst-id))
            (set! cost (add1 cost))))
  cost)