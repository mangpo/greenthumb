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

(define-syntax-rule (bytes->element vec byte i)
  (bytes->number (vector-copy vec (* i byte) (* (add1 i) byte))))

;; list to vector
(define-syntax-rule (elements->bytes lst byte)
  (list->vector
   (flatten
    (for/list ([x lst]) (number->bytes x byte)))))


;; (define (finite-bit val byte type)
;;   (bitwise-and val (sub1 (arithmetic-shift 1 (* byte 8)))))

  

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
    (define adjust identity)

    (define (sign-extend val)
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
        (raise (format "Saturate only works with type signed (s) or unsigned (u), but ~a is given." (vector-ref type-id type)))]
       ))

    (define (long)
      (set! dest-byte (* 2 byte))
      (set! adjust sign-extend))

    (define (narrow)
      (set! dest-byte (quotient byte 2))
      (set! adjust sign-extend)
      )

    ;; Access registers
    (define (get-dreg id)
      (let ([bytes (if (< id nregs-d) 8 16)]
            [my-id (if (< id nregs-d) id (- id nregs-d))])
        (let ([base (* my-id bytes)])
          (vector-copy dregs base (+ base bytes)))))

    (define (set-dreg! id val)
      (let ([bytes (if (< id nregs-d) 8 16)]
            [my-id (if (< id nregs-d) id (- id nregs-d))])
        (let ([base (* my-id bytes)])
          (vector-copy! dregs base val))))

    ;; Load
    (define (load stride update)
      (define dest (vector-ref args 0))
      (define n (car dest))
      (define dest-regs (cdr dest)) ;; TODO: check validity
      
      (define dest-index-list
        (for/list ([i n])
                  (map (lambda (x) (+ x (* 8 (vector-ref dest-regs i))))
                       (range 8))))

      (define dest-index-stride (make-vector stride (list)))
      (for* ([iter (quotient n stride)]
             [i stride])
            (let ([lst (car dest-index-list)])
              (set! dest-index-list (cdr dest-index-list))
              (vector-set! dest-index-stride i
                           (append (vector-ref dest-index-stride i) lst))))

      (define indexes (list))
      (for* ([iter (quotient (length (vector-ref dest-index-stride 0)) byte)]
             [lst-id stride])
            (let ([lst (vector-ref dest-index-stride lst-id)])
              (set! indexes (append indexes (take lst byte)))
              (vector-set! dest-index-stride lst-id (drop lst byte))))

      (define r-id (vector-ref args 1))
      (define mem-addr (vector-ref rregs r-id)) ;; check alignment
      
      (for ([i (* n 8)]
            [reg-addr indexes]) ;; reverse indexes here
           (vector-set! dregs reg-addr (vector-ref memory (+ mem-addr i))))

      (when update (vector-set! rregs r-id (+ mem-addr (* n 8)))))

    ;; Special case for vld1 (no stride)
    (define (load1 update)
      (define dest (vector-ref args 0))
      (define n (car dest))
      (define dest-regs (cdr dest)) ;; TODO: check validity
      
      (define r-id (vector-ref args 1))
      (define mem-addr (vector-ref rregs r-id)) ;; check alignment

      (for ([d-reg dest-regs]
            [i n])
           (vector-copy! 
            dregs (* 8 d-reg) 
            memory (+ mem-addr (* 8 i)) (+ mem-addr (* 8 (add1 i)))))
      (when update (vector-set! rregs r-id (+ mem-addr (* n 8)))))
      
    ;; Operand patterns
    (define (nnn arg-type f)
      (define d (vector-ref args 0))
      (define n (vector-ref args 1))
      (define m (vector-ref args 2))
      (cond
       [(= arg-type 0) ;; normal
        (unless (or (and (< d nregs-d) (< n nregs-d) (< m nregs-d))
                    (and (>= d nregs-d) (>= n nregs-d) (>= m nregs-d)))
                (raise "Normal: operands mismatch."))]
       [(= arg-type 1) ;; long
        (long)
        (unless (and (>= d nregs-d) (< n nregs-d) (< m nregs-d))
                (raise "Long: operands mismatch."))]
       [(= arg-type 2) ;; wide
        (long)
        (unless (and (>= d nregs-d) (>= n nregs-d) (< m nregs-d))
                (raise "Wide: operands mismatch."))])
       
      (define vn (get-dreg n))
      (define vm (get-dreg m))
      (set-dreg! d (f d vn vm)))

    (define (nn arg-type f)
      (define d (vector-ref args 0))
      (define n (vector-ref args 1))
      (cond
       [(= arg-type 0) ;; normal
        (unless (or (and (< d nregs-d) (< n nregs-d))
                    (and (>= d nregs-d) (>= n nregs-d)))
                (raise "Normal: operands mismatch."))]
       [(= arg-type 1) ;; long
        (long)
        (unless (and (>= d nregs-d) (< n nregs-d))
                (raise "Long: operands mismatch."))]
       [(= arg-type 2) ;; narrow
        (narrow)
        (unless (and (< d nregs-d) (>= n nregs-d))
                (raise "Narrow: operands mismatch."))]
       )
       
      (define vn (get-dreg n))
      (set-dreg! d (f d vn)))

    (define (ni arg-type f)
      (define d (vector-ref args 0))
      (define i (vector-ref args 1))
      (define vn (number->bytes i (if (< d nregs-d) 8 16)))
      (set-dreg! d (f d vn)))

    ;; exti
    (define (ext d vn vm)
      (define imm (vector-ref args 3))
      (define shift (* imm byte))
      (vector-append (vector-drop vn shift) (vector-take vm shift)))

    ;; mul, mla, mls
    (define (mul/mla/mls-scalar d vn vm f)
      (define ed (bytes->elements (get-dreg d) dest-byte)) ;; list
      (define en (bytes->elements vn byte))
      (define num-m (bytes->element vm byte (vector-ref args 3)))
      (define res
        (for/list ([num-d ed]
                   [num-n en])
                  (f num-d (adjust num-n) (adjust num-m))))
      (elements->bytes res dest-byte))

    (define (mul/mla/mls-vector d vn vm f)
      (define ed (bytes->elements (get-dreg d) dest-byte)) ;; list
      (define en (bytes->elements vn byte))
      (define em (bytes->elements vm byte))
      (define res
        (for/list ([num-d ed]
                   [num-n en]
                   [num-m em])
                  (f num-d (adjust num-n) (adjust num-m))))
      (elements->bytes res dest-byte))

    (define (mul/mla/mls d vn vm g)
      (let ([f (if (= (vector-length args) 3) 
                   mul/mla/mls-vector
                   mul/mla/mls-scalar)])
        (f d vn vm g)))

    (define (mla d vn vm)
      (mul/mla/mls d vn vm (lambda (d n m) (+ d (* n m)))))
    

    ;; mov, mvn
    (define (mov/mvn-simple d vn g)
      (for/vector ([num-n vn]) (g (adjust num-n))))

    (define (mov/mvn d vn g)
      (define en (bytes->elements vn byte))
      (define res (for/list ([num-n en]) 
                            (g (adjust num-n))))
      (elements->bytes res dest-byte))

    (define (mov-simple d vn) (mov/mvn-simple d vn identity))
    (define (mvn-simple d vn) (mov/mvn-simple d vn (lambda (x) (bitwise-xor x #xff))))
    (define (mov d vn)   (mov/mvn d vn identity))
    (define (qmov d vn)  (mov/mvn d vn saturate))

    (define-syntax-rule (inst-eq? x) (= op (vector-member x inst-id)))
    (define-syntax-rule (type-eq? x) (= type (vector-member x inst-type)))

    (cond
     [(inst-eq? `vld1)  (load1 #f)]
     [(inst-eq? `vld1!) (load1 #t)]
     [(inst-eq? `vld2)  (load 2 #f)]
     [(inst-eq? `vld2!) (load 2 #t)]

     [(inst-eq? `vexti) (nnn 0 ext)]
     [(inst-eq? `vmla)  (nnn 0 mla)]
     [(inst-eq? `vmlal) (long) (nnn 1 mla)]

     [(inst-eq? `vmov)   (nn 0 mov-simple)]
     [(inst-eq? `vmovi)  (ni 0 mov-simple)] ;; TODO: constraint on constant
     [(inst-eq? `vmvn)   (nn 0 mvn-simple)]
     [(inst-eq? `vmvni)  (ni 0 mvn-simple)] ;; TODO: constraint on constant
     [(inst-eq? `vmovl)  (nn 1 mov)]
     [(inst-eq? `vmovn)  (nn 2 mov)]
     [(inst-eq? `vqmovn) (nn 2 qmov)]
     ;[(inst-eq? `vqmovun) (nn 2 qmov)]

     ))
  
  (for ([x program])
       (interpret-step x))
  (progstate dregs rregs memory))