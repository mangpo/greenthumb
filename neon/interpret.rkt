#lang s-exp rosette

(require "../ast.rkt" "../ops.rkt"
         "machine.rkt")

(provide interpret)

;; vector -> number
(define (bytes->number vec)
  (foldr (lambda (x res) (+ (<< res 8) x)) 0 (vector->list vec)))

;; number -> list
(define (number->bytes num len)
  (for/list ([i len])
    (let ([x (bitwise-and num #xff)])
      (set! num (>> num 8))
      x)))

;; vector to list
(define (bytes->elements vec byte)
  (for/list ([i (quotient (vector-length vec) byte)])
    (bytes->number (vector-copy vec (* i byte) (* (add1 i) byte)))))

;; list to vector
(define (elements->bytes lst byte)
  (list->vector
   (flatten
    (for/list ([x lst]) (number->bytes x byte)))))


;; (define (finite-bit val byte type)
;;   (bitwise-and val (sub1 (arithmetic-shift 1 (* byte 8)))))

(define (sign-extend val byte type)
  (if (= type (vector-member `u type-id))
      val
      (finitize val (* 8 byte))))

(define (saturate val byte type)
  (cond
   [(= type (vector-member `s type-id))
    (define bound (arithmetic-shift 1 (sub1 (* byte 8))))
    (if (>= val bound) 
        (sub1 bound)
        (if (< val (- bound)) 
            (- bound)
            val))
    ]
   [(= type (vector-member `u type-id))
    (define bound (<< 1 (* byte 8)))
    (if (>= val bound)
        (sub1 bound)
        (if (< val 0)
            0
            val))
    ]
   ))
  

(define (interpret program state [policy #f])
  (define dregs (vector-copy (progstate-dregs state))) ;; 8-bit unit
  (define rregs (vector-copy (progstate-rregs state))) ;; 32-bit unit?
  (define memory (vector-copy (progstate-memory state))) ;; 8-bit unit

  (define (interpret-step x)
    (define op (inst-op x))
    (define byte (inst-byte x))
    (define type (inst-type x))
    (define args (inst-args x))

    (define-syntax-rule (inst-eq? x) (= op (vector-member x inst-id)))
    (define-syntax-rule (type-eq? x) (= type (vector-member x inst-type)))

    (define (get-dreg id)
      (let ([bytes (if (< id nregs-d) 8 16)]
            [my-id (if (< id nregs-d) id (- id nregs-d))])
        (let ([base (* my-id bytes)])
          (vector-copy dregs base (+ base bytes)))))

    (define (set-dreg! id val)
      (pretty-display `(set-dreg! ,id ,val))
      (let ([bytes (if (< id nregs-d) 8 16)]
            [my-id (if (< id nregs-d) id (- id nregs-d))])
        (let ([base (* my-id bytes)])
          (vector-copy! dregs base val))))

    (define (load stride update)
      (define dest (vector-ref args 0))
      (define n (car dest))
      (define dest-regs (cdr dest)) ;; TODO: check validity
      
      (define dest-index-list
        (for/list ([i n])
                  (map (lambda (x) (+ x (* 8 (vector-ref dest-regs i))))
                       (range 8))))

      (pretty-display `(dest-index-list ,dest-index-list))

      (define dest-index-stride (make-vector stride (list)))
      (for* ([iter (quotient n stride)]
             [i stride])
            (let ([lst (car dest-index-list)])
              (set! dest-index-list (cdr dest-index-list))
              (vector-set! dest-index-stride i
                           (append (vector-ref dest-index-stride i) lst))))
      (pretty-display `(dest-index-stride ,dest-index-stride))

      (define indexes (list))
      (for* ([iter (quotient (length (vector-ref dest-index-stride 0)) byte)]
             [lst-id stride])
            (let ([lst (vector-ref dest-index-stride lst-id)])
              (set! indexes (append indexes (take lst byte)))
              (vector-set! dest-index-stride lst-id (drop lst byte))))

      (define r-id (vector-ref args 1))
      (define mem-addr (vector-ref rregs r-id)) ;; check alignment

      (pretty-display `(indexes ,indexes))
      
      (for ([i (* n 8)]
            [reg-addr indexes]) ;; reverse indexes here
           (vector-set! dregs reg-addr (vector-ref memory (+ mem-addr i))))

      (when update (vector-set! rregs r-id (+ mem-addr (* n 8)))))

    ;; TODO (load) special case for vld1 (no stride)
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
      
    (define (nnn f arg-type)
      (define d (vector-ref args 0))
      (define n (vector-ref args 1))
      (define m (vector-ref args 2))
      (cond
       [(= arg-type 0) ;; normal
        (unless (or (and (< d nregs-d) (< n nregs-d) (< m nregs-d))
                    (and (>= d nregs-d) (>= n nregs-d) (>= m nregs-d)))
                (raise "Normal: operands mismatch."))]
       [(= arg-type 1) ;; long
        (unless (and (>= d nregs-d) (< n nregs-d) (< m nregs-d))
                (raise "Long: operands mismatch."))]
       [(= arg-type 2) ;; wide
        (unless (and (>= d nregs-d) (>= n nregs-d) (< m nregs-d))
                (raise "Wide: operands mismatch."))])
       
      (define vn (get-dreg n))
      (define vm (get-dreg m))
      (set-dreg! d (f d vn vm)))

    (define (ext d vn vm)
      (define imm (vector-ref args 3))
      (define shift (* imm byte))
      (vector-append (vector-drop vn shift) (vector-take vm shift)))

    (define (mla d vn vm)
      (define ed (bytes->elements (get-dreg d) byte)) ;; list
      (define en (bytes->elements vn byte))
      (define em (bytes->elements vm byte))
      (define res
        (for/list ([num-d ed]
                   [num-n en]
                   [num-m em])
                  (+ num-d (* num-n num-m))))
      (elements->bytes res byte))

    (define (mlal d vn vm)
      (define ed (bytes->elements (get-dreg d) (* 2 byte))) ;; list
      (define en (bytes->elements vn byte))
      (define em (bytes->elements vm byte))
      (pretty-display `(mlal ,ed))
      (define res
        (for/list ([num-d ed]
                   [num-n en]
                   [num-m em])
          (+ num-d (* (sign-extend num-n byte type) 
                      (sign-extend num-m byte type)))))
      (elements->bytes res (* 2 byte)))
    
    (cond
     [(inst-eq? `vld1)  (load1 #f)]
     [(inst-eq? `vld1!) (load1 #t)]
     [(inst-eq? `vld2)  (load 2 #f)]
     [(inst-eq? `vld2!) (load 2 #t)]

     [(inst-eq? `vext)  (nnn ext 0)]
     [(inst-eq? `vmla)  (nnn mla 0)]
     [(inst-eq? `vmlal) (nnn mlal 1)]

     ))
  
  (for ([x program])
       (interpret-step x))
  (progstate dregs rregs memory))