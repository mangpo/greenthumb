#lang s-exp rosette

(require "../ast.rkt"
         "machine.rkt")

(provide interpret)

(define (interpret program state [policy #f])
  (define dregs (vector-copy (progstate-dregs state))) ;; 8-bit unit
  (define rregs (vector-copy (progstate-rregs state))) ;; 32-bit unit?
  (define memory (vector-copy (progstate-memory state))) ;; 8-bit unit

  (define (interpret-step x)
    (define op (inst-op x))
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
          (pretty-display `(base ,base))
          (vector-copy! dregs base val))))
      

    (define (load stride update)
      (define dest (vector-ref args 0))
      (define n (car dest))
      (define dest-regs (cdr dest)) ;; TODO: check validity
      (define type-byte (quotient (string->number (vector-ref type-id type)) 8))
      
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
      (for* ([iter (quotient (length (vector-ref dest-index-stride 0)) type-byte)]
             [lst-id stride])
            (let ([lst (vector-ref dest-index-stride lst-id)])
              (set! indexes (append indexes (take lst type-byte)))
              (vector-set! dest-index-stride lst-id (drop lst type-byte))))

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
      

    (define (vext)
      ;; Dd, Dn, Dm, imm | Qd, Qn, Qm, imm
      (define d (vector-ref args 0))
      (define n (vector-ref args 1))
      (define m (vector-ref args 2))
      (define imm (vector-ref args 3)) ;; TODO: check imm
      (unless (or (and (< d nregs-d) (< m nregs-d) (< n nregs-d))
                  (and (>= d nregs-d) (>= m nregs-d) (>= n nregs-d)))
              (raise "vext: operands mismatch."))
      (define vn (get-dreg n))
      (define vm (get-dreg m))
      (define type-byte (quotient (string->number (vector-ref type-id type)) 8))
      (define shift (* imm type-byte))
      
      (define vd (vector-append (vector-drop vn shift) (vector-take vm shift)))
      (set-dreg! d vd))
    
    (cond
     [(inst-eq? `vld1)  (load1 #f)]
     [(inst-eq? `vld1!) (load1 #t)]
     [(inst-eq? `vld2)  (load 2 #f)]
     [(inst-eq? `vld2!) (load 2 #t)]

     [(inst-eq? `vext)  (vext)]))
  
  (for ([x program])
       (interpret-step x))
  (progstate dregs rregs memory))