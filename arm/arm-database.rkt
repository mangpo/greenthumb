#lang racket

(require "../database.rkt" "../ops-racket.rkt"
         "../ast.rkt" "arm-ast.rkt" "arm-machine.rkt")

(provide arm-database%)

(define arm-database%
  (class database%
    (super-new)
    (inherit-field machine validator validator-precise)     
    (override get-all-states vector->id progstate->id progstate->ids
              reduce-precision increase-precision)

    (define bit (get-field bit machine))
    (define fp (send machine get-fp))
    (define nregs (send machine get-nregs))
    (define nmems (send machine get-nmems))
    (define max-val (arithmetic-shift 1 bit))
    (define mask (sub1 (arithmetic-shift 1 bit)))
    (define inst-id (get-field inst-id machine))
    (define cmp-id (vector-member `cmp inst-id))

    (define reg-range-db
      (for/vector ([v (arithmetic-shift 1 bit)]) (finitize v bit)))
    (define z-range-db (vector -1 0 1 2 3))
    
    (define (get-all-states)
      ;;(when (> nmems 0) (raise "get-all-states: does not support nmems > 0"))
      (define states (list))
      
      ;;(define count 0)
      (define (recurse-regs z n vals)
        (if (= n 0)
            (begin
              ;;(pretty-display `(debug ,count ,vals))
              ;;(set! count (add1 count))
              (set! states (cons (progstate (list->vector (reverse vals))
                                            (vector) z fp) states))
              )
            (for ([v reg-range-db])
                 (recurse-regs z (sub1 n) (cons v vals)))))

      (for ([z z-range-db])
           (recurse-regs z nregs (list)))

      (reverse states))

    (define (vector->id state)
      (define z (vector-ref state 2))
      (define regs (vector-ref state 0))
      (define id 0)
        
      (for ([r regs]) (set! id (+ (* id max-val) (bitwise-and r mask))))
      
      (+ id (* (vector-member z z-range-db) (power max-val nregs))))

    (define (progstate->id state)
      (define z (progstate-z state))
      (define regs (progstate-regs state))
      (define id 0)
        
      (for ([r regs]) (set! id (+ (* id max-val) (bitwise-and r mask))))
      
      (+ id (* (vector-member z z-range-db) (power max-val nregs))))

    ;; TODO: test
    (define (progstate->ids state live)
      ;; (define use-cmp (member cmp-id (get-field inst-pool machine)))
      (define regs-live (progstate-regs live))
      (define regs (progstate-regs state))
      (define z-live (progstate-z live))
      (define z (progstate-z state))

      (define ret (list))

      (define (inner regs plus)
        (define id 0)
        (for ([r regs]) (set! id (+ (* id max-val) (bitwise-and r mask))))
        (set! ret (cons (+ plus id) ret)))

      (define (recurse ans regs live plus)
        (cond
         [(empty? regs) (inner ans plus)]
         [(car live) (recurse (cons (car regs) ans) (cdr regs) (cdr live) plus)]
         [else
          (for ([v reg-range-db])
               (recurse (cons v ans) (cdr regs) (cdr live) plus))]
         ))

      (cond
       [z-live 
        (recurse (list)
                 (reverse (vector->list regs))
                 (reverse (vector->list regs-live))
                 (* (vector-member z z-range-db) (power max-val nregs)))]
       [else ;;(not use-cmp)
        (recurse (list)
                 (reverse (vector->list regs))
                 (reverse (vector->list regs-live))
                 0)]
       ;; [else
       ;;  (for ([i (vector-length z-range-db)])
       ;;       (pretty-display `(plus ,(* i (power max-val nregs)) ,i ,max-val ,nregs))
       ;;       (recurse (list)
       ;;                (reverse (vector->list regs))
       ;;                (reverse (vector->list regs-live))
       ;;                (* i (power max-val nregs))))]
       )
      ret)
      
    (define (power b p)
      (if (= p 0) 1 (* b (power b (sub1 p)))))

    
    (define (reduce-inst x change)
      (define opcode-name (send machine get-inst-name (inst-op x)))
      (define args (inst-args x))
      (define shfop-name (and (inst-shfop x) (send machine get-shf-inst-name (inst-shfop x))))
      (define shfarg (inst-shfarg x))
      (define types (send machine get-arg-types opcode-name))
      
      (define new-args
        (for/vector
         ([arg args]
          [type types])
         (if (member type '(op2 bit bit-no-0))
             (change arg)
             arg)))

      (define new-shfarg
        (if (member shfop-name '(lsr# asr# lsl#))
            (change shfarg)
            shfarg))

      (arm-inst (inst-op x) new-args (inst-shfop x) new-shfarg (inst-cond x)))

    (define bit-nonprecise (get-field bit (get-field machine validator)))
    (define bit-precise (get-field bit (get-field machine validator-precise)))

    (define (reduce-precision prog)
      (define (change arg)
        (cond
         [(= arg bit-precise) bit-nonprecise]
         [(= arg (sub1 bit-precise)) (sub1 bit-nonprecise)]
         [(= arg (/ bit-precise 2)) (/ bit-nonprecise 2)]
         [else arg]))
      (for/vector ([x prog]) (reduce-inst x change)))
    
    (define (increase-precision prog)
      (define (change arg)
        (cond
         [(= arg bit-nonprecise) bit-precise]
         [(= arg (sub1 bit-nonprecise)) (sub1 bit-precise)]
         [(= arg (/ bit-nonprecise 2)) (/ bit-precise 2)]
         [else arg]))
      (for/vector ([x prog]) (reduce-inst x change)))
              

    ))
        
      
      
