#lang racket

(require "../database.rkt" "../ops-racket.rkt" "arm-machine.rkt")

(provide arm-database%)

(define arm-database%
  (class database%
    (super-new)
    (inherit-field machine)     
    (override get-all-states vector->id progstate->id progstate->ids)

    (define bit (get-field bit machine))
    (define fp (send machine get-fp))
    (define nregs (send machine get-nregs))
    (define nmems (send machine get-nmems))
    (define max-val (arithmetic-shift 1 bit))
    (define mask (sub1 (arithmetic-shift 1 bit)))

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
      (define regs-live (progstate-regs live))
      (define regs (progstate-regs state))
      (define z (progstate-z state))

      (define ret (list))

      (define (inner regs)
        (define id 0)
        (for ([r regs]) (set! id (+ (* id max-val) (bitwise-and r mask))))
        (set! ret (cons id ret)))

      (define (recurse ans regs live )
        (cond
         [(empty? regs) (inner ans)]
         [(car live) (recurse (cons (car regs) ans) (cdr regs) (cdr live))]
         [else
          (for ([v reg-range-db])
               (recurse (cons v ans) (cdr regs) (cdr live)))]
         ))

      (recurse (list)
               (reverse (vector->list regs))
               (reverse (vector->list regs-live)))

      ret)
      
      
    (define (power b p)
      (if (= p 0) 1 (* b (power b (sub1 p)))))

    ))
        
      
      
