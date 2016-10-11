#lang racket

(require "../forwardbackward.rkt" "../inst.rkt" "../ops-racket.rkt"
         "../special.rkt" "../memory-racket.rkt"
         "GA-machine.rkt")

(require (only-in "GA-simulator-racket.rkt" [GA-simulator-racket% GA-simulator-racket%]))
(require (only-in "GA-simulator-rosette.rkt" [GA-simulator-rosette% GA-simulator-rosette%]))

(provide GA-forwardbackward%)

(define GA-forwardbackward%
  (class forwardbackward%
    (super-new)
    (inherit-field machine printer)
    (override len-limit window-size
              reduce-precision increase-precision reduce-precision-assume
              change-inst change-inst-list
	      combine-live)

    (define (len-limit) 8)
    (define (window-size) 14)

    (define bit 4)
    
    (define UP #x145)
    (define DOWN #x115)
    (define LEFT #x175)
    (define RIGHT #x1d5)
    (define IO #x15d)
    
    (define UP-abst -3)
    (define DOWN-abst -4)
    (define LEFT-abst -5)
    (define RIGHT-abst -6)
    (define IO-abst -7)

    (define opcodes (get-field opcodes machine))
    (define mask (sub1 (arithmetic-shift 1 bit)))
    (define mask-1 (sub1 (arithmetic-shift 1 (sub1 bit))))
    
    (define (change-inst x change)
      (define opcode-id (inst-op x))
      (define opcode-name (send machine get-opcode-name opcode-id))
      (if (equal? opcode-name `@p)
          (inst opcode-id (vector-map change (inst-args x)))
          x))

    (define (change-inst-list x change)
      (define opcode-id (inst-op x))
      (define opcode-name (send machine get-opcode-name opcode-id))

      (if (equal? opcode-name `@p)
          (let ([arg (vector-ref (inst-args x) 0)])
            (for/list ([new-arg (change arg)]) (inst opcode-id (vector new-arg))))
          (list x)))
    
    (define (reduce-precision prog)
      (define mapping (make-hash))
      (define (change arg)
        (define ret
          (cond
           [(= arg UP) UP-abst]
           [(= arg DOWN) DOWN-abst]
           [(= arg LEFT) LEFT-abst]
           [(= arg RIGHT) RIGHT-abst]
           [(= arg IO) IO-abst]
           [(> arg 0) (bitwise-and arg mask-1)]
           [else (finitize (bitwise-and arg mask) bit)]))
        (if (hash-has-key? mapping ret)
            (let ([val (hash-ref mapping ret)])
              (unless (member arg val)
                      (hash-set! mapping ret (cons arg val))))
            (hash-set! mapping ret (list arg)))
        ret)
        
      (cons (for/vector ([x prog]) (change-inst x change)) mapping))

    (define (reduce-precision-assume x)
      (define (inner x)
        (cond
         [(boolean? x) x]
         [(number? x)
          (cond
           [(= x 0) 0]
           [(> x 0)
            (define ret (bitwise-and x mask-1))
            (if (= ret 0) (arithmetic-shift 1 (quotient bit 2)) ret)]
           [(< x 0)
            (define ret (finitize (bitwise-and x mask) bit))
            (if (= ret 0) (arithmetic-shift -1 (quotient bit 2)) ret)])]

         [(pair? x) (cons (inner (car x)) (inner (cdr x)))]
         [else x]
         ))
      (and x
	   (let ([data (progstate-data x)])
	     (progstate (inner (progstate-a x))
			(inner (progstate-b x))
			(inner (progstate-r x))
			(inner (progstate-s x))
			(inner (progstate-t x))
			(stack (stack-sp data)
			       (for/vector ([i (stack-body data)])
					   (inner i)))
			(progstate-return x)
			(progstate-memory x)
			(progstate-recv x)
			(progstate-comm x)))))

    (define (increase-precision prog mapping)
      (define (change arg)
        (define (finalize x)
          (if (hash-has-key? mapping arg)
              (let ([val (hash-ref mapping arg)])
                (if (member x val) val (cons x val)))
              (list x)))
        (finalize arg))

      (define ret (list))
      (define (recurse lst final)
        (if (empty? lst)
            (set! ret (cons (list->vector final) ret))
            (for ([x (car lst)])
                 (recurse (cdr lst) (cons x final)))))
      
      (recurse (reverse (for/list ([x prog]) (change-inst-list x change)))
               (list))
      ret)
    
    ;; Ignore a completely.
    (define (combine-live a b)
      (define s (vector-ref b 3))
      (define t (vector-ref b 4))
      (when (and s (not t)) (vector-set! b 4 #t))
      b)
    
    ))
  
    
