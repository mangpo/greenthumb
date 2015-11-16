#lang racket

(require "../forwardbackward.rkt" "../ast.rkt" "../ops-racket.rkt"
         "GA-machine.rkt")

(require (only-in "GA-simulator-racket.rkt" [GA-simulator-racket% GA-simulator-racket%]))
(require (only-in "GA-simulator-rosette.rkt" [GA-simulator-rosette% GA-simulator-rosette%]))

(provide GA-forwardbackward%)

(define GA-forwardbackward%
  (class forwardbackward%
    (super-new)
    (inherit-field machine printer)
    (override len-limit window-size
              mask-in 
              reduce-precision increase-precision reduce-precision-assume
              change-inst change-inst-list
	      get-live-mask combine-live prescreen)

    (define (len-limit) 8)
    (define (window-size) 14)

    (define bit 4)
    
    (define UP (get-field UP machine))
    (define DOWN (get-field DOWN machine))
    (define LEFT (get-field LEFT machine))
    (define RIGHT (get-field RIGHT machine))
    (define IO (get-field IO machine))
    
    (define UP-abst (get-field UP machine))
    (define DOWN-abst (get-field DOWN machine))
    (define LEFT-abst (get-field LEFT machine))
    (define RIGHT-abst (get-field RIGHT machine))
    (define IO-abst (get-field IO machine))

    (define inst-id (get-field inst-id machine))
    (define mask (sub1 (arithmetic-shift 1 bit)))
    (define mask-1 (sub1 (arithmetic-shift 1 (sub1 bit))))
    
    (define (change-inst x change)
      (define opcode-id (inst-op x))
      (define opcode-name (send machine get-inst-name opcode-id))
      (define arg (inst-args x))
      (if (equal? opcode-name `@p)
          (inst opcode-id (change arg))
          x))

    (define (change-inst-list x change)
      (define opcode-id (inst-op x))
      (define opcode-name (send machine get-inst-name opcode-id))
      (define arg (inst-args x))

      (if (equal? opcode-name `@p)
          (for/list ([new-arg (change arg)]) (inst opcode-id new-arg))
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

    (define (get-live-mask state-vec)
      (define (inner x)
        (cond
         [(vector? x) (for/vector ([i x]) (number? i))]
         [(number? x) #t]
         [else #f]))
      (for/vector ([x state-vec]) (inner x)))
    
    (define (mask-in state-vec live-list #:keep-flag [keep #t])
      (if live-list
          (let* ([pass #t]
                 [ret
                  (for/vector ([x state-vec]
                               [v live-list] #:break (not pass))
                              (cond
                               [(number? x)
                                (and v x)]
                               [(and (vector? x) (or (number? v) (vector? v)))
                                (for/vector
                                 ([i x] [live v] #:break (not pass))
                                 (when (and live (not i)) (set! pass #f))
                                 (and live i))]
                               [(and (vector? x) (equal? v #f))
                                (make-vector (vector-length x) #f)]
                               [(equal? v #t)
                                (unless x (set! pass #f)) x]
                               [else x]))])
	    (and pass ret))
          state-vec))
      
    ;; Ignore a completely.
    (define (combine-live a b)
      (define s (vector-ref b 3))
      (define t (vector-ref b 4))
      (when (and s (not t)) (vector-set! b 4 #t))
      b)

    (define (prescreen my-inst state)
      (define opcode-id (inst-op my-inst))
      (define a (vector-ref state 0))
      (define b (vector-ref state 1))
      (define r (vector-ref state 2))
      (define s (vector-ref state 3))
      (define t (vector-ref state 4))
      (define mem-len (vector-length (vector-ref state 7)))

      (define opcode-name (vector-ref inst-id opcode-id))
      (define-syntax-rule (inst-eq x) (equal? x opcode-name))
      (cond
       [(member opcode-name '(@b))
        (and b
             (or (and (>= b 0) (< b mem-len))
                 (member b (list UP-abst DOWN-abst LEFT-abst RIGHT-abst))))]
       [(member opcode-name '(!b))
        (and b t
             (or (and (>= b 0) (< b mem-len))
                 (member b (list UP-abst DOWN-abst LEFT-abst RIGHT-abst))))]
       [(member opcode-name '(@ @+))
        (and a
             (or (and (>= a 0) (< a mem-len))
                 (member a (list UP-abst DOWN-abst LEFT-abst RIGHT-abst))))]
       [(member opcode-name '(@ @+ ! !+))
        (and a t
             (or (and (>= a 0) (< a mem-len))
                 (member a (list UP-abst DOWN-abst LEFT-abst RIGHT-abst))))]
       ;; TODO: up down left right for bit = 4

       [(member opcode-name '(+*)) (and a s t)]
       [(member opcode-name '(2* 2/ - dup push b! a!)) t]
       [(member opcode-name '(+ and or)) (and s t)]
       [(member opcode-name '(pop)) r]
       [(member opcode-name '(over)) s]
       [(member opcode-name '(a)) a]
       [else #t]))
      
    
    ))
  
    
