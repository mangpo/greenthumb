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
              change-inst change-inst-list)

    (define (len-limit) 8)
    (define (window-size) 14)

    (define bit 4)
    
    (define UP #x145)
    (define DOWN #x115)
    (define LEFT #x175)
    (define RIGHT #x1d5)
    (define IO #x15d)
    
    (define UP-abst -2)
    (define DOWN-abst -4)
    (define LEFT-abst -6)
    (define RIGHT-abst -8)
    (define IO-abst 6)

    ;; (define UP-abst -3)
    ;; (define DOWN-abst -4)
    ;; (define LEFT-abst -5)
    ;; (define RIGHT-abst -6)
    ;; (define IO-abst -7)

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
    
    ;; Optional but make performance better much better.
    ;; Without this, we will never mask-in init states
    ;; (mask-in init state with 'a' which is always #f in GA
    ;; because GA:update-live always returns #f)
    (define/override (combine-live a b)
      (define s (vector-ref b 3))
      (define t (vector-ref b 4))
      (when (and s (not t)) (vector-set! b 4 #t))
      b)

    ;; Optional but make performance better much better.
    (define/override (mask-in state-vec live-list #:keep-flag [keep #t])
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
                               [(is-a? x memory-racket%)
                                (if v x (send x clone-init))]
                               [(is-a? x special%) x]
                               [(equal? v #t)
                                (unless x (set! pass #f)) x]
                               [else x]))])
            (and pass ret))
          state-vec))

    ;; ;; optional but make performance slightly better.
    ;; (define/override (prescreen my-inst state)
    ;;   (define opcode-id (inst-op my-inst))
    ;;   (define a (progstate-a state))
    ;;   (define b (progstate-b state))
    ;;   (define r (progstate-r state))
    ;;   (define s (progstate-s state))
    ;;   (define t (progstate-t state))
    ;;   (define mem-len 64)

    ;;   (define opcode-name (vector-ref opcodes opcode-id))
    ;;   (define-syntax-rule (inst-eq x) (equal? x opcode-name))
    ;;   (cond
    ;;    [(member opcode-name '(@b))
    ;;     (and b
    ;;          (or (and (>= b 0) (< b mem-len))
    ;;              (member b (list UP-abst DOWN-abst LEFT-abst RIGHT-abst))))]
    ;;    [(member opcode-name '(!b))
    ;;     (and b t
    ;;          (or (and (>= b 0) (< b mem-len))
    ;;              (member b (list UP-abst DOWN-abst LEFT-abst RIGHT-abst))))]
    ;;    [(member opcode-name '(@ @+))
    ;;     (and a
    ;;          (or (and (>= a 0) (< a mem-len))
    ;;              (member a (list UP-abst DOWN-abst LEFT-abst RIGHT-abst))))]
    ;;    [(member opcode-name '(! !+))
    ;;     (and a t
    ;;          (or (and (>= a 0) (< a mem-len))
    ;;              (member a (list UP-abst DOWN-abst LEFT-abst RIGHT-abst))))]
    ;;    ;; TODO: up down left right for bit = 4

    ;;    [(member opcode-name '(+*)) (and a s t)]
    ;;    [(member opcode-name '(2* 2/ - dup push b! a!)) t]
    ;;    [(member opcode-name '(+ and or)) (and s t)]
    ;;    [(member opcode-name '(pop)) r]
    ;;    [(member opcode-name '(over)) s]
    ;;    [(member opcode-name '(a)) a]
    ;;    [else #t]))
    
    ))
  
    
