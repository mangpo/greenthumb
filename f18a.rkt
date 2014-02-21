#lang s-exp rosette

(require "state.rkt" "stack.rkt")

(provide interpret assert-output 
         inst-string->list list->inst-string)

;; ISA
(define inst-id '#(@p @+ @b @ !p !+ !b ! +* 2* 2/ - + 
                      and or drop dup pop over a nop push b! a!))

(define UP #x145) ;325
(define DOWN #x115) ;277
(define LEFT #x175) ;373
(define RIGHT #x1d5) ;469
(define IO #x15d)

(define num-bits 18)
 
;; REQUIRED FUNCTION
;; Interpret a given program from a given state.
;; program: a list of (inst,const)
;; state: progstate
;; spec-state: state after intepreting spec. This is given when interpreting sketch.
(define (interpret program state)
  (define a (progstate-a state))
  (define b (progstate-b state))
  (define p (progstate-p state))
  (define i (progstate-i state))
  (define r (progstate-r state))
  (define s (progstate-s state))
  (define t (progstate-t state))
  (define data (stack (stack-sp (progstate-data state))
                      (vector-copy (stack-body (progstate-data state)))))
  (define return (stack (stack-sp (progstate-return state))
                        (vector-copy (stack-body (progstate-return state)))))
  (define memory (vector-copy (progstate-memory state)))
  
  (define recv (progstate-recv state))
  (define comm (progstate-comm state))
  ;(define comm-ref (and spec-state (reverse (progstate-comm spec-state))))
  ;(pretty-display `(comm-ref ,comm-ref))

  ;;; Pushes to the data stack.
  (define (push! value)
    (push-stack! data s)
    (set! s t)
    (set! t value))
  
  ;;; Pushes to the return stack.
  (define (r-push! value)
    (push-stack! return r)
    (set! r value))
  
  ;;; Pops from the data stack.
  (define (pop!)
    (let ([ret-val t])
      (set! t s)
      (set! s (pop-stack! data))
      ret-val))
      
  ;;; Pops from the return stack.
  (define (r-pop!)
    (let ([ret-val r])
      (set! r (pop-stack! return))
          ret-val))
  
  ;; Define comm-type
  (define comm-dict (hash UP 0 DOWN 1 LEFT 2 RIGHT 3 IO 4))

  (define-syntax-rule (send val port)
    (set! comm (cons (cons val (hash-ref comm-dict port)) comm)))
  
  ;; Read from the given memory address or communication port. If it
  ;; gets a communication port, it just returns a random number (for
  ;; now).
  (define (read-memory addr)
    (define-syntax-rule (member? x a ...)
      (or (equal? x a) ...))
    (define (read port)
      (let ([val (car recv)])
        (set! comm (cons (cons val (hash-ref comm-dict port)) comm))
        (set! recv (cdr recv))
        val))
    (if (member? addr UP DOWN LEFT RIGHT IO)
        (cond
         [(equal? addr UP)    (read UP)]
         [(equal? addr DOWN)  (read DOWN)]
         [(equal? addr LEFT)  (read LEFT)]
         [(equal? addr RIGHT) (read RIGHT)]
         [(equal? addr IO)    (read IO)])
        (vector-ref memory addr)))
  
  ;; Write to the given memeory address or communication
  ;; port. Everything written to any communication port is simply
  ;; aggregated into a list.
  (define (set-memory! addr val)
    (define (write port)
      (set! comm (cons (cons val (+ 5 (hash-ref comm-dict port))) comm))
      )
    (cond
     [(equal? addr UP)    (write UP)]
     [(equal? addr DOWN)  (write DOWN)]
     [(equal? addr LEFT)  (write LEFT)]
     [(equal? addr RIGHT) (write RIGHT)]
     [(equal? addr IO)    (write IO)]
     [else (vector-set! memory addr val)]))
  
  ;; Signed right shift
  (define (right-shift-one x)
    (let ([x17 (bitwise-and x #x20000)])
      (bitwise-ior x17 (arithmetic-shift x -1))))
  
  ;; Treats T:A as a single 36 bit register and shifts it right by one
  ;; bit. The most signficicant bit (T17) is kept the same.
  (define (multiply-step-even!)
    (let ([t0  (bitwise-and t #x1)])
      (set! t (right-shift-one t))
      (set! a (bitwise-ior (arithmetic-shift t0 (sub1 num-bits)) 
                           (arithmetic-shift a -1)))))
  
  ;; Sums T and S and concatenates the result with A, shifting
  ;; everything to the right by one bit.
  (define (multiply-step-odd!)
    (let* ([sum (+ t s)]
           [sum17 (bitwise-and sum #x20000)]
           [result (bitwise-ior (arithmetic-shift sum (sub1 num-bits)) 
                                (arithmetic-shift a -1))])
      (set! a (bitwise-bit-field result 0 num-bits))
      (set! t (bitwise-ior sum17 
                           (bitwise-bit-field result num-bits (* 2 num-bits))))))
  
  (define (interpret-step inst-const)
    (define inst (car inst-const))
    (define const (cdr inst-const))
    (define-syntax-rule (inst-eq x) (= inst (vector-member x inst-id)))
    (cond
      [(inst-eq `@p)   (push! const)]
      [(inst-eq `@+)   (push! (read-memory a)) (set! a (add1 a))]
      [(inst-eq `@b)   (push! (read-memory b))]
      [(inst-eq `@)    (push! (read-memory a))]
      [(inst-eq `!+)   (set-memory! a (pop!)) (set! a (add1 a))]
      [(inst-eq `!b)   (set-memory! b (pop!))]
      [(inst-eq `!)    (set-memory! a (pop!))]
      [(inst-eq `+*)   (if (even? a)
                                (multiply-step-even!)
                                (multiply-step-odd!))]
      [(inst-eq `2*)   (set! t (arithmetic-shift t 1))]
      [(inst-eq `2/)   (set! t (right-shift-one t))] ;; sign shiftx
      [(inst-eq `-)    (set! t (bitwise-not t))]
      [(inst-eq `+)    (push! (+ (pop!) (pop!)))]
      [(inst-eq `and)  (push! (bitwise-and (pop!) (pop!)))]
      [(inst-eq `or)   (push! (bitwise-xor (pop!) (pop!)))]
      [(inst-eq `drop) (pop!)]
      [(inst-eq `dup)  (push! t)]
      [(inst-eq `over) (push! s)]
      [(inst-eq `a)    (push! a)]
      [(inst-eq `nop)    (void)]
      [(inst-eq `push) (r-push! (pop!))]
      [(inst-eq `b!)   (set! b (pop!))]
      [(inst-eq `a!)   (set! a (pop!))]
      [else (assert #f (format "invalid instruction ~a" inst))]
      ))
  
  (for ([inst program])
    (interpret-step inst))
  
  (progstate a b p i r s t data return memory recv comm))

;; REQUIRED FUNCTION
;; Assert if state1 == state2 wrt to constraint
;; state1, state2, constraint: progstate
;; TODO: comm-data, comm-type
(define (assert-output state1 state2 constraint)
  (define-syntax-rule (check-reg progstate-x)
    (when (progstate-x constraint)
      (assert (equal? (progstate-x state1) (progstate-x state2)) `progstate-x)))
  
  (define-syntax-rule (check-stack progstate-x)
    (when (progstate-x constraint)
      (for ([i (in-range (progstate-x constraint))])
        (assert (equal? (get-stack (progstate-x state1) i) 
                   (get-stack (progstate-x state2) i))
                `progstate-x))))
  
  (define-syntax-rule (check-mem)
    (when (progstate-memory constraint)
      (define mem1 (progstate-memory state1))
      (define mem2 (progstate-memory state2))
      (for ([i (in-range 0 (vector-length mem1))])
           ;;(pretty-display `(assert (= ,(vector-ref mem1 i) ,(vector-ref mem2 i))))
        ;; (assert (= (vector-ref mem1 i) (vector-ref mem2 i)) `progstate-mem))))
        ;; = expects number? not symbolic
        (assert (equal? (vector-ref mem1 i) (vector-ref mem2 i)) `progstate-mem))))
  
  (define-syntax-rule (check-comm)
    (when (progstate-comm constraint)
          (assert (equal? (length (progstate-comm state1)) 
                          (length (progstate-comm state2))) `comm-length)
          (for*/all ([j1 (progstate-comm state1)]
                     [j2 (progstate-comm state2)])
                    (for ([i1 j1]
                          [i2 j2])
                         (assert (equal? (car i1) (car i2)) `comm-data)
                         (assert (equal? (cdr i1) (cdr i2)) `comm-type))
          )))
  
  (check-reg progstate-a)
  (check-reg progstate-b)
  (check-reg progstate-r)
  (check-reg progstate-s)
  (check-reg progstate-t)
  (check-stack progstate-data)
  (check-stack progstate-return)
  (check-mem)
  (check-comm)
  )

(define (sym-inst)
  (define-symbolic* inst number?)
  (assert (and (>= inst 0) (< inst (vector-length inst-id))))
  inst)

(define (sym-const)
  (define-symbolic* const number?)
  const)

;; Convert a program from string into a proper format for 'interpret' function.
(define (inst-string->list program)
  (map (lambda (x) 
         (cond 
           [(equal? x "_") 
            (cons (sym-inst) (sym-const))]
           [(string->number x) 
            (cons (vector-member `@p inst-id) (string->number x))]
           [(equal? x "@p")
            (cons (vector-member `@p inst-id) (sym-const))]
           [else
            (cons (vector-member (string->symbol x) inst-id) 0)]))
       (string-split program)))

;; Evaluate lst with the given model and return the output in straing format
(define (list->inst-string lst model)
  (string-join (map (lambda (x) 
                      (let* ([inst (vector-ref inst-id (evaluate (car x) model))])
                        (if (equal? inst `@p)
                            (if (sym? (evaluate (cdr x) model))
                                "?"
                                (number->string (evaluate (cdr x) model)))
                            (symbol->string inst))))
                    lst)))
  

      