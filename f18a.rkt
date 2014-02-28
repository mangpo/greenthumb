#lang s-exp rosette

(require "state.rkt" "stack.rkt" "ast.rkt")

(provide (all-defined-out))

(define debug #f)

;; ISA
(define inst-id '#(@p @+ @b @ !p !+ !b ! +* 2* 
                      2/ - + and or drop dup pop over a 
                      nop push b! a!))

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
;; policy: a procedure that enforces a communication policy (see the definition of comm-policy below)
(define (interpret bit program state [policy #f])
  (set! policy (if policy
                   (comm-policy at-most policy)
                   (comm-policy all)))
      
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

  ;; Objective cost to be optimized.
  (define len-cost 0)

  (define-syntax-rule (len-add x)
    (set! len-cost (+ len-cost x)))

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
  
  ;; Read from the given memory address or communication port. If it
  ;; gets a communication port, it just returns a random number (for
  ;; now).
  (define (read-memory addr)
    (define (read port)
      (let ([val (car recv)]
            [type (hash-ref comm-dict port)])
        (set! comm (policy (cons val type) comm))
        (set! recv (cdr recv))
        val))
    (cond
     [(equal? addr UP)    (read UP)]
     [(equal? addr DOWN)  (read DOWN)]
     [(equal? addr LEFT)  (read LEFT)]
     [(equal? addr RIGHT) (read RIGHT)]
     [(equal? addr IO)    (read IO)]
     [else (vector-ref memory addr)]))
  
  ;; Write to the given memeory address or communication
  ;; port. Everything written to any communication port is simply
  ;; aggregated into a list.
  (define (set-memory! addr val)
    (when debug (pretty-display `(set-memory! ,addr ,val)))
    (define (write port)
      (let ([type (+ 5 (hash-ref comm-dict port))])
        (set! comm (policy (cons val type) comm))
      ))
    (cond
     [(equal? addr UP)    (write UP)]
     [(equal? addr DOWN)  (write DOWN)]
     [(equal? addr LEFT)  (write LEFT)]
     [(equal? addr RIGHT) (write RIGHT)]
     [(equal? addr IO)    (write IO)]
     [else (vector-set! memory addr val)]))

  (define (clip x)
    (let ([res (bitwise-and x #x3ffff)])
      (if (= (bitwise-and #x20000 res) 0)
          res
          (- (add1 (bitwise-xor res #x3ffff))))))

  (define (push-right-one x carry)
    (clip (bitwise-ior (<< (bitwise-and #x1 carry) (sub1 num-bits)) (>>> x 1))))
  
  ;; Treats T:A as a single 36 bit register and shifts it right by one
  ;; bit. The most signficicant bit (T17) is kept the same.
  (define (multiply-step-even!)
    (set! a (push-right-one a t))
    (set! t (>> t 1)))
  
  ;; Sums T and S and concatenates the result with A, shifting
  ;; the concatenated 37-bit to the right by one bit.
  (define (multiply-step-odd!)
    (let ([sum (+ t s)])
      (set! a (push-right-one a sum))
      (set! t (>> sum 1))))
  
  (define (interpret-step inst-const)
    (when debug (pretty-display `(interpret-step ,inst-const)))
    (define inst (car inst-const))
    (define const (cdr inst-const))
    (define-syntax-rule (inst-eq x) (= inst (vector-member x inst-id)))
    (cond
     [(inst-eq `@p)   (push! const) (len-add 5)]
     [(inst-eq `@+)   (push! (read-memory a)) (set! a (add1 a)) (len-add 1)]
     [(inst-eq `@b)   (push! (read-memory b)) (len-add 1)]
     [(inst-eq `@)    (push! (read-memory a)) (len-add 1)]
     [(inst-eq `!+)   (set-memory! a (pop!)) (set! a (add1 a)) (len-add 1)]
     [(inst-eq `!b)   (set-memory! b (pop!)) (len-add 1)]
     [(inst-eq `!)    (set-memory! a (pop!)) (len-add 1)]
     [(inst-eq `+*)   (if (= (bitwise-and #x1 a) 0)
                          (multiply-step-even!)
                          (multiply-step-odd!))
                      (len-add 1)]
     [(inst-eq `2*)   (set! t (clip (<< t 1))) (len-add 1)]
     [(inst-eq `2/)   (set! t (>> t 1)) (len-add 1)] ;; sign shiftx
     [(inst-eq `-)    (set! t (bitwise-not t)) (len-add 1)]
     [(inst-eq `+)    (push! (clip (+ (pop!) (pop!)))) (len-add 2)]
     [(inst-eq `and)  (push! (bitwise-and (pop!) (pop!))) (len-add 1)]
     [(inst-eq `or)   (push! (bitwise-xor (pop!) (pop!))) (len-add 1)]
     [(inst-eq `drop) (pop!) (len-add 1)]
     [(inst-eq `dup)  (push! t) (len-add 1)]
     [(inst-eq `pop)  (push! (r-pop!)) (len-add 1)]
     [(inst-eq `over) (push! s) (len-add 1)]
     [(inst-eq `a)    (push! a) (len-add 1)]
     [(inst-eq `nop)  (void) (len-add 0)]
     [(inst-eq `push) (r-push! (pop!)) (len-add 1)]
     [(inst-eq `b!)   (set! b (pop!)) (len-add 1)]
     [(inst-eq `a!)   (set! a (pop!)) (len-add 1)]
     [else (assert #f (format "invalid instruction ~a" inst))]
     ))

  (define (interpret-struct x)
    (cond
     [(list? x)
      (if (pair? (car x))
          (for ([i x]) (interpret-step i))
          (for ([i x]) (interpret-struct i)))]
      
     [(block? x)
      (interpret-struct (block-body x))]

     [(forloop? x)
      (interpret-struct (forloop-init x))
      (r-push! (pop!))
      (for ([i (in-range (add1 r))])
           (interpret-struct (forloop-body x))
           (set! r (sub1 r)))
      (r-pop!)
      ]

     [(ift? x)
      (when (not (equal? t 0))
            (interpret-struct (ift-t x)))]

     [(iftf? x)
      (if (not (equal? t 0))
          (interpret-struct (iftf-t x))
          (interpret-struct (iftf-f x)))]

     [(-ift? x)
      (when (negative? t) ;(or (negative? t) (>= t (arithmetic-shift 1 (sub1 bit)))) ;; negative
            (interpret-struct (-ift-t x)))]

     [(-iftf? x)
      (if (negative? t) ;(or (negative? t) (>= t (arithmetic-shift 1 (sub1 bit)))) ;; negative
          (interpret-struct (-iftf-t x))
          (interpret-struct (-iftf-f x)))]
     ))
    
  (interpret-struct program)
  (progstate a b p i r s t data return memory recv comm len-cost)
  )

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
                         (assert (equal? (cdr i1) (cdr i2)) `comm-type)))
          ))
  
  (define (check-cost)
    (when (progstate-cost constraint)
      (assert (> (progstate-cost state1) (progstate-cost state2)) `progstate-cost)))

  (check-reg progstate-a)
  (check-reg progstate-b)
  (check-reg progstate-r)
  (check-reg progstate-s)
  (check-reg progstate-t)
  (check-stack progstate-data)
  (check-stack progstate-return)
  (check-mem)
  (check-comm)
  ;;(check-cost)
  )

;; Assert assumption about start-state
(define (assume state constraint)
  (define (check item assumption)
    (when (pair? assumption)
          (cond
           [(member (car assumption) (list "=" '=))
            (assert (equal? item (cdr assumption)))]
           [(member (car assumption) (list "<=" '<=))
            (assert (and (<= item (cdr assumption)) (>= item 0)))])
          ))
          

  (define (check-reg progstate-x)
    (check (progstate-x state) (progstate-x constraint)))
  
  (define (check-stack progstate-x)
    (when (progstate-x constraint)
      (for ([i (in-range 8)])
           (check (get-stack (progstate-x state) i)
                  (get-stack (progstate-x constraint) i)))))
  
  (define (check-mem)
    (when (= (vector-length (progstate-memory constraint))
             (vector-length (progstate-memory state)))
      (define mem-state (progstate-memory state))
      (define mem-constraint (progstate-memory constraint))
      (for ([i (in-range 0 (vector-length mem-state))])
           (check (vector-ref mem-state i) (vector-ref mem-constraint i)))))

  (check-reg progstate-a)
  (check-reg progstate-b)
  (check-reg progstate-r)
  (check-reg progstate-s)
  (check-reg progstate-t)
  (check-stack progstate-data)
  (check-stack progstate-return)
  (check-mem)
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

;; Traverse a given program AST recursively until (base? program) is true.
;; Then apply base-apply to program.
(define (traverse program base? base-apply)
  (define (f x)
    (cond
     [(base? x)    (base-apply x)]
     [(list? x)    (map f x)]
     [(block? x)   (block (f (block-body x)) (f (block-org x)) 
                          (block-cnstr x) (block-assume x))]
     [(forloop? x) (forloop (f (forloop-init x)) (f (forloop-body x)))]
     [(ift? x)     (ift (f (ift-t x)))]
     [(iftf? x)    (iftf (f (iftf-t x)) (f (iftf-f x)))]
     [(-ift? x)    (-ift (f (-ift-t x)))]
     [(-iftf? x)   (-iftf (f (-iftf-t x)) (f (-iftf-f x)))]
     ))
  (f program))

(define (print-program x)
  (cond
   [(list? x)
    (map print-program x)
    (void)]

   [(block? x)
    (print-program (block-body x))]

   [(forloop? x)
    (print (forloop-init x))
    (display " for ")
    (print-program (forloop-body x))]

   [(ift? x)
    (display " if ")
    (print-program (ift-t x))
    (display " then ")]

   [(iftf? x)
    (display " if ")
    (print-program (iftf-t x))
    (display " ; ] then ")
    (print-program (iftf-f x))
    (newline)]

   [(-ift? x)
    (display " -if ")
    (print-program (-ift-t x))
    (display " then ")]

   [(-iftf? x)
    (display " -if ")
    (print-program (-iftf-t x))
    (display " ; ] then ")
    (print-program (-iftf-f x))
    (newline)]
   
   [else
    (display x)]))
  

;; Creates a policy that determines what kind of communication is allowed 
;; during interpretation.  The policy is a procedure that takes as input a 
;; comm pair and the current comm list.  If the policy allows 
;; the given pair to be added to the list, the pair is inserted at the beginning 
;; of the list and the result is returned.  If the policy doesn't allow the pair 
;; to be added to the list, then an assertion error is thrown.
(define-syntax comm-policy
  (syntax-rules (all at-most)
    [(comm-policy all) cons]         ; allow everything
    [(comm-policy at-most state)     ; allow only prefixes of the communication sequence observed in the given state 
     (let ([limit (reverse (progstate-comm state))])
       (lambda (p comm)
         (for*/all ([c comm])        ; this is not needed for correctness, but improves performance
           (let* ([len (length c)]
                  [limit-p (list-ref limit len)])
             (assert (< len (length limit)) 'comm-length)  
             (assert (equal? (car p) (car limit-p)) `comm-data)
             (assert (equal? (cdr p) (cdr limit-p)) `comm-type)
             (cons limit-p c)))))])) ; can return (cons p c) here, but this is more efficient. 
                                     ; it is correct because we assert that p == limit-p.
