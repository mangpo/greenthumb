#lang s-exp rosette

(require "state.rkt" "stack.rkt" "ast.rkt")

(provide print-syntax
         ;; superoptimizer
         interpret assert-output assume
         ;; for controller
         encode decode get-size
         get-length-limit merge-blocks modify-blockinfo interpret-for-assume?
         generate-sketch generate-info generate-constraint generate-assumption)

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
     [(inst-eq `@p)   (push! const)]
     [(inst-eq `@+)   (push! (read-memory a)) (set! a (add1 a))]
     [(inst-eq `@b)   (push! (read-memory b))]
     [(inst-eq `@)    (push! (read-memory a))]
     [(inst-eq `!+)   (set-memory! a (pop!)) (set! a (add1 a))]
     [(inst-eq `!b)   (set-memory! b (pop!))]
     [(inst-eq `!)    (set-memory! a (pop!))]
     [(inst-eq `+*)   (if (= (bitwise-and #x1 a) 0)
                          (multiply-step-even!)
                          (multiply-step-odd!))]
     [(inst-eq `2*)   (set! t (clip (<< t 1)))]
     [(inst-eq `2/)   (set! t (>> t 1))] ;; sign shiftx
     [(inst-eq `-)    (set! t (bitwise-not t))]
     [(inst-eq `+)    (push! (clip (+ (pop!) (pop!))))]
     [(inst-eq `and)  (push! (bitwise-and (pop!) (pop!)))]
     [(inst-eq `or)   (push! (bitwise-xor (pop!) (pop!)))]
     [(inst-eq `drop) (pop!)]
     [(inst-eq `dup)  (push! t)]
     [(inst-eq `pop)  (push! (r-pop!))]
     [(inst-eq `over) (push! s)]
     [(inst-eq `a)    (push! a)]
     [(inst-eq `nop)  (void)]
     [(inst-eq `push) (r-push! (pop!))]
     [(inst-eq `b!)   (set! b (pop!))]
     [(inst-eq `a!)   (set! a (pop!))]
     [else (assert #f (format "invalid instruction ~a" inst))]
     ))

  (define (interpret-struct x)
    (when debug (pretty-display `(interpret-struct ,x)))
    (cond
     [(list? x)
      (unless (empty? x)
              (if (pair? (car x))
                  (for ([i x]) (interpret-step i))
                  (for ([i x]) (interpret-struct i))))]
      
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

     [else (raise (format "interpret-struct: unimplemented for ~a" x))]
     ))

  (define (cost-step inst-const)
    (define inst (car inst-const))
    (define-syntax-rule (inst-eq x) (= inst (vector-member x inst-id)))
    (cond
     [(inst-eq `+) 2]
     [(inst-eq `nop) 0]
     [else 1]))

  (define (cost-struct x)
    ;(pretty-display `(cost-struct ,x))
    (cond
     [(list? x)
      ;(pretty-display `(list ,(empty? x)))
                   (if (empty? x)
                       0
                       (if (pair? (car x))
                           (foldl (lambda (i all) (+ all (cost-step i))) 0 x)
                           (foldl (lambda (i all) (+ all (cost-struct i))) 0 x)))]
     [(block? x)   (cost-struct (block-body x))]
     [(forloop? x) (+ (cost-struct (forloop-init x)) (cost-struct (forloop-body x)))]
     [(ift? x)     (cost-struct (ift-t x))]
     [(iftf? x)    (+ (cost-struct (iftf-t x)) (cost-struct (iftf-f x)))]
     [(-ift? x)    (cost-struct (-ift-t x))]
     [(-iftf? x)   (+ (cost-struct (-iftf-t x)) (cost-struct (-iftf-f x)))]
     [else (raise (format "cost-struct: unimplemented for ~a" x))]
     ))
    
  (interpret-struct program)
  (define cost (cost-struct program))
  ;(pretty-display `(cost ,cost))
  (progstate a b r s t data return memory recv comm cost)
  )

;; REQUIRED FUNCTION
;; Assert if state1 == state2 wrt to constraint
;; state1, state2, constraint: progstate
;; TODO: comm-data, comm-type
(define (assert-output state1 state2 constraint cost)
  (define (check-reg progstate-x)
    (when (progstate-x constraint)
      ;(pretty-display `(check-reg ,(equal? (progstate-x state1) (progstate-x state2))))
      (assert (equal? (progstate-x state1) (progstate-x state2)) `progstate-x)))
  
  (define-syntax-rule (check-stack progstate-x)
    (when (progstate-x constraint)
      (for ([i (in-range (progstate-x constraint))])
           ;; (pretty-display `(check-stack ,(equal? (get-stack (progstate-x state1) i) 
           ;;                                        (get-stack (progstate-x state2) i))))
        (assert (equal? (get-stack (progstate-x state1) i) 
                   (get-stack (progstate-x state2) i))
                `progstate-x))))
  
  (define-syntax-rule (check-mem)
    (let ([mem1 (progstate-memory state1)]
          [mem2 (progstate-memory state2)]
          [mem-const (progstate-memory constraint)])
      (if (vector? mem-const)
          (for ([i (in-range 0 (vector-length mem1))])
               (when (vector-ref mem-const i)
                     ;;(pretty-display `(check-mem ,(equal? (vector-ref mem1 i) (vector-ref mem2 i))))
                     (assert (equal? (vector-ref mem1 i) (vector-ref mem2 i)) 
                             `progstate-mem)))
          
          (for ([i (in-range 0 (vector-length mem1))])
               (assert (equal? (vector-ref mem1 i) (vector-ref mem2 i)) 
                       `progstate-mem)))
    ))
  
  (define-syntax-rule (check-comm)
    (when (progstate-comm constraint)
          ;; (pretty-display `(check-comm-length ,(equal? (length (progstate-comm state1)) 
          ;;                                              (length (progstate-comm state2)))))
          (assert (equal? (length (progstate-comm state1)) 
                          (length (progstate-comm state2))) `comm-length)
          (for*/all ([j1 (progstate-comm state1)]
                     [j2 (progstate-comm state2)])
                    (for ([i1 j1]
                          [i2 j2])
                         (assert (equal? (car i1) (car i2)) `comm-data)
                         (assert (equal? (cdr i1) (cdr i2)) `comm-type)))
          ))
  
  (define-syntax-rule (check-cost)
    (when (progstate-cost constraint)
          ;(pretty-display `(check-cost ,cost ,(progstate-cost state2) ,(progstate-cost state1)))
          (if cost
              (assert (< (progstate-cost state2) cost) `progstate-cost)
              (assert (< (progstate-cost state2) (progstate-cost state1)) 
                      `progstate-cost))))

  (check-reg progstate-a)
  (check-reg progstate-b)
  (check-reg progstate-r)
  (check-reg progstate-s)
  (check-reg progstate-t)
  (check-stack progstate-data)
  (check-stack progstate-return)
  (check-mem)
  (check-comm)
  ;;(pretty-display "check cost")
  (check-cost)
  ;;(pretty-display "done check cost")
  )
  

;; Assert assumption about start-state
(define (assume state constraint)
  (define (check item assumption)
    (when (pair? assumption)
          (cond
           [(member (car assumption) (list "=" '=))
	    (if (list? (cdr assumption))
		(assert (member item (cdr assumption)))
		(assert (equal? item (cdr assumption))))]
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

(define (sym-inst)
  (define-symbolic* inst number?)
  (assert (and (>= inst 0) (< inst (vector-length inst-id))))
  inst)

(define (sym-const)
  (define-symbolic* const number?)
  const)

;; Convert a program from string into a proper format for 'interpret' function.
(define (inst-string->list program)
  (define port-dict (hash "up" UP "down" DOWN "left" LEFT "right" RIGHT "io" IO))
  (map (lambda (x) 
         (cond 
           [(equal? x "_") 
            (cons (sym-inst) (sym-const))]
           [(string->number x) 
            (cons (vector-member `@p inst-id) (string->number x))]
           [(hash-has-key? port-dict x)
            (cons (vector-member `@p inst-id) (hash-ref port-dict x))]
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

;; Convert string of instructions into encoded numbers.
(define (encode program)
  (traverse program string? inst-string->list))

;; Convert encoded numbers into string of instructions.
(define (decode program model)
  (traverse program 
	    [block?
	     (lambda (x) (block (list->inst-string (block-body x) model) 
				(block-org x) 
				(block-info x)))]

	    [(lambda (x) (and (list? x) (empty? x)))
	     (lambda (x) x)]

	    [(lambda (x) (and (list? x) (pair? (car x))))
	     (lambda (x) (list->inst-string x model))]))

	    
  ;; (traverse program 
  ;;           (lambda (x) (and (list? x) 
  ;;                            (or (empty? x) (pair? (car x)))))
  ;;           (lambda (x) (list->inst-string x model))))

;; Merge a list of block into one block.
(define (merge-blocks block-list)
  (define infos (map block-info block-list))
  (define cnstr (blockinfo-cnstr (last infos)))
  (define recv (foldl + 0 (map blockinfo-recv infos)))
  (block (string-join (map block-body block-list))
         (string-join (map block-org block-list))
         (blockinfo cnstr recv)))

(define (generate-sketch spec)
  (pretty-display ">>> generate-sketch >>>")
  (define (inner program)
    (traverse program 
              [string? 
               (lambda (x) 
                 (string-join (build-list (length (string-split x)) (lambda (i) "_"))))]
              [forloop?
               (lambda (x)
                 ;; Keep init concrete
                 (forloop (forloop-init x) (inner (forloop-body x)) (forloop-bound x)))]))
  (inner spec))

;; Generate info necessary for creating default-state.
;; (cons mem-size recv-size)
(define (generate-info program spec #:prefix [prefix (list)])
  (pretty-display ">>> generate-info >>>")
  (syninfo (program-memsize program) 
           (number-of-recv (append prefix spec)) 
           (program-indexmap program)))

;; Generate output constraint for synthesizer.
(define (generate-constraint spec)
  (pretty-display ">>> generate-constraint >>>")
  (blockinfo-cnstr (block-info (last-block spec))))

;; Transform blockinfo-cnstr into progstate format.
(define (modify-blockinfo b func prog)
  (block (block-body b) 
         (block-org b) 
         (blockinfo (create-constraint (blockinfo-cnstr (block-info b))
                                       (labelinfo-data (label-info func))
                                       (labelinfo-return (label-info func))
                                       (program-memsize prog)
                                       )
                    (blockinfo-recv (block-info b)))))

;; Generate assumption for synthesizer.
(define (generate-assumption spec #:prefix [prefix (list)])
  (define (to-number x)
    (cond 
     [(number? x) x]
     [(equal? x 'up) UP]
     [(equal? x 'down) DOWN]
     [(equal? x 'left) LEFT]
     [(equal? x 'right) RIGHT]
     [(equal? x 'io) IO]
     [(equal? x 'udlr) (list UP DOWN LEFT RIGHT)]
     ))

  (pretty-display ">>> generate-assumption >>>")

  (define (inner spec [res (default-state)])
    (cond
     [(and (list? spec) (assumption? (item-x (first spec))))
      ;; TODO: currently only support assumption <= or = on stack.
      ;; assume is a pair but can be extended to list of pair
      (define assume (assumption-cnstr (item-x (first spec))))
      (define ele (car assume))
      (define condition (cdr assume))
      
      (define ret
	(inner
	 (cdr spec) ;; recursive in case there are more than one assumption objects.
	 (cond 
	  [(equal? ele 'stack) 
	   (constrain-stack condition res)]
	  [(equal? ele 'a) 
	   (struct-copy progstate res [a (cons (car condition) (to-number (cdr condition)))])]
	  [(equal? ele 'b) 
	   (struct-copy progstate res [b (cons (car condition) (to-number (cdr condition)))])]
	  [(equal? ele 't) 
	   (struct-copy progstate res [t (cons (car condition) (to-number (cdr condition)))])]
	  [else (raise (format "generate-assumption: unimplemented for ~a" assume))]
	  )))
      
      (pretty-display `(assumption ,ret))
      ret
      ]
     
     [else res]))

  (if (empty? prefix) (inner spec) (inner prefix)))

(define (interpret-for-assume? code)
  (define last-inst #f)
  (define (f x)
    (cond
     [(block? x)
      (define need-interpret #f)
      (for ([inst (string-split (block-body x))])
           (when (and (or (equal? inst "b!") (equal? inst "a!"))
                      (equal? (string->number last-inst) #f)
                      (not (member last-inst (list "up" "down" "left" "right" "io" "udlr"))))
                 (set! need-interpret #t))
           (set! last-inst inst))
      need-interpret]
     [(list? x)    (ormap f x)]
     [(forloop? x) (or (f (forloop-init x)) (f (forloop-body x)))]
     [(ift? x)     (f (ift-t x))]
     [(iftf? x)    (or (f (iftf-t x)) (f (iftf-f x)))]
     [(-ift? x)    (f (-ift-t x))]
     [(-iftf? x)   (or (f (-iftf-t x)) (f (-iftf-f x)))]
     [else (raise (format "interpret-for-assume?: unimplemented for ~a" x))]))
  (define ret (f code))
  (pretty-display `(interpret-for-assume? ,ret))
  ret)

(define (get-length-limit func)
  (if (labelinfo-simple (label-info func)) 1000 16))

(define (get-size insts)
  (length (string-split insts)))

(define (number-of-recv x)
  (cond
   [(block? x) (blockinfo-recv (block-info x))]
   [(list? x)  (foldl + 0 (map number-of-recv x))]
   [(forloop? x) 
    (define body-count 
      (+ (number-of-recv (forloop-init x)) (number-of-recv (forloop-body x))))
    (if (forloop-bound x) 
        (* (forloop-bound x) body-count)
        (if (= body-count 0)
            0
            (raise "number-of-recv: there is read operation inside unbounded loop.")))]
   [(ift? x)   (number-of-recv (ift-t x))]
   [(iftf? x)  (max (number-of-recv (iftf-t x)) (number-of-recv (iftf-f x)))]
   [(-ift? x)  (number-of-recv (-ift-t x))]
   [(-iftf? x) (max (number-of-recv (-iftf-t x)) (number-of-recv (-iftf-f x)))]
   [else       (raise (format "number-of-recv: unimplemented for ~a" x))]))

(define (print-syntax x w h id [original #t])

  (define node-offset 10)
  (define block-offset 800)
  (define-syntax-rule (core-id id w)
    (+ (* 100 (floor (/ id w))) (modulo id w) node-offset))
  (define-syntax-rule (inc indent)
    (string-append indent "  "))

  (define (small-body? l [size 0])
    (cond 
     [(empty? l) #t]
     [(or (block? (car l)) 
          (and (item? (car l)) (block? (item-x (car l)))))
      (define b (if (block? (car l)) (car l) (item-x (car l))))
      (for ([inst (string-split (block-body b))])
           (if (or (string->number inst)
                   (member inst (list "up" "down" "left" "right" "udlr" "io")))
               (set! size (+ size 5))
               (set! size (add1 size))))
      (and (< size 4) (small-body? (cdr l) size))]
     [else #f]))
  
  (define (f x indent)
    (cond
     [(string? x)
      (for ([i (string-split x)])
           (cond
            [(equal? i "nop") (void)]
            [(equal? i "+") (display ". + ")]
            [(equal? i "0") (display "dup dup or ")]
            [else (display i) (display " ")]))
      ]
      
     [(list? x)
      (for ([i x])
           (f i indent))]
     
     [(block? x)
      (f (block-body x) indent)]
     
     [(ift? x)
      (when original (display "| cr"))
      (newline)
      (display indent)
      (display ".. if ")
      (f (ift-t x) (inc indent))
      (display " then ")]
     
     [(iftf? x)
      (when original (display "| cr"))
      (newline)
      (display indent)
      (display ".. if ")
      (f (iftf-t x) (inc indent))
      (display " ; ] then ")
      (f (iftf-f x) (inc indent))]
     
     [(-ift? x)
      (when original (display "| cr"))
      (newline)
      (display indent)
      (display ".. -if ")
      (f (-ift-t x) (inc indent))
      (display " then ")]
     
     [(-iftf? x)
      (when original (display "| cr"))
      (newline)
      (display indent)
      (display ".. -if ")
      (f (-iftf-t x) (inc indent))
      (display " ; ] then ")
      (f (-iftf-f x) (inc indent))
      (newline)]
   
     [(forloop? x)
      (when original (display "| cr"))
      (newline)
      (display indent)
      
      (f (forloop-init x) indent)
      (display "for ")
      (when original (display "| cr"))
      (newline)
      (display (inc indent))
      (f (forloop-body x) (inc indent))
      (if (small-body? (forloop-body x))
          (display "unext ")
          (display "next "))]

     [(special? x)
      (when original (display "| cr"))
      (newline)
      (display indent)
      (cond
       [(equal? (special-name x) "mult")
        (display "a! dup dup or 17 for +* unext drop drop a")]
       [else (raise (format "print-syntax: unimplemented for special ~a" 
                            (special-name x)))])
      (when original (display "| cr"))
      (newline)
      (display indent)]
   
     [(call? x)
      (define name (call-name x))
      (when (or original (not (member name (list "in" "out"))))
            (display name)
            (display " "))
      ]
     
     [(item? x)
      (f (item-x x) indent)]
    
     [(label? x)
      (display (format ": ~a " (label-name x)))
      (when original (display "= $0 "))
      
      (f (label-body x) "  ")
      
      (when original 
            (when (equal? (label-name x) "main")
                  (display "warm "))
          (display "= $0 "))
      (display "; ")
      (when original (display "| cr"))
      (newline)
      ]
   
     [(vardecl? x)
      (for ([val (vardecl-val x)])
           (display val)
           (display " , "))
      (if original
          (pretty-display "| br")
          (pretty-display "green"))
      ]
     
     [(program? x)
      (define memsize (program-memsize x))
      (define node (core-id id w))

      (if original
          (begin
            (pretty-display (format "{block ~a}" (+ block-offset (* 2 id))))
            (pretty-display (format "( -) # ~a ( id ~a mem ~a) 0 org | cr" node id memsize)))
          (begin
            (pretty-display (format "yellow ~a node" id))
            (pretty-display (format "0 org"))))
      
      (f (program-code x) "")
      (newline)
      ]

     [(vector? x)
      (pretty-display "{block 790}")
      (pretty-display "host target | cr")
      (for ([id (* w h)])
           (when (vector-ref x id)
                 (pretty-display (format "~a node ~a load" 
                                         (core-id id w) (+ block-offset (* 2 id))))))
      
      (newline)
      (pretty-display "{block 792}")
      (pretty-display ": /node dup +node /ram ; | cr")
      (for ([id (* w h)])
           (when (vector-ref x id)
                 (pretty-display (format "~a /node $0 /p" (core-id id w)))))
      (newline)
      
      (for ([i (* w h)])
           (set! id i)
           (f (vector-ref x i) ""))
      ]

     [(or (equal? x #f) (assumption? x)) (void)]
      
     [else (raise (format "print-syntax: unimplemented for ~a" x))]))
  (f x ""))
  
