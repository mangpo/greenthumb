#lang s-exp rosette

(require 
 ;; ISA independent
 "ast.rkt"
 ;; ISA dependent
 "f18a.rkt" "state.rkt" "f18a-compress.rkt")

;(require rosette/solver/z3/z3)
;(require rosette/solver/kodkod/kodkod)

(provide superoptimize optimize 
         linear-search binary-search 
         program-eq? optimize-cost)

(define time-limit 10)
(define-syntax-rule (timeout sec expr)
  (let* ([t (let ([parent (current-thread)])
              (thread
               (thunk
                (thread-send 
                 parent
                 (with-handlers [(exn? identity)]
                                expr)))))]
         [out (sync/timeout sec t)])
    (cond [out  (thread-receive)]
          [else (break-thread t)
                (raise (thread-receive))])))

;; Superoptimize program given
;; spec: program specification (naive code)
;; sketch: skeleton of the output program
;; info: additional information (e.g. memory size, # of receiveing data)
;; constraint: constraint on the output state
;; cost: upperbound (exclusive) of the cost of the output program
(define (superoptimize spec sketch info constraint [cost #f]
                       #:bit [bit 18]
                       #:assume [assumption (default-state)])
  ;; (pretty-display "SUPERPOTIMIZE")
  ;; (print-struct spec)
  ;; (print-struct sketch)
  ;; (pretty-display info)
  ;; (pretty-display constraint)
  ;; (pretty-display assumption)

  ;(current-solver (new z3%))
  ;(current-solver (new kodkod%))
  (configure [bitwidth bit])
  (define start-state (default-state info (sym-input)))
  (define spec-state #f)
  (define sketch-state #f)

  ;; (pretty-display ">>>>>>>>>>> START >>>>>>>>>>>>>")
  ;; (display-state start-state)

  (define (interpret-spec)
    (assume start-state assumption)
    (pretty-display "interpret spec")
    (set! spec-state (interpret bit spec start-state)))

  (define (compare-spec-sketch)
    (pretty-display "interpret sketch")
    (set! sketch-state (interpret bit sketch start-state spec-state))
    
    ;; (pretty-display ">>>>>>>>>>> SPEC >>>>>>>>>>>>>")
    ;; (display-state spec-state)
    ;; (pretty-display ">>>>>>>>>>> SKETCH >>>>>>>>>>>>>")
    ;; (display-state sketch-state)
    ;; (pretty-display ">>>>>>>>>>> FORALL >>>>>>>>>>>>>")
    ;; (pretty-display (get-sym-vars start-state))
    (pretty-display "check output")
    (assert-output spec-state sketch-state constraint cost))
  
  ;; Collect input variables and contruct their init values.
  (define sym-vars (get-sym-vars start-state))
  (define init-pair (make-hash (for/list ([v sym-vars]) (cons v 0))))
  (define init-pair2 
    (make-hash (for/list ([v sym-vars]) 
                         (let* ([rand (random (<< 1 bit))]
                                [val (if (>= rand (<< 1 (sub1 bit)))
                                         (- rand (<< 1 bit))
                                         rand)])
                             (cons v val)))))
  (for ([pair (solution->list (solve (interpret-spec)))])
       (when (hash-has-key? init-pair (car pair))
             (hash-set! init-pair (car pair) (cdr pair))
             (hash-set! init-pair2 (car pair) (cdr pair))
             ))
  ;; (pretty-display ">>>>>>>>>>> INIT >>>>>>>>>>>>>")
  ;; (pretty-display init-pair)
  
  ;; ATTN(emina)
  ;; (assume start-state assumption) = precondition from the user
  ;; (interpret-spec) = precondition for input that is legal for spec
  ;; Using (assume start-state assumption) is faster.
  (define model 
    (timeout
     time-limit
     (synthesize 
      #:forall sym-vars
      #:init (sat (make-immutable-hash (hash->list init-pair)))
      #:assume (interpret-spec) ;; (assume start-state assumption)
      #:guarantee (compare-spec-sketch))
     )
    )

  (define final-program (decode sketch model))
  (define final-cost (evaluate (progstate-cost sketch-state) model))

  (pretty-display ">>> superoptimize-output")
  (print-struct final-program)
  (pretty-display (format "limit cost = ~a" cost))
  (pretty-display (format "old cost = ~a" (progstate-cost spec-state)))
  (pretty-display (format "new cost = ~a" final-cost))
  (pretty-display "=====================================")
  (clear-asserts)
  (values final-program final-cost)
  )

(define (program-eq? spec program info constraint
                     #:bit [bit 18]
                     #:assume [assumption (default-state)])
  (configure [bitwidth bit])
  (define start-state (default-state info (sym-input)))
  (define spec-state #f)
  (define program-state #f)

  (define (interpret-spec)
    (assume start-state assumption)
    (pretty-display "interpret spec")
    (set! spec-state (interpret bit spec start-state)))

  (define (compare)
    (pretty-display "interpret program")
    (set! program-state (interpret bit program start-state spec-state))
    
    (pretty-display ">>>>>>>>>>> SPEC >>>>>>>>>>>>>")
    (display-state spec-state)
    (pretty-display ">>>>>>>>>>> PROG >>>>>>>>>>>>>")
    (display-state program-state)

    (pretty-display "check output")
    (pretty-display constraint)
    (assert-output spec-state program-state 
                   (struct-copy progstate constraint [cost #f])
                   #f))

  (with-handlers* ([exn:fail? 
                    (lambda (e)
                      (pretty-display "program-eq? SAME")
                      (or (equal? (exn-message e) "verify: no counterexample found")
                          (raise e)))])
    (verify #:assume (interpret-spec) #:guarantee (compare))
    (pretty-display "program-eq? DIFF")
    #f))

;; Optimize the cost incrementally using fixed number of holes.
;; spec: encoded spec
;; sketch: encoded spec
(define (linear-search spec sketch info constraint [assumption (default-state)])
  (define final-program #f)
  (define (inner cost)
    (define-values (out-program out-cost) 
      (superoptimize spec sketch info constraint cost #:assume assumption))
    (set! final-program out-program)
    (inner out-cost))

  (with-handlers* ([exn:fail? (lambda (e) 
                                (if (regexp-match #rx"synthesize: synthesis failed" 
                                                  (exn-message e))
                                    final-program
                                    (raise e)))])
    (inner #f)))

;; Optimize the cost using binary search on the number of holes.
;; spec: non-encoded block
(define (binary-search spec info constraint [assumption (default-state)])
  (define final-program #f)
  (define encoded-spec (encode (block-body spec)))
  (define (inner begin end cost)
    (define middle (quotient (+ begin end) 2))
    (pretty-display `(binary-search ,begin ,end ,middle))
    (define encoded-sketch (encode (string-join (build-list middle (lambda (x) "_")))))
    (define-values (out-program out-cost)
      (with-handlers* ([exn:fail? 
                        (lambda (e) 
                          (pretty-display "catch error")
                          (if (regexp-match #rx"synthesize: synthesis failed" 
                                            (exn-message e))
                              (values #f cost)
                              (raise e)))])
        (superoptimize encoded-spec encoded-sketch info constraint cost 
                       #:assume assumption)))
    (pretty-display `(out ,out-program ,out-cost))

    (when out-program 
          (set! final-program out-program))

    (if out-program
        (inner begin middle out-cost)
        (and (< middle end) (inner (add1 middle) end cost))))
  
  (inner 1 (get-size (block-body spec)) #f)
  (pretty-display "after inner")
  (and final-program (list (block final-program (block-org spec) (block-info spec)))))

;; TODO: compress, decompress, "timeout", "same"

(define (optimize-cost spec sketch info constraint assumption [prefix (list)])
  ;; if structure -> linear search
  ;; if inst seq -> binary search
  (pretty-display "SPEC >>")
  (print-struct spec)
  (pretty-display "SKETCH >>")
  (print-struct sketch)
  (pretty-display "INFO >>")
  (print-struct info)
  (pretty-display "CONSTRAINT >>")
  (print-struct constraint)
  (pretty-display "ASSUMPTION >>")
  (print-struct assumption)

  (with-handlers 
   ([exn:break? (lambda (e) "timeout")])
   (define output-compressed
     (if (and (= (length spec) 1) (block? (car spec)))
         (binary-search (car spec) info constraint assumption)
         (linear-search (encode spec) 
                        (encode sketch) info constraint assumption)))
   
   ;; Decompress and verfiy
   (if output-compressed
       (decompress output-compressed info constraint assumption program-eq?)
       "same"))
  )

;; Merge consecutive blocks into one block, and get rid of item object.
(define (simplify program)
  ;; (pretty-display `(simplify ,program))
  (define (merge lst)
    (if (empty? lst) 
        lst
        (list (merge-blocks lst))))

  (define (f x)
    ;; (pretty-display `(simplify-f ,x))
    (define output (list))
    (define buffer (list))
    (for ([i x])
	 (cond 
          [(block? i) (set! buffer (cons i buffer))]
          [(and (item? i) (block? (item-x i))) (set! buffer (cons (item-x i) buffer))]
          [(assumption? i) (void)]
          [(and (item? i) (assumption? (item-x i))) (void)]
          [else
           (set! output (append output (merge (reverse buffer)) (list (simplify i))))
           (set! buffer (list))]))
    (append output (merge (reverse buffer))))
	 
  (traverse program list? f))
   
(define (optimize prog)

  (define (optimize-func func)
    ;; If func is simple, then limit is large so that we can optimize entire function.
    (define length-limit (and (label? func) (get-length-limit func)))
    (define (superoptimize-fragment x)
      (define simple-x (simplify x))
      (pretty-display ">>> after simplify >>>")
      (print-struct simple-x)
      (optimize-cost simple-x 
		     (generate-sketch simple-x) 
		     (generate-info prog simple-x) 
		     (generate-constraint func simple-x)
		     (generate-assumption x))
      )
    ;; END superoptimize-fragment

    (define (number-of-units l)
      (count (lambda (x) (not (assumption? (item-x x)))) l))

    (define (get-first-unit l)
      (if (assumption? (item-x (car l)))
          (cons (car l) (get-first-unit (cdr l)))
          (list (car l))))

    (define (sliding-window x)
      (pretty-display ">>>>> SLIDING WINDOW >>>>>")
      (define output (list))
      (define work (list))
      (define size 0) ;; invariant: size < length-limit
      
      (define (optimize-slide buffer)
        (pretty-display ">>> optimize-slide >>>")
        (pretty-display `(buffer ,buffer ,(length buffer)))
        (print-struct buffer)
	(define res (superoptimize-fragment buffer))
        (pretty-display `(res ,res))
	(cond
	 [(equal? res "same")
          (pretty-display "sliding-window: same->slide")
	  (set! output (append output (list (original (car work)))))
          (set! size (- size (item-size (car work))))
	  (set! work (cdr work))]
	 
	 [(equal? res "timeout")
          (pretty-display "sliding-window: timeout->shrink")
	  (if (> (number-of-units buffer) 1)
	      (optimize-slide (take buffer (sub1 (length buffer))))
	      (let ([first-unit (get-first-unit work)])
		(set! output (append output 
                                     (original 
                                      (filter (lambda (x) (not (assumption? (item-x x))))
                                              first-unit))))
                (set! size (- size (foldl + 0 (map item-size first-unit))))
		(set! work (drop work (length first-unit)))))]
	 
	 [else
          (pretty-display "sliding-window: found->skip")
	  (set! output (append output res))
          (set! size (- size (foldl + 0 (map item-size buffer))))
	  (set! work (drop work (length buffer)))]))
      
      (define (until-empty)
	(when (> size 0)
              (optimize-slide work)
              (until-empty)))
      
      ;; sliding window loop
      (for ([i x])
           (pretty-display `(superopt-unit ,i))
	   (cond
            [(and (> (item-size i) length-limit) (not (block? (item-x i))))
             (pretty-display `(and (> (item-size i) length-limit) (not (block? (item-x i)))))
             (until-empty)
             (set! work (list))
             (set! size 0)
             (set! output (append output (list (optimize-inner i))))
             ]

	    [(> (item-size i) length-limit)
             (pretty-display `(> (item-size i) length-limit))
	     (until-empty)
             (set! work (append work (list i)))
             (set! size (+ size (item-size i)))
             (optimize-slide work)]
	    
	    [(> (+ size (item-size i)) length-limit)
             (pretty-display `(> (+ size (item-size i)) length-limit))
	     (optimize-slide work)
	     (set! work (append work (list i)))
             (set! size (+ size (item-size i)))]
	    
	    [else
             (pretty-display "else")
	     (set! work (append work (list i)))
             (set! size (+ size (item-size i)))]))
      
      (until-empty)
      output)
    ;; END sliding-window
    
    (define (optimize-inner code)
      (define x (item-x code))
      (pretty-display ">>> optimize-inner >>>")
      (print-struct x)
      (cond
       [(list? x)
	(sliding-window x)]
       
       [(block? x)
	(define ret (superoptimize-fragment x))
	(if (or (equal? ret "timeout") (equal? ret "same"))
	    (original ret)
	    ret)]
       
       [(forloop? x)
	(forloop (optimize-inner (forloop-init x)) (optimize-inner (forloop-body x)))]
       [(ift? x)
	(ift (optimize-inner (ift-t x)))]
       [(iftf? x)
	(iftf (optimize-inner (iftf-t x)) (optimize-inner (iftf-f x)))]
       [(-ift? x)
	(-ift (optimize-inner (-ift-t x)))]
       [(-iftf? x)
	(-iftf (optimize-inner (-iftf-t x)) (optimize-inner (-iftf-f x)))]
       [else x]))
    ;; END optimize-inner
    
    ;; optimize-func body
    (if (label? func)
        (let ([opt (optimize-inner (wrap (label-body func) get-size))])
          (pretty-display "FINISH")
          (label (label-name func) opt (label-info func)))
	func))
  
  ;; optimize body
  (if prog
      (let ([code (program-code prog)]
            [memsize (program-memsize prog)]
            [indexmap (program-indexmap prog)])
        (program (map optimize-func code)
                 (if indexmap (dict-ref indexmap memsize) memsize)
                 indexmap))
      #f))
       
