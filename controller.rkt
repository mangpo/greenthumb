#lang s-exp rosette

(require 
 ;; ISA independent
 "ast.rkt" "liveness.rkt"
 ;; ISA dependent
 "GA/interpret.rkt" "GA/state.rkt" "GA/print.rkt" "GA/compress.rkt"
 )

;(require rosette/solver/z3/z3)
;(require rosette/solver/kodkod/kodkod)

(provide superoptimize 
	 optimize 
         linear-search binary-search 
         program-eq? optimize-cost
	 )

(define time-limit 3600)
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
(define (superoptimize spec sketch info constraint 
                       [cost #f]
                       #:assume-interpret [assume-interpret #t]
                       #:bit [bit 18]
                       #:assume [assumption (no-assumption)])
  (pretty-display (format "SUPERPOTIMIZE: assume-interpret = ~a" assume-interpret))
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

  ;; (define (interpret-spec-test)
  ;;   (define test-state (default-state info 0))
  ;;   (assume test-state assumption)
  ;;   (pretty-display "interpret spec test")
  ;;   (display-state (interpret spec test-state)))

  ;; (interpret-spec-test)

  (define (interpret-spec)
    (assume start-state assumption)
    (pretty-display "interpret spec")
    (set! spec-state (interpret spec start-state))
    (pretty-display "done interpret spec")
    )

  (define (compare-spec-sketch)
    (pretty-display "interpret sketch")
    (set! sketch-state (interpret sketch start-state spec-state))
    
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
    (make-hash 
     (for/list ([v sym-vars]) 
       (let* ([rand (random (min 4294967087 (<< 1 bit)))]
	      [val (if (>= rand (<< 1 (sub1 bit)))
		       (- rand (<< 1 bit))
		       rand)])
	 (cons v val)))))
  ;; (pretty-display ">>>>>>>>>>> INIT >>>>>>>>>>>>>")
  (for ([pair (solution->list (solve (interpret-spec)))])
       ;; (pretty-display `(pair ,pair))
       (when (hash-has-key? init-pair (car pair))
             ;; (pretty-display `(in-sol ,pair))
             (hash-set! init-pair (car pair) (cdr pair))
             (hash-set! init-pair2 (car pair) (cdr pair))
             ))
  ;; (pretty-display ">>>>>>>>>>> DONE INIT >>>>>>>>>>>>>")
  ;; (pretty-display init-pair2)
  
  ;; ATTN(emina)
  ;; (assume start-state assumption) = precondition from the user
  ;; (interpret-spec) = precondition for input that is legal for spec
  ;; Using (assume start-state assumption) is faster.

  (define model 
    (timeout
     time-limit
     (synthesize 
      #:forall sym-vars
      #:init (list 
              (sat (make-immutable-hash (hash->list init-pair)))
              (sat (make-immutable-hash (hash->list init-pair2)))
              )
      #:assume (if assume-interpret (interpret-spec) (assume start-state assumption))
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
    (pretty-display "eq: interpret spec")
    (set! spec-state (interpret spec start-state)))

  (define (compare)
    (pretty-display "eq: interpret program")
    (set! program-state (interpret program start-state spec-state))
    
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
(define (linear-search spec sketch info constraint 
                       [assumption (default-state)] 
                       [assume-interpret #t]
		       #:prefix [prefix (list)])
  (define final-program #f)
  (define (inner cost)
    (define-values (out-program out-cost) 
      (if (empty? prefix)
	  (superoptimize spec sketch info constraint cost 
                         ;#:assume-interpret assume-interpret
			 #:assume assumption)
	  (superoptimize (append prefix spec) (append prefix sketch) info constraint cost 
                         ;#:assume-interpret assume-interpret
			 #:assume assumption)))
    (set! final-program out-program)
    (inner out-cost))

  (with-handlers* 
   ([exn:fail? (lambda (e) 
                 (if (or (regexp-match #rx"synthesize: synthesis failed" (exn-message e))
                         (regexp-match #rx"assert: progstate-cost" (exn-message e)))
                     (if (empty? prefix)
                         final-program
                         (drop final-program (length prefix)))
                     (raise e)))]
    [exn:break? (lambda (e) 
                  (if final-program
                      (if (empty? prefix)
                          final-program
                          (drop final-program (length prefix)))
                      "timeout"))])
    (inner #f)))

;; Optimize the cost using binary search on the number of holes.
;; spec: non-encoded block
(define (binary-search spec info constraint 
                       [assumption (default-state)]
                       [assume-interpret #t]
		       #:prefix [prefix (list)])
  (define final-program #f)
  (define final-len (get-size (block-body spec)))
  (define final-cost #f)
  (define encoded-prefix (encode prefix))
  (define encoded-spec (encode spec))
  (define (inner begin end cost)
    (define middle (quotient (+ begin end) 2))
    (pretty-display `(binary-search ,begin ,end ,middle))
    (define encoded-sketch 
      (block
       (encode (string-join (build-list middle (lambda (x) "_"))))
       #f #f))
      
    (define-values (out-program out-cost)
      (with-handlers* ([exn:fail? 
                        (lambda (e) 
                          (pretty-display "catch error")
                          (if (regexp-match #rx"synthesize: synthesis failed" 
                                            (exn-message e))
                              (values #f cost)
                              (raise e)))])
        (superoptimize (append encoded-prefix (list encoded-spec)) 
		       (append encoded-prefix (list encoded-sketch))
		       info constraint cost 
                       ;#:assume-interpret assume-interpret
                       #:assume assumption)))
    (pretty-display `(out ,out-program ,out-cost))

    (when out-program 
          (set! final-program out-program)
	  (set! final-len middle)
	  (set! final-cost out-cost))

    (if out-program
        (inner begin middle out-cost)
        (when (< middle end) (inner (add1 middle) end cost))))
  
  (with-handlers 
   ([exn:break? (lambda (e) (unless final-program (set! final-program "timeout")))])
   (inner 1 final-len #f))

  ;; Try len + 2
  (unless (equal? final-program "timeout")
	  (with-handlers 
	   ([exn:break? (lambda (e) (void))])
	   (inner (+ final-len 2) (+ final-len 2) final-cost)))
	

  (pretty-display "after inner")
  (if (list? final-program)
      (let ([last-block (last final-program)])
        (list (block (block-body last-block) (block-org spec) (block-info spec))))
      final-program))

(define (optimize-cost spec sketch info constraint assumption #:prefix [prefix (list)])
  ;; if structure -> linear search
  ;; if inst seq -> binary search
  (pretty-display "PREFIX >>")
  (print-struct prefix)
  (pretty-display "SPEC >>")
  (print-struct spec)
  (pretty-display "SKETCH >>")
  (print-struct sketch)
  (pretty-display "INFO >>")
  (pretty-display `(syninfo ,(syninfo-memsize info) 
                            ,(syninfo-recv info) 
                            ,(syninfo-indexmap info)))
  (pretty-display "CONSTRAINT >>")
  (print-struct constraint)
  (pretty-display "ASSUMPTION >>")
  (print-struct assumption)
  (pretty-display "INTERPRET FOR ASSUME? >>")
  ;(define assume-interpret (interpret-for-assume? spec))

  (define output-compressed
    (if (and (= (length spec) 1) (block? (car spec)))
        (binary-search (car spec) info constraint assumption ;assume-interpret 
                       #:prefix prefix)
        (linear-search (encode spec) 
                       (encode sketch) 
                       info constraint assumption ;assume-interpret
                       #:prefix (encode prefix))))
   
   ;; Decompress and verfiy
  (cond
   [(equal? output-compressed "timeout") "timeout"]
   [(list? output-compressed) 
    (decompress output-compressed info constraint assumption program-eq? #:prefix prefix)]
   [else "same"])
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
    (define (superoptimize-fragment x #:prefix [prefix (list)])
      (define simple-x (simplify x))
      (define simple-prefix (simplify prefix))
      (pretty-display ">>> after simplify >>>")
      (print-struct simple-x)
      (optimize-cost simple-x 
		     (generate-sketch simple-x) 
		     (generate-info prog simple-x #:prefix simple-prefix) 
		     (generate-constraint simple-x)
		     (generate-assumption x #:prefix prefix)
		     #:prefix simple-prefix)
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
      (define output-org (list))
      (define work (list))
      (define size 0) ;; invariant: size < length-limit
      (define need-opt #t)

      (define (adjust-work-size lst)
	(set! output-org (append output-org lst))
	(set! size (- size (foldl + 0 (map item-size lst))))
	(set! work (drop work (length lst))))

      (define (append-org-output lst)
	(set! output (append output 
			     (original (filter (lambda (x) (not (assumption? (item-x x)))) lst)))))
      
      (define (optimize-slide buffer)
        (pretty-display ">>> optimize-slide >>>")
        (pretty-display `(buffer ,buffer ,(length buffer)))
        (print-struct buffer)
	(if need-opt
	    (let ([res (superoptimize-fragment buffer)]); #:prefix output-org)])
	      (pretty-display `(res ,res))
	      (cond
	       [(equal? res "same")
                (when (= (length buffer) (length work))
                  (set! need-opt #f))
		(pretty-display "sliding-window: same->slide")
		(define first-unit (get-first-unit work))
		(append-org-output first-unit)
		(adjust-work-size first-unit)]
	       
	       [(equal? res "timeout")
		(pretty-display "sliding-window: timeout->shrink")
		(if (> (number-of-units buffer) 1)
		    (optimize-slide (take buffer (sub1 (length buffer))))
		    (let ([first-unit (get-first-unit work)])
		      (if (block? (item-x (last first-unit)))
			  ;; block
			  (append-org-output first-unit)
			  ;; structure => recurse opyimixr-inner
			  (set! output 
				(append output (list (optimize-struct (last first-unit))))))
					 
		      (adjust-work-size first-unit)))]
	 
	       [else
		(pretty-display "sliding-window: found->skip")
		(set! output (append output res))
		(adjust-work-size buffer)]))
	    (begin
	      ;; no better implementation & no new unit => output original
	      (pretty-display "no more unit added (output = original)")
	      (append-org-output buffer)
	      (adjust-work-size buffer))))
	      
      
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
             (set! output (append output (list (optimize-struct i))))
             ]

	    [(> (item-size i) length-limit)
             (pretty-display `(> (item-size i) length-limit))
	     (until-empty)
             (set! work (append work (list i)))
	     (set! need-opt #t)
             (set! size (+ size (item-size i)))
             (optimize-slide work)]
	    
	    [(> (+ size (item-size i)) length-limit)
             (pretty-display `(> (+ size (item-size i)) length-limit))
	     (optimize-slide work)
	     (set! work (append work (list i)))
	     (set! need-opt #t)
             (set! size (+ size (item-size i)))]
	    
	    [else
             (pretty-display "else")
	     (set! work (append work (list i)))
	     (set! need-opt #t)
             (set! size (+ size (item-size i)))]))
      
      (until-empty)
      output)
    ;; END sliding-window
    
    (define (optimize-struct code)
      (define x (item-x code))
      (pretty-display ">>> optimize-struct >>>")
      (print-struct x)
      (cond
       [(list? x)
	(sliding-window x)]
       
       [(block? x)
	(define ret (superoptimize-fragment x))
	(if (or (equal? ret "timeout") (equal? ret "same"))
	    (original x)
	    ret)]
       
       [(forloop? x)
	(forloop (original (forloop-init x))
                 (optimize-struct (forloop-body x))
                 (forloop-bound x))]
       [(ift? x)
	(ift (optimize-struct (ift-t x)))]
       [(iftf? x)
	(iftf (optimize-struct (iftf-t x)) (optimize-struct (iftf-f x)))]
       [(-ift? x)
	(-ift (optimize-struct (-ift-t x)))]
       [(-iftf? x)
	(-iftf (optimize-struct (-iftf-t x)) (optimize-struct (-iftf-f x)))]
       [else x]))
    ;; END optimize-struct
    
    ;; optimize-func body
    (if (label? func)
        (let ([body (label-body func)])
          (pretty-display ">>> optimize-func >>>")
          (pretty-display (format "RELAX: ~a" (label-name func)))
          (relax-constraint body prog func-dict 
                            (cdr (hash-ref func-dict (label-name func))))
          (print-struct body)
          (let ([opt (optimize-struct (wrap body get-size))])
            (pretty-display "FINISH")
            (label (label-name func) opt (label-info func))))
        func))

  (define func-dict (make-hash))
  (define (modify-blockinfo-all func)
    (if (label? func)
	(let* ([name (label-name func)]
	       [body (traverse (label-body func) 
			       block? (lambda (x) (modify-blockinfo x func prog)))]
	       [new-func (label name body (label-info func))])
	  (hash-set! func-dict name (cons new-func #f))
	  new-func)
	func))
  
  ;; optimize body
  (if prog
      (let ([code (map modify-blockinfo-all (program-code prog))]
            [memsize (program-memsize prog)]
            [indexmap (program-indexmap prog)])
        (program (reverse (map optimize-func (reverse code)))
                 (if indexmap (dict-ref indexmap memsize) memsize)
                 indexmap))
      #f))
       
