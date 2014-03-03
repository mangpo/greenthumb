#lang s-exp rosette

(require "f18a.rkt" "state.rkt" "ast.rkt")
;(require rosette/solver/z3/z3)
;(require rosette/solver/kodkod/kodkod)

(provide superoptimize optimize)

;; Superoptimize program given
;; spec: program specification (naive code)
;; sketch: skeleton of the output program
;; info: additional information (e.g. memory size, # of receiveing data)
;; constraint: constraint on the output state
(define (superoptimize spec sketch info constraint
                       #:bit [bit 18]
                       #:assume [assumption (default-state)])
  (pretty-display "SUPERPOTIMIZE")
  (print-struct spec)
  (print-struct sketch)
  (pretty-display info)
  (pretty-display constraint)
  (pretty-display assumption)

  ;(current-solver (new z3%))
  ;(current-solver (new kodkod%))
  (configure [bitwidth bit])
  (define start-state (default-state info (sym-input)))
  (define spec-state #f)

  ;; (pretty-display ">>>>>>>>>>> START >>>>>>>>>>>>>")
  ;; (display-state start-state)

  (define (interpret-spec)
    (assume start-state assumption)
    (pretty-display "interpret spec")
    (set! spec-state (interpret bit spec start-state)))

  (define (compare-spec-sketch)
    (pretty-display "interpret sketch")
    (define sketch-state (interpret bit sketch start-state spec-state))
    
    ;; (pretty-display ">>>>>>>>>>> SPEC >>>>>>>>>>>>>")
    ;; (display-state spec-state)
    ;; (pretty-display ">>>>>>>>>>> SKETCH >>>>>>>>>>>>>")
    ;; (display-state sketch-state)
    ;; (pretty-display ">>>>>>>>>>> FORALL >>>>>>>>>>>>>")
    ;; (pretty-display (get-sym-vars start-state))
    (pretty-display "check output")
    (assert-output spec-state sketch-state constraint))
  
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
  (pretty-display ">>>>>>>>>>> INIT >>>>>>>>>>>>>")
  (pretty-display init-pair2)
  
  (define model 
    (synthesize 
     #:forall sym-vars
     #:init (sat (make-immutable-hash (hash->list init-pair)))
     ;; start cegis with all inputs set to 0
     #:assume (interpret-spec);; (assume start-state assumption)
     #:guarantee (compare-spec-sketch))
    )
  
  (define ret (decode sketch model))

  (pretty-display "superoptimize-output >>")
  (print-struct ret)
  ;(clear-asserts)
  ret
  )

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

  (if (and (= (length spec) 1) (block? (car spec)))
      ;(binary-search)
      (superoptimize (encode spec) (encode sketch) info constraint #:assume assumption)
      (superoptimize (encode spec) (encode sketch) info constraint #:assume assumption)))

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
   
(define (optimize program)

  (define (optimize-func func)
    ;; If func is simple, then limit is large so that we can optimize entire function.
    (define length-limit (and (label? func) (get-length-limit func)))
    (define (superoptimize-fragment x)
      (define simple-x (simplify x)) ;; TODO: error here!
      (pretty-display ">>> after simplify >>>")
      (print-struct simple-x)
      (optimize-cost simple-x 
		     (generate-sketch simple-x) 
		     (generate-info program simple-x) 
		     (generate-constraint func simple-x)
		     (generate-assumption x))
      )
    ;; END superoptimize-fragment

    (define (sliding-window x)
      (define output (list))
      (define work (list))
      (define size 0) ;; invariant: size < length-limit
      
      (define (optimize-slide buffer)
        (pretty-display ">>> optimize-slide >>>")
        (print-struct buffer)
	(define res (superoptimize-fragment buffer))
	(cond
	 [(equal? res "same")
	  ;; TODO: org
	  (set! output (append output (list (original (car work)))))
          (set! size (- size (item-size (car work))))
	  (set! work (cdr work))]
	 
	 [(equal? res "timeout")
	  (if (> (length buffer) 1)
	      (sliding-window (take buffer (sub1 (length buffer))))
	      (begin
		(set! output (append output (list (original (car work)))))
                (set! size (- size (item-size (car work))))
		(set! work (cdr work))))]
	 
	 [else
	  (set! output (append output res))
          (set! size (- size (foldl + 0 (map item-size buffer))))
	  (set! work (drop work (length buffer)))]))
      
      (define (until-empty)
	(unless (empty? work)
		(optimize-slide work)
		(until-empty)))
      
      ;; sliding window loop
      (for ([i x])
	   (cond
	    [(> (item-size i) length-limit)
	     (until-empty)
	     (set! output (append (list (optimize-inner i))))
             (set! size 0)]
	    
	    [(> (+ size (item-size i)) length-limit)
	     (optimize-slide work)
	     (set! work (append work (list i)))
             (set! size (+ size (item-size i)))]
	    
	    [else
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
        (label (label-name func) 
               (optimize-inner (wrap (label-body func) number-of-insts)) 
               (label-info func))
	func))
  
  ;; optimize body
  (map optimize-func (program-code program)))
       
