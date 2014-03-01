#lang s-exp rosette

(require "f18a.rkt" "state.rkt" "ast.rkt")
;(require rosette/solver/z3/z3)

(provide superoptimize)

;; Superoptimize program given
;; spec: program specification (naive code)
;; sketch: skeleton of the output program
;; info: additional information (e.g. memory size, # of receiveing data)
;; constraint: constraint on the output state
(define (superoptimize spec sketch info constraint
                       #:bit [bit 18]
                       #:assume [assumption (default-state)])
  ;(current-solver (new z3%))
  (configure [bitwidth bit])
  (define start-state (default-state info (sym-input)))
  ;; (set! spec (inst-string->list spec))
  ;; (set! sketch (inst-string->list sketch))

  (define (compare-spec-sketch)
    ;; (pretty-display ">>>>>>>>>>> START >>>>>>>>>>>>>")
    ;; (display-state start-state)
    (pretty-display "interpret spec")
    (define spec-state (interpret bit spec start-state))
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
  
  (define model 
    (synthesize 
     #:forall sym-vars
     #:init (sat (for/hash ([v sym-vars]) (values v 0))) ; start cegis with all inputs set to 0
     #:assume (assume start-state assumption)
     #:guarantee (compare-spec-sketch))
    )
  
  (decode sketch model)
  )

(define (optimize-cost spec sketch info constraint)
  ;; if structure -> linear search
  ;; if inst seq -> binary search
  spec)
   
(define (optimize program)
  (define length-limit 16)

  (define (optimize-func func)
    (define (superoptimize-fragment x)
      (pretty-display `(superopt ,x))
      (print-program x)
      (newline)
      (define simple-x (simplify x))
      ;; TODO: implement these functions
      (optimize-cost simple-x 
		     (generate-sketch simple-x) 
		     (generate-info program simple-x) 
		     (generate-constraint func simple-x)))
    ;; END superoptimize-fragment

    (define (sliding-window x)
      (define output (list))
      (define work (list))
      (define size 0) ;; invariant: size < length-limit
      
      (define (optimize-slide buffer)
	(define res (superoptimize-fragment buffer))
	(cond
	 [(equal? res "same")
	  ;; TODO: org
	  (set! output (append output (list (original (car work)))))
	  (set! work (cdr work))]
	 
	 [(equal? res "timeout")
	  (if (> (length buffer) 1)
	      (sliding-window (take buffer (sub1 (length buffer))))
	      (begin
		(set! output (append output (list (car work))))
		(set! work (cdr work))))]
	 
	 [else
	  (set! output (append output res))
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
	     (set! output (append (list (optimize-inner i))))]
	    
	    [(> (+ size (item-size i)) length-limit)
	     (optimize-slide work)
	     (set! work (append work (list (item-x i))))]
	    
	    [else
	     (set! work (append work (list (item-x i))))]))
      
      (until-empty)
      output)
    ;; END sliding-window
    
    (define (optimize-inner code)
      (define x (item-x code))
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
	(label (label-name func) (optimize-inner (wrap (label-body func))) (label-info func))
	func))
  
  ;; optimize body
  (map optimize-func (program-code program)))
       
(define x
  (program
   (list
    (vardecl '(32867 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (label "sumrotate"
      ;; linklist
      (list 
        (block
          "dup"
          "dup" #f)
        (block
          "right b! !b"
          "right b! !b" #f)
        (block
          "drop"
          "drop" #f)
	) #f)
    (label "main"
      (list
       (forloop 
	;; linklist
	(list 
	 (block
	  "15"
	  "15" #f)
	 )
	;; linklist
	(list 
	 (block
	  "dup"
	  "dup" #f)
	 (block
	  "b! @b"
	  "b! @b" #f)))
	(call "sumrotate")
       ) #f)
    )
   0 #f #f))

(optimize x)
