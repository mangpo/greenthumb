#lang racket

(provide (all-defined-out))

(struct inst (op args)) ;; machine-dependent
(struct block (body org info))
;; info may include output constraint and # of recv data depending on arch
(struct call (name))
(struct label (name body info)) 
;; info may include size of preserved stack and precondition depeding on arch
(struct vardecl (val))
(struct forloop (init body bound))
(struct ift (t))     ;; exit if cond = 0
(struct iftf (t f))  ;; jump if cond = 0
(struct -ift (t))    ;; exit if cond >= 0
(struct -iftf (t f)) ;; jump if cond >= 0
(struct program (code memsize indexmap))
(struct special (name))
(struct assumption (cnstr))
(struct item (x size))
(struct	exn:state exn (state))

(struct node (val size p))

(define (display-node x [indent ""])
  (when (node? x)
	(pretty-display (format "~anode: ~a(~a;~a)" 
                                indent (node-val x) (length (node-p x)) (node-size x)))
	(for ([i (node-p x)])
	     (display-node i (string-append indent "  ")))))


(define (create-node-ex val p init-vals)
  (define my-size (+ (if val 1 0) (for/sum ([i (filter node? p)]) (node-size i))))
  (define my-val (if (member val init-vals) #f val))
  (define my-p (list))
  (for ([x p])
       (when (node? x)
	     (if (node-val x) 
		 (set! my-p (cons x my-p))
		 (set! my-p (append (node-p x) my-p)))))
  (cond
   [(and (= (length my-p) 1) (equal? (node-val (car my-p)) my-val))
    (car my-p)]
   [else
    (node my-val my-size my-p)]))
  ;; (node my-val my-size my-p))

(define (adjust cost val dep inter [min-val 1])
  ;;(pretty-display `(adjust ,cost ,val))
  (define (dfs x)
    ;;(pretty-display `(dfs ,(node-val x)))
    (if (member (node-val x) inter)
	(node-size x)
	(for/sum ([i (node-p x)])
		 (if (node? i) (dfs i) 0))))
  
  
  (define half (max (/ cost 2) min-val))

  ;; (cond
  ;;  ;[#t cost]
  ;;  [(<= cost min-val) cost]
  ;;  ;; No computation
  ;;  [(not (node? dep)) 
  ;;   ;;(pretty-display `(no-comp))
  ;;   min-val]
  ;;  [(and (equal? (node-val dep) val) (member val inter))
  ;;   ;;(pretty-display `(cover-all))
  ;;   min-val]
  ;;  [else cost])
    (cond
   ;[#t cost]
   [(<= cost min-val) 
    ;;(pretty-display `(min-val))
    cost]
   ;; [(and (node? dep) (equal? (node-val dep) val) (member val inter))
   ;;  (pretty-display `(cover-all))
   ;;  min-val]
   [(node? dep)
    (define total (node-size dep))
    (define uncover (- total (dfs dep)))
    ;;(pretty-display `(uncover-total ,uncover ,total ,cost))
    ;;(exact->inexact (add1 (* (/ uncover total) (sub1 cost))))
    (exact->inexact (+ half (* (/ uncover total) (- cost half))))
    ]

   ;; No computation
   [else 
    ;;(pretty-display `(no-comp))
    min-val])
  )

;; Traverse a given program AST recursively until (base? program) is true.
;; Then apply base-apply to program.
(define-syntax traverse
  (syntax-rules ()
    ;; (pretty-display `(traverse ,program))
    ((traverse program [base? base-apply] ...)
     (letrec 
         ([f (lambda (x)
               ;; (pretty-display `(traverse-f ,x))
               (cond
                [(base? x)    (base-apply x)]
                ...
                [(list? x)    (map f x)]
                [(vector? x)  (list->vector (map f (vector->list x)))]
                [(block? x)   (block (f (block-body x)) (block-org x) (block-info x))]
                [(forloop? x) (forloop (f (forloop-init x)) (f (forloop-body x)) (forloop-bound x))]
                [(ift? x)     (ift (f (ift-t x)))]
                [(iftf? x)    (iftf (f (iftf-t x)) (f (iftf-f x)))]
                [(-ift? x)    (-ift (f (-ift-t x)))]
                [(-iftf? x)   (-iftf (f (-iftf-t x)) (f (-iftf-f x)))]
                [(item? x)    (f (item-x x))]
                [(label? x)   (label (label-name x) (f (label-body x)) (label-info x))]
                [(program? x) 
                 (program (f (program-code x)) (program-memsize x) (program-indexmap x))]
                [else x]
                ))])
       (f program)))

    ((traverse program base? base-apply)
     (traverse program [base? base-apply]))))

(define (last-block x)
  ;; (pretty-display `(last-block ,x))
  (cond
   [(block? x)   x]
   [(list? x)    (ormap last-block (reverse x))]
   [(forloop? x) (last-block (forloop-body x))]
   [(ift? x)     (last-block (ift-t x))]
   [(iftf? x)    (last-block (iftf-t x))]
   [(-ift? x)    (last-block (-ift-t x))]
   [(-iftf? x)   (last-block (-iftf-t x))]
   [(or (call? x) (special? x)) #f]
   [else         (raise (format "last-block: unimplemented for ~a" x))]))

;; Wrap every object inside item object and calculate its size.
(define (wrap x f)
  (define (inner x)
    (cond
     [(list? x)    
      (define lst (map inner x))
      (define size (foldl + 0 (map item-size lst)))
      (item lst size)]
     
     [(block? x)  
      (item x (f (block-body x)))]
     
     [(forloop? x)
      (define init-ret (inner (forloop-init x)))
      (define body-ret (inner (forloop-body x)))
      (item (forloop init-ret body-ret (forloop-bound x)) 
	    (if (forloop-bound x)
		(* 2 (+ (item-size init-ret) (item-size body-ret)))
		1000000))]
     
     [(ift? x)
      (define t-ret (inner (ift-t x)))
      (item (ift t-ret) (item-size t-ret))]
     
     [(iftf? x)
      (define t-ret (inner (iftf-t x)))
      (define f-ret (inner (iftf-f x)))
      (item (iftf t-ret f-ret) (+ (item-size t-ret) (item-size f-ret)))]
     
     [(-ift? x)
      (define t-ret (inner (-ift-t x)))
      (item (-ift t-ret) (item-size t-ret))]
     
     [(-iftf? x)
      (define t-ret (inner (-iftf-t x)))
      (define f-ret (inner (-iftf-f x)))
      (item (-iftf t-ret f-ret) (+ (item-size t-ret) (item-size f-ret)))]
     
     [(or (vardecl? x) (call? x) (special? x))
      (item x 1000000)]
     
     [(assumption? x)
      (item x 0)]
     
     [else (raise (format "inner: unimplemented for ~a" x))]
     ))
  (inner x))

;; Replace block-body with block-org.
(define (original program)
  (traverse program block? 
            (lambda (x) (block (block-org x) (block-org x) (block-info x)))))

(define (concrete-in x lst)
  (and (member x lst) #t))

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

(define (random-from-vec vec)
  (vector-ref vec (random (vector-length vec))))

(define (random-from-list-ex lst ex)
  (let ([new-lst (remove ex lst)])
    (if (empty? new-lst)
        ex
        (list-ref new-lst (random (length new-lst))))))

(define (random-from-vec-ex vec ex)
  (define len (vector-length vec))
  (define (inner)
    (define sample (vector-ref vec (random len)))
    (if (eq? sample ex)
        (inner)
        sample))
  (if (= len 1)
      (vector-ref vec 0)
      (inner)))
