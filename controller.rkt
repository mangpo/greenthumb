#lang s-exp rosette

(require "f18a.rkt" "state.rkt" "ast.rkt")
(require rosette/solver/z3/z3)

(define (superoptimize spec sketch info constraint
                       #:bit [bit 6]
                       #:assume [assumption (default-state)])
  (current-solver (new z3%))
  (configure [bitwidth bit])
  (define start-state (default-state info (sym-input)))
  ;; (set! spec (inst-string->list spec))
  ;; (set! sketch (inst-string->list sketch))

  (define (compare-spec-sketch)
    ;(pretty-display ">>>>>>>>>>> START >>>>>>>>>>>>>")
    ;(display-state start-state)
    (pretty-display "interpret spec")
    (define spec-state (interpret bit spec start-state))
    (pretty-display "interpret sketch")
    (define sketch-state (interpret bit sketch start-state spec-state))
    
    (pretty-display ">>>>>>>>>>> SPEC >>>>>>>>>>>>>")
    (display-state spec-state)
    (pretty-display ">>>>>>>>>>> SKETCH >>>>>>>>>>>>>")
    (display-state sketch-state)
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
  
  (traverse sketch 
            (lambda (x) (and (list? x) 
                             (or (empty? x) (pair? (car x)))))
            (lambda (x) (list->inst-string x model)))
  )

(define t (current-seconds))

(superoptimize (traverse "63 and" string? inst-string->list)
               (traverse "@p and" string? inst-string->list)
               (cons 0 0)
               (constraint s t))

;; (print-program
;; (superoptimize (traverse (list (block "-3" #f #f #f) (-iftf "1" "2")) string? inst-string->list)
;;                (traverse (list (block "_" #f #f #f) (-iftf "1" "2")) string? inst-string->list)
;;                (cons 0 0)
;;                (constraint t)))
;; (print-program
;; (superoptimize (traverse (forloop "3" "0") string? inst-string->list)
;;                (traverse (forloop "3" "dup dup or") string? inst-string->list)
;;                (cons 0 0)
;;                (constraint memory r s t)))


;; (superoptimize "a 277 b! dup or a! @+ !b @+ !b @+
;; 277 a! ! 3 b! @b ! 0 b! @b !
;; 277 b! @b 0 b! !b 1 b! @b 277 b! !b 277 b! @b 1" 
;; "a 277 b! dup or a! @+ !b @+ !b @+
;; 277 a! ! 3 b! @b ! 0 b! @b !
;; _ _ _ _ _ _ _ _" 
;;                (cons 4 2)
;;                (constraint memory s t))

;; (superoptimize "325 b! !b 277 b! !b 373 b! !b 469 b! !b" "_ _ _ _ _ _ _ _ _ _ _ _" 
;;                (cons 0 0)
;;                (constraint memory r s t))
;; (superoptimize "5 b! !b 373 b! @b 5 b! @b 277 b! !b" "_ _ _ _ _ _ _ _ _ _" 
;;                (cons 6 1)
;;                (constraint memory s t))
;; (superoptimize "4 a! !+ 4 b! @b 373 b! @b +" "_ _ _ _ _ _ _ _" 
;;                (cons 5 1)
;;                (constraint memory s t))
;; (superoptimize "2 b! @b 277 b! !b 1 b! @b 277 b! !b" "_ _ _ _ _ _ _ _" 
;;                (cons 3 0)
;;                (constraint memory s t))
;; (superoptimize "1 2 3 4" 
;;                "_" 
;;                (cons 0 2)
;;                (constraint [data 2] memory r s t)
;;                #:assume (constrain-stack '((= . 1) (= . 2) (= . 3)))
;;                )
(pretty-display `(time ,(- (current-seconds) t)))