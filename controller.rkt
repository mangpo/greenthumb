#lang s-exp rosette

(require "f18a.rkt" "state.rkt")

(define (superoptimize spec sketch info constraint #:assume [assumption (default-state)])
  (define start-state (default-state info (sym-input)))
  (configure [bitwidth 18])
  (set! spec (inst-string->list spec))
  (set! sketch (inst-string->list sketch))

  (define (compare-spec-sketch)
    ;(pretty-display ">>>>>>>>>>> START >>>>>>>>>>>>>")
    ;(display-state start-state)
    (pretty-display "interpret spec")
    (define spec-state (interpret spec start-state))
    (pretty-display "interpret sketch")
    (define sketch-state (interpret sketch start-state spec-state))
    
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
  
  (list->inst-string sketch model)
  )

(define t (current-seconds))
;; (superoptimize "a 277 b! dup or a! @+ !b @+ !b @+
;; 277 a! ! 3 b! @b ! 0 b! @b !
;; 277 b! @b 0 b! !b 1 b! @b 277 b! !b 277 b! @b 1" 
;; "a 277 b! dup or a! @+ !b @+ !b @+
;; 277 a! ! 3 b! @b ! 0 b! @b !
;; _ _ _ _ _ _ _ _" 
;;                (cons 4 2)
;;                (constraint memory s t))

(superoptimize "0" "_ _ _" 
               (cons 0 0)
               (constraint memory r s t))

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
(- (current-seconds) t)