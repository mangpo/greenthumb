#lang s-exp rosette

(require "f18a.rkt" "state.rkt")

(define (superoptimize spec sketch info constraint)
  (define start-state (default-state info (sym-input)))
  (configure [bitwidth 18])
  (set! spec (inst-string->list spec))
  (set! sketch (inst-string->list sketch))

  (define (compare-spec-sketch)
    (pretty-display "interpret spec")
    (define spec-state (interpret spec start-state))
    (pretty-display "interpret sketch")
    (pretty-display ">>>>>>>>>>> START >>>>>>>>>>>>>")
    (display-state start-state)
    (define sketch-state (interpret sketch start-state))
    
    
    (pretty-display ">>>>>>>>>>> SPEC >>>>>>>>>>>>>")
    (display-state spec-state)
    (pretty-display ">>>>>>>>>>> SKETCH >>>>>>>>>>>>>")
    (display-state sketch-state)
    (pretty-display ">>>>>>>>>>> FORALL >>>>>>>>>>>>>")
    (pretty-display (get-sym-vars start-state))
    (pretty-display "check output")
    (assert-output spec-state sketch-state constraint))

  (define model 
    (synthesize 
     #:forall (get-sym-vars start-state)
     #:guarantee (compare-spec-sketch))
    )
  
  (list->inst-string sketch model)
  )

(define t (current-seconds))
(superoptimize "325 b! !b" "325 b! _" 
               (cons 1 0)
               (constraint memory r s t))
(- (current-seconds) t)