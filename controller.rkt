#lang s-exp rosette

(require "f18a.rkt" "state.rkt")


(define (superoptimize spec sketch mem constraint)
  (define start-state (default-state mem (sym-input)))
  (configure [bitwidth 18])
  (set! spec (inst-string->list spec))
  (set! sketch (inst-string->list sketch))

  (define (compare-spec-sktech)
    (pretty-display "interpret spec")
    (define-values (spec-state spec-comm-data spec-comm-type)
      (interpret spec start-state))
    (pretty-display "interpret sketch")
    ;(pretty-display ">>>>>>>>>>> START >>>>>>>>>>>>>")
    ;(display-state start-state)
    (define-values (sketch-state sketch-comm-data sketch-comm-type)
      (interpret sketch start-state))
    
    #|
    (pretty-display ">>>>>>>>>>> SPEC >>>>>>>>>>>>>")
    (display-state spec-state)
    (pretty-display ">>>>>>>>>>> SKETCH >>>>>>>>>>>>>")
    (display-state sketch-state)
    (pretty-display ">>>>>>>>>>> FORALL >>>>>>>>>>>>>")
    (pretty-display (get-sym-vars start-state))|#
    (pretty-display "check output")
    (assert-output spec-state sketch-state constraint))

  (define model 
    (synthesize 
     #:forall (get-sym-vars start-state)
     #:guarantee (compare-spec-sktech)))
  
  (list->inst-string sketch model))

(define t (current-seconds))
(superoptimize "0 b! @b 3 b!" "_ _ _ _ _" 4 (constraint memory s t b))
;; (superoptimize "0 b! @b 3 b! !b" "_ _ _ _ _ _" 4 (constraint memory s t b))
;; 2 b! @b 3 b! !b 1 b! @b 2 b! !b
(- (current-seconds) t)