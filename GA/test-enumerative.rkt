#lang s-exp rosette

(require "GA-validator.rkt" "GA-machine.rkt" "GA-printer.rkt"
         "GA-parser.rkt" "../inst.rkt"
         "GA-simulator-racket.rkt" "GA-simulator-rosette.rkt"
         "GA-symbolic.rkt" "GA-stochastic.rkt" "GA-forwardbackward.rkt"
         "GA-inverse.rkt" "GA-enumerator.rkt")


(define parser (new GA-parser%))
(define machine (new GA-machine% [config #f]))

(define printer (new GA-printer% [machine machine]))
(define simulator-racket (new GA-simulator-racket% [machine machine]))
(define simulator-rosette (new GA-simulator-rosette% [machine machine]))
(define validator (new GA-validator% [machine machine] [simulator simulator-rosette]))

(define symbolic (new GA-symbolic% [machine machine]
                      [printer printer] [parser parser]
                      [validator validator] [simulator simulator-rosette]))
(define backward (new GA-forwardbackward% [machine machine] 
                      [printer printer] [parser parser] 
                      [validator validator] [simulator simulator-racket]
                      [inverse% GA-inverse%]
                      [enumerator% GA-enumerator%]
                      [syn-mode `linear]))

(define (test id code-str size liveout-str #:sym [sym #f] #:enum [enum #t] #:assume [assume #f])
  (pretty-display (format "TEST = ~a" id))
  (define code (send parser ir-from-string code-str))
  (define encoded-code (send printer encode code))
  
  (define sketch (for/vector ([i size]) (send symbolic gen-sym-inst)))
  (define constraint (send machine output-constraint liveout-str))
  
  ;; symbolic
  (when sym
    (define-values (out-sym cost-sym)
      (send symbolic synthesize-from-sketch encoded-code sketch constraint
            #:assume (and assume (send machine constrain-stack assume))))
    
    (unless out-sym (raise (format "TEST ~a: fail to synthesize [symbolic]" id)))
    (define ce-sym (send validator counterexample encoded-code out-sym constraint
                         #:assume (and assume (send machine constrain-stack assume))))
    (when ce-sym (raise (format "TEST ~a: counter-example [symbolic]" id))))

  (when enum
    (define out-enum
      (send backward synthesize-window
            encoded-code
            sketch
            (vector) (vector)
            constraint ;; live-out
            #f ;; upperbound cost, #f = no upperbound
            3600 ;; time limit in seconds
            #:assume (and assume (send machine constrain-stack assume))
            ))
    
    (unless out-enum (raise (format "TEST ~a: fail to synthesize [enumerative]" id)))
    (define ce-enum (send validator counterexample encoded-code out-enum constraint
                          #:assume (and assume (send machine constrain-stack assume))))
    (when ce-enum (raise (format "TEST ~a: counter-example [enumerative]" id))))
  )

(test 1 "dup drop up a! @ !" 4 '((data . 1) memory))
;;(test 2 "dup drop up a! @+ !" 4 '((data . 1) memory))
(test 3 "dup drop up a! ! @" 4 '((data . 1) memory))
;;(test 4 "dup drop up a! !+ @" 4 '((data . 1) memory))
;;(test 5 "up a! !+ !" 3 '((data . 0)))
;;(test 6 "up a! @+ @" 3 '((data . 0)))
(test 7 "dup drop @+ !" 2 '((data . 1) memory))
(test 8 "dup drop !+ @" 2 '((data . 1) memory))

(test 'hhh "0 a! !+ push !+ pop 1 b! @b over or 0 b! @b or push drop pop" 2 '((data . 2)))
(test 'fff "0 a! !+ !+ push pop dup 1 b! @b and over 65535 or 0 b! @b and over - and + push drop pop"
      6 '((data . 2))
      #:assume '((<= . 65535) (<= . 65535) (<= . 65535)) #:sym #f)
(test 'ggg "0 a! push !+ !+ pop dup 1 b! @b and over 65535 or 0 b! @b and over - and + push drop pop"
      7 '((data . 2))
      #:assume '((<= . 65535) (<= . 65535) (<= . 65535)) #:sym #f)
(test 'shaf "0 a! !+ !+ push pop dup 1 b! @b and over - 0 b! @b and or push drop pop" 6
      '((data . 2) (return . 1)) #:sym #f)

#;(test 'complexB "drop 3 and + push drop pop dup 0 b! @b" 10
      '((data . 2)) #:sym #f) ;; very slow
#;(test 'rrotate "2 b! !b push drop pop 2 b! @b 0 b! !b up b! @b 0 b! @b 2/ 2/ + 65535 and" 8
      '((data . 2) (return . 1)) #:sym #f) ;; very slow