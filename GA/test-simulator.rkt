#lang racket

(require "GA-parser.rkt" "GA-printer.rkt" "GA-machine.rkt" "GA-validator.rkt"
         "GA-simulator-racket.rkt")

(define parser (new GA-parser%))
(define machine (new GA-machine%))
(send machine set-config 4)
(define printer (new GA-printer% [machine machine]))
(define simulator-racket (new GA-simulator-racket% [machine machine]))

(define code (send parser ast-from-string 
                   "2 b! @b"))
(define encoded-code (send printer encode code))
(send printer print-struct encoded-code)


(define input
   (cdar (send machine get-states-from-file "data-fff/inputs")))
;(define output
;  (send simulator-racket interpret encoded-code input #:dep #f))
;(send machine display-state output)

(define (my-get-stack stack i)
  (define-syntax-rule (modulo- x y) (if (< x 0) (+ x y) x))
  (vector-ref (stack-body stack) (modulo- (- (stack-sp stack) i) 8)))

(define (stack->vector x)
  (if (stack? x)
      (for/vector ([i 8]) (my-get-stack x i))
      x))

(define (stack->vector2 x)
  (if (stack? x)
      (let* ([lst (vector->list (stack-body x))]
             [p (add1 (stack-sp x))]
             [a (reverse (take lst p))]
             [b (reverse (drop lst p))])
        (list->vector (append a b)))
      x))

(define t (current-milliseconds))
;(send simulator-racket interpret encoded-code input #:dep #f)
(for ([i 100000])
  (stack->vector2 (stack 0 (vector 0 1 2 3 4 5 6 7))))
(pretty-display `(time ,(- (current-milliseconds) t))) ;203 vs 80

;(define live-in
;  (send solver get-live-in encoded-code (constraint (data 0) s t memory) 0))


#|
(define (sym-input)
  (define-symbolic* input number?)
   input)
(define state (default-state machine 1 sym-input))
(send machine display-state (send simulator-rosette interpret encoded-sketch state #:dep #f))
|#

#|
(define inputs 
   (map cdr (send machine get-states-from-file "data-fff/inputs")))


(define states1 
  (for/list ([input inputs])
    (send simulator interpret encoded-code input #:dep #t)))

(define states2 
  (for/list ([input inputs])
    (send simulator interpret encoded-sketch input #:dep #f)))


;(send stochastic correctness-cost final-state final-state2 (constraint t memory))

(define total-cost
  (for/sum ([state1 states1]
            [state2 states2]
            [id (length states1)])
    (let ([cost
           (send stochastic correctness-cost state1 state2 
                 (constraint t a memory))])
      (pretty-display "expect=")
      (send machine display-state state1)
      (pretty-display "output=")
      (send machine display-state state2)
      (pretty-display `(cost ,cost ,id))
      cost)))

(pretty-display `(total-cost ,total-cost))|#
