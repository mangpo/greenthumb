#lang racket

(require "neon-parser.rkt" "neon-printer.rkt" "neon-machine.rkt" "neon-simulator-rosette.rkt" "neon-solver.rkt")

(define parser (new neon-parser%))
(define machine (new neon-machine%))
(define printer (new neon-printer% [machine machine]))
(define simulator (new neon-simulator-rosette% [machine machine]))
(define solver (new neon-solver% [machine machine] [printer printer]))

(define code
(send parser ast-from-string "
vext.16 d3, d0, d1, #1
"))

(define code2
  (send parser ast-from-string "? ? ?"))

(define encoded-code (send printer encode code))
(define encoded-code2 (send solver encode-sym code2))

;(print-struct code)
;(print-struct encoded-code)

;(define state (struct-copy progstate (default-state (random 10)) [rregs (vector 0 0 0 0)]))
;(define state (default-state 0))
#|
(define state (progstate 
               (vector 
                0 0 0 0 0 0 0 0
                0 1 2 3 4 5 6 7
                8 9 11 22 33 44 55 66
                225 77 15 108 0 0 0 0
                170 50 212 200 49 61 2 26
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                255 255 255 255 255 255 255 255
                27 1 0 0 0 0 0 0
                )
               (make-vector nregs-r 0)
               (list->vector (range (* 8 nmems)))
               0))|#
;(display-state state)
(send simulator performance-cost encoded-code)