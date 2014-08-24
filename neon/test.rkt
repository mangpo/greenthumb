#lang racket

(require "neon-parser.rkt" "neon-printer.rkt" "neon-machine.rkt" 
         "neon-simulator-rosette.rkt" "neon-solver.rkt")

(define parser (new neon-parser%))
(define machine (new neon-machine%))
(send machine set-config (list 24 1 1))
(define printer (new neon-printer% [machine machine]))
(define simulator (new neon-simulator-rosette% [machine machine]))
(define solver (new neon-solver% [machine machine] [printer printer]))

(define code
(send parser ast-from-string "
vhadd.s16	q4, q0, q2
vhsub.s16	q5, q0, q2
vhadd.s16	q6, q1, q3
vhsub.s16	q7, q1, q3
vhsub.s16	q10, q4, q6
vhadd.s16	q8, q4, q6 
vhadd.s16	d18, d10, d15
vhsub.s16	d19, d11, d14
vhsub.s16	d22, d10, d15
vhadd.s16	d23, d11, d14
vtrn.16	q8, q9
vtrn.16	q10, q11
vtrn.32	q8, q10
vtrn.32	q9, q11
"))

(define code2
  (send parser ast-from-string "?"))

(define encoded-code (send printer encode code))
(define encoded-code2 (send solver encode-sym code2))

(send printer print-struct encoded-code)

;(print-struct code)
;(print-struct encoded-code)

;(define state (struct-copy progstate (default-state (random 10)) [rregs (vector 0 0 0 0)]))
;(define state (default-state 0))

(define state (progstate 
               (vector 
                93 155 84 222 91 252 99 139
                33 19 86 101 42 106 10 72
                182 137 12 130 109 180 107 62
                214 104 221 239 48 121 20 223
                159 63 144 153 216 32 109 213
                140 83 173 255 10 43 247 144
                237 235 204 49 19 151 30 124
                250 210 75 117 220 199 189 3
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                )
               (make-vector (send machine get-nregs-r) 0)
               (list->vector (range (* 8 (send machine get-nmems))))))
;(display-state state)
;(send simulator performance-cost encoded-code)
(send machine display-state 
      (send simulator interpret encoded-code state)
      )