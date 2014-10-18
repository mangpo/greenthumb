#lang s-exp rosette

(require "GA-solver.rkt" "GA-machine.rkt" "GA-printer.rkt"
         "GA-parser.rkt")

(define parser (new GA-parser%))
(define machine (new GA-machine%))
(send machine set-config 4)
(define printer (new GA-printer% [machine machine]))
(define solver (new GA-solver% [machine machine] [printer printer] [parser parser] [syn-mode `partial]))

(define code
(send parser ast-from-string 
      "2 b! !b push drop pop 2 b! @b 0 b! !b up b! @b 0 b! @b 2/ 2/ + 65535 and"))

(define sketch (send parser ast-from-string 
      "? ? ? ? ? ? ?"))
;2/ dup a! over dup over dup drop a 2/ 325 a! @+ + 65535 and
;size = 16, 
;opt = 10
; 6 ?, no sol = 13
; 8 ? => 16 s, no sol > 5 min
; 9 ? => 72 s
; 10 ? => > 2s, 

(define encoded-code (send printer encode code))
(define encoded-sketch (send solver encode-sym sketch))

#|
(send solver counterexample encoded-code encoded-sketch 
      (constraint r s t)
      1)|#

#|
(define t (current-seconds))
(with-handlers* 
 ([exn:fail? 
   (lambda (e) (pretty-display (exn-message e)))])
 (send solver synthesize-from-sketch encoded-code encoded-sketch
       (constraint s t r) 1))
(pretty-display `(time ,(- (current-seconds) t)))|#

(define x
(send solver superoptimize encoded-code 
      (constraint s t r) "./foo" 3600 #f 1))