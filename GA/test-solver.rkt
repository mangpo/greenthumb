#lang s-exp rosette

(require "GA-validator.rkt" "GA-machine.rkt" "GA-printer.rkt" "GA-simulator-rosette.rkt" 
         "GA-parser.rkt")

(define parser (new GA-parser%))
(define machine (new GA-machine%))
(send machine set-config 4)
(define printer (new GA-printer% [machine machine]))
(define simulator-rosette (new GA-simulator-rosette% [machine machine]))
(define validator (new GA-validator% [machine machine] [printer printer] [simulator simulator-rosette]))

(define code
(send parser ast-from-string 
      "0 a! !+ !+ push pop dup 1 b! @b and over 0 b! @b and or 1 b! @b 0 b! @b and or push drop pop"))

(define sketch (send parser ast-from-string 
      "over over and push or and pop or "))
;2/ dup a! over dup over dup drop a 2/ 325 a! @+ + 65535 and
;size = 16, 
;opt = 10
; 6 ?, no sol = 13
; 8 ? => 16 s, no sol > 5 min
; 9 ? => 72 s
; 10 ? => > 2s, 

(define encoded-code (send printer encode code))
(define encoded-sketch (send validator encode-sym sketch))

(define ce
  (send validator counterexample encoded-code encoded-sketch 
        (constraint r s t)
        0))
(send machine display-state ce)
(send machine display-state (send simulator-rosette interpret encoded-code ce #:dep #f))
(send machine display-state (send simulator-rosette interpret encoded-sketch ce #:dep #f))

#|
(define t (current-seconds))
(with-handlers* 
 ([exn:fail? 
   (lambda (e) (pretty-display (exn-message e)))])
 (send solver synthesize-from-sketch encoded-code encoded-sketch
       (constraint s t r) 1))
(pretty-display `(time ,(- (current-seconds) t)))|#
