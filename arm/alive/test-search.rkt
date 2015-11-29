#lang racket

(require "../arm-validator.rkt" "../arm-machine.rkt" "../arm-printer.rkt"
         "../arm-parser.rkt"
         "../arm-forwardbackward.rkt"
         "../arm-simulator-racket.rkt")


(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 4 0 0))

(define printer (new arm-printer% [machine machine]))
(define validator (new arm-validator% [machine machine]))
(define simulator (new arm-simulator-racket% [machine machine]))
(define backward (new arm-forwardbackward% [machine machine] 
                      [printer printer] [parser parser] 
                      [syn-mode `partial1]))

;%Y = and %Z, C1
;%X = xor %Y, C1
;%LHS = add %X, 1
;%r = add %LHS, %RHS

(define prefix 
(send parser ast-from-string "
"))

(define postfix
(send parser ast-from-string "
"))

(define code
(send parser ast-from-string "
and r0, r0, r1
eor r0, r0, r1
add r0, r0, 1
add r0, r0, r2
"))


(define sketch
(send parser ast-from-string "
? ? ? ? ?
"))

(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))
(define encoded-code (send printer encode code))
(define encoded-sketch (send validator encode-sym sketch))
(define cost (send simulator performance-cost encoded-code))

(define t (current-seconds))
(define f
  (send backward synthesize-window
        encoded-code ;; spec
        encoded-sketch ;; sketch = spec in this case
        encoded-prefix encoded-postfix
        (constraint machine [reg 0] [mem]) #f cost 3600)
  )
(pretty-display `(t ,(- (current-seconds) t)))
