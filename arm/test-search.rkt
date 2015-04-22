#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt" "arm-simulator-rosette.rkt" 
         "arm-enumerative.rkt" "arm-symbolic.rkt" "arm-stochastic.rkt" "arm-psql.rkt")


(define time (new time% [total-start (current-seconds)]))
(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 2 0 4))
(define printer (new arm-printer% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))
(define validator (new arm-validator% [machine machine] [printer printer] [simulator simulator-rosette]))
(define enum (new arm-enumerative% [machine machine] [printer printer] [parser parser]))
(define symbolic (new arm-symbolic% [machine machine] [printer printer] [parser parser]))
(define stoch (new arm-stochastic% [machine machine] [printer printer] [parser parser] [syn-mode #t]))
(define stitch (new arm-psql% [machine machine] [printer printer] [time time]))

(define prefix
(send parser ast-from-string "
"))

(define postfix
(send parser ast-from-string "
"))

(define code
(send parser ast-from-string "
orr r1, r0, r0, lsl 1
sub r0, r0, r1
"))

;orr r1, r0, r0, lsl 1
;sub r0, r0, r1
;sub r0, r0, 1

;sub r0, r0, r1
;orr r0, r0, r1

(define sketch
(send parser ast-from-string "
? ?
"))

(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))
(define encoded-code (send printer encode code))
(define encoded-sketch (send validator encode-sym sketch))

(define t (current-seconds))
(send enum synthesize-window
      encoded-code ;; spec
      encoded-sketch ;; sketch = spec in this case
      encoded-prefix encoded-postfix
      (constraint machine [reg 0] [mem]) #f #f 3600)
#|(send stoch superoptimize encoded-code 
      (constraint machine [reg 0] [mem]) ;; constraint
      (constraint machine [reg 0] [mem]) ;; live-in
      "./driver-0" 3600 #f)|#
(pretty-display `(time ,(- (current-seconds) t)))
