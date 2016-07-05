#lang s-exp rosette

(require "llvm-demo-validator.rkt" "llvm-demo-machine.rkt"
         "llvm-demo-printer.rkt"
         "llvm-demo-simulator-rosette.rkt" 
         "llvm-demo-parser.rkt" "../inst.rkt")

(require rosette/solver/smt/z3)

(current-solver (new z3%))

(define parser (new llvm-demo-parser% [compress? #f]))
(define machine (new llvm-demo-machine% [config 4] [bitwidth 32]))
(define printer (new llvm-demo-printer% [machine machine]))
(define simulator-rosette (new llvm-demo-simulator-rosette% [machine machine]))
(define validator (new llvm-demo-validator% [machine machine] [simulator simulator-rosette]))

(define code
(send parser ir-from-string "
%out = load i32, i32* %1
"))


(define sketch
(send parser ir-from-string "
%out = shl i32 0, %2
"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))
(send printer print-syntax (send printer decode encoded-code))

(define ex 
  (send validator counterexample encoded-code encoded-sketch 
        (send printer encode-live '(%out))))

(pretty-display "Counterexample:")
(if ex 
  (send machine display-state ex)
  (pretty-display "No"))
(newline)
