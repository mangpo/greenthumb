#lang s-exp rosette

(require "llvm-validator.rkt" "llvm-machine.rkt"
         "llvm-printer.rkt"
         "llvm-simulator-rosette.rkt" 
         "llvm-parser.rkt" "../inst.rkt")

(require rosette/solver/smt/z3)

(current-solver (new z3%))

(define parser (new llvm-parser% [compress? #f]))
(define machine (new llvm-machine% [config 4] [bitwidth 32]))
(define printer (new llvm-printer% [machine machine]))
(define simulator-rosette (new llvm-simulator-rosette% [machine machine]))
(define validator (new llvm-validator% [machine machine] [simulator simulator-rosette]))

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
        (send printer encode-live (vector '(%out) #t))))

(pretty-display "Counterexample:")
(if ex 
  (send machine display-state ex)
  (pretty-display "No"))
(newline)
