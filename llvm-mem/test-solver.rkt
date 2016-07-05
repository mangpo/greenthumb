#lang s-exp rosette

(require "llvm-mem-validator.rkt" "llvm-mem-machine.rkt"
         "llvm-mem-printer.rkt"
         "llvm-mem-simulator-rosette.rkt" 
         "llvm-mem-parser.rkt" "../inst.rkt")

(require rosette/solver/smt/z3)

(current-solver (new z3%))

(define parser (new llvm-mem-parser% [compress? #f]))
(define machine (new llvm-mem-machine% [config 4] [bitwidth 32]))
(define printer (new llvm-mem-printer% [machine machine]))
(define simulator-rosette (new llvm-mem-simulator-rosette% [machine machine]))
(define validator (new llvm-mem-validator% [machine machine] [simulator simulator-rosette]))

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
