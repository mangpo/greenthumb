#lang s-exp rosette

(require "llvm-demo-validator.rkt" "llvm-demo-machine.rkt"
         "llvm-demo-printer.rkt"
         "llvm-demo-simulator-rosette.rkt" 
         "llvm-demo-parser.rkt" "../inst.rkt")

(require rosette/solver/smt/z3)

(current-solver (new z3%))

(define parser (new llvm-demo-parser% [compress? #t]))
(define machine (new llvm-demo-machine% [config 15] [bitwidth 32]))
(define printer (new llvm-demo-printer% [machine machine]))
(define simulator-rosette (new llvm-demo-simulator-rosette% [machine machine]))
(define validator (new llvm-demo-validator% [machine machine] [simulator simulator-rosette]))

(define code
(send parser ir-from-string "
  %1 = add nsw i32 %in, -1
  %2 = ashr i32 %1, 1
  %3 = or i32 %2, %1
  %4 = ashr i32 %3, 2
  %5 = or i32 %4, %3
  %6 = ashr i32 %5, 4
  %7 = or i32 %6, %5
  %8 = ashr i32 %7, 8
  %9 = or i32 %8, %7
  %10 = ashr i32 %9, 16
  %11 = or i32 %10, %9
  %out = add nsw i32 %11, 1
"))


(define sketch
(send parser ir-from-string "
%9 = sub i32 %in, 1
%9 = ctlz i32 %9
%out = lshr i32 -1, %9
%9 = lshr i32 %out, %9
%9 = or i32 %9, %out
%out = ashr i32 %9, 16
%out = add i32 %9, 1
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
