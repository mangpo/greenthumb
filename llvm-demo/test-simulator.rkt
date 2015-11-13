#lang s-exp rosette

(require "llvm-demo-parser.rkt" "llvm-demo-printer.rkt" "llvm-demo-machine.rkt")

(current-bitwidth 32)
(define parser (new llvm-demo-parser%))
(define machine (new llvm-demo-machine% [config 4]))
(define printer (new llvm-demo-printer% [machine machine]))

;; Section 1: Concrete program

(define code
(send parser ast-from-string "
%1 = lshr i32 %in, 3
%out = shl nuw i32 %1, 3
"))

(send printer print-struct code)
(send printer print-syntax code)
