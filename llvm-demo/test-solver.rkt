#lang s-exp rosette

(require "llvm-demo-validator.rkt" "llvm-demo-machine.rkt"
         "llvm-demo-printer.rkt"
         "llvm-demo-simulator-rosette.rkt" 
         "llvm-demo-parser.rkt" "../ast.rkt")

(require rosette/solver/smt/z3)

(current-solver (new z3%))

(define parser (new llvm-demo-parser%))
(define machine (new llvm-demo-machine% [config 3] [bit 4]))
(define printer (new llvm-demo-printer% [machine machine]))
(define simulator-rosette (new llvm-demo-simulator-rosette% [machine machine]))
(define validator (new llvm-demo-validator% [machine machine] [printer printer]
                       [simulator simulator-rosette]))

(define code
(send parser ast-from-string "
%1 = lshr i32 %in, 1
%out = shl nuw i32 %1, 1
"))


(define sketch
(send parser ast-from-string "
%out = and i32 %in, -2
"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send validator encode-sym sketch))

(define ex 
  (send validator counterexample encoded-code encoded-sketch 
        (send printer encode-live '(%out))))

(pretty-display "Counterexample:")
(if ex 
  (send machine display-state ex)
  (pretty-display "No"))
(newline)
