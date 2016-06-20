#lang s-exp rosette

(require "llvm-demo-validator.rkt" "llvm-demo-machine.rkt"
         "llvm-demo-printer.rkt"
         "llvm-demo-simulator-rosette.rkt" 
         "llvm-demo-parser.rkt" "../inst.rkt")

;(require rosette/solver/smt/z3)

;(current-solver (new z3%))
(current-bitwidth 32)

(define parser (new llvm-demo-parser% [compress? #f]))
(define machine (new llvm-demo-machine% [config 15] [bitwidth 32]))
(define printer (new llvm-demo-printer% [machine machine]))
(define simulator-rosette (new llvm-demo-simulator-rosette% [machine machine]))
(define validator (new llvm-demo-validator% [machine machine] [simulator simulator-rosette]))

(define code
(send parser ir-from-string "
%1 = mul i32 %1, 9
%2 = mul i32 %2, 6
%3 = mul i32 %3, 3
%out = add i32 %1, %2
%out = add i32 %3, %out
"))

(define sketch
(send parser ir-from-string "
%2 = add i32 %1, %2
%2 = shl i32 %2, 1
%1 = add i32 %2, %1
%out = add i32 %1, %3
%2 = shl i32 %out, 1
%out = add i32 %out, %2
")) ;; 5 vars are enough.

#;(define sketch
(send parser ir-from-string "
%out = add i32 %2, %3
%out = shl i32 %out, 1
%5 = shl i32 %2, 2
%out = add i32 %out, %5
%out = add i32 %out, %3
%out = add i32 %out, %1
%1 = shl i32 %1, 3
%out = add i32 %out, %1
")) ;; 5 vars are enough.



(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))
(send printer print-syntax (send printer decode encoded-code))
(send printer print-struct encoded-code)
(pretty-display "-------------------")
(send printer print-syntax (send printer decode encoded-sketch))
(send printer print-struct encoded-sketch)

(define t1 (current-seconds))
(define ex 
  (send validator counterexample encoded-code encoded-sketch 
        (send printer encode-live '(%out))))
(define t2 (current-seconds))

(pretty-display "Counterexample:")
(if ex 
  (send machine display-state ex)
  (pretty-display "No"))
(newline)
(pretty-display `(time ,(- t2 t1)))
