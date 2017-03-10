#lang rosette

(require "llvm-validator.rkt" "llvm-machine.rkt"
         "llvm-printer.rkt"
         "llvm-simulator-rosette.rkt" 
         "llvm-parser.rkt" "../inst.rkt")

(define parser (new llvm-parser%))
(define machine (new llvm-machine% [config (cons 3 3)] [bitwidth 32]))
(define printer (new llvm-printer% [machine machine]))
(define simulator-rosette (new llvm-simulator-rosette% [machine machine]))
(define validator (new llvm-validator% [machine machine] [simulator simulator-rosette]
                       [printer printer]))

(define code
(send parser ir-from-string "
%1 = lshr i32 %in, 3
%out = shl nuw i32 %1, 3
"))


(define sketch
(send parser ir-from-string "
%out = and i32 %in, -8
"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))
(send printer print-syntax (send printer decode encoded-code))
(send validator adjust-memory-config encoded-code)

(define ex 
  (send validator counterexample encoded-code encoded-sketch 
        (send printer encode-live (vector '(%out) '() #f))))

(pretty-display "Counterexample:")
(if ex 
  (send machine display-state ex)
  (pretty-display "No"))
(newline)
