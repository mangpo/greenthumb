#lang rosette

(require "llvm-validator.rkt" "llvm-machine.rkt"
         "llvm-printer.rkt"
         "llvm-simulator-rosette.rkt" 
         "llvm-parser.rkt" "../inst.rkt")

(define parser (new llvm-parser%))
(define machine (new llvm-machine% [config (cons 4 0)] [bitwidth 32]))
(define printer (new llvm-printer% [machine machine]))
(define simulator-rosette (new llvm-simulator-rosette% [machine machine]))
(define validator (new llvm-validator% [machine machine] [simulator simulator-rosette]
                       [printer printer]))

(define code
(send parser ir-from-string "
%0 = mul i32 %1, -1
%2 = add i32 %3, %0
%2 = add i32 %2, 1
%2 = mod i32 %2, 6
%2 = udiv i32 %2, 2
%0 = mul i32 %0, 3
%3 = mul i32 %3, 3
%0 = add i32 %0, %2
%0 = add i32 %0, %3
%0 = mod i32 %0, 6
"))


(define sketch
(send parser ir-from-string "
%0 = mul i32 %1, -1
%2 = add i32 %3, %0
%2 = add i32 %2, 1
%2 = mod i32 %2, 6
%2 = udiv i32 %2, 2
%0 = mul i32 %0, 3
%3 = mul i32 %3, 3
%0 = add i32 %0, %2
%0 = add i32 %0, %3
%0 = mod i32 %0, 6
"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))
(send printer print-syntax (send printer decode encoded-code))
(send validator adjust-memory-config encoded-code)

(define assume (vector (vector #f (list (cons >= 0) (cons < 32)) #f (list (cons >= 0) (cons < 6)))
                       #f #f))

(define ex 
  (send validator counterexample encoded-code encoded-sketch 
        (send printer encode-live (vector '(%0) '() #f))
        #:assume assume
        ))

(pretty-display "Counterexample:")
(if ex 
  (send machine display-state ex)
  (pretty-display "No"))
(newline)
