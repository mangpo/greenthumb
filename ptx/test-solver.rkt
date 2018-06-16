#lang s-exp rosette

(require "ptx-validator.rkt" "ptx-machine.rkt"
         "ptx-printer.rkt"
         "ptx-simulator-rosette.rkt" 
         "ptx-parser.rkt" "../inst.rkt")

(require rosette/solver/smt/z3)

(current-solver (new z3%))

(define parser (new ptx-parser% [compress? #f]))
(define machine (new ptx-machine% [config (cons 4 0)] [bitwidth 32]))
(define printer (new ptx-printer% [machine machine]))
(define simulator-rosette (new ptx-simulator-rosette% [machine machine]))
(define validator (new ptx-validator% [machine machine] [simulator simulator-rosette]))

(define code
(send parser ir-from-string "
and.u32 %r0, 31;
	mul.wide.u32 	%r2, %r1, %r0, -1431655765;
	shr.u32 	%r1, %r1, 1;
	mul.lo.s32 	%r1, %r1, 3;
	sub.s32 	%r1, %r0, %r1;
"))


(define sketch
(send parser ir-from-string "
and.u32 %r0, 31;
rem.u32 %r1, %r0, 3;
"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))
(send printer print-syntax (send printer decode encoded-code))

(define ex 
  (send validator counterexample encoded-code encoded-sketch 
        (progstate (vector #t #t #f #f) (vector))))

(pretty-display "Counterexample:")
(if ex 
  (send machine display-state ex)
  (pretty-display "No"))
(newline)
