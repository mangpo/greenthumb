#lang s-exp rosette

(require "ptx-parser.rkt" "ptx-printer.rkt" "ptx-machine.rkt" "../validator.rkt"
         "ptx-simulator-rosette.rkt" "../memory-racket.rkt"
         "ptx-simulator-racket.rkt"
         "ptx-validator.rkt" "ptx-symbolic.rkt"
         )

;; Phase 0: Set up bitwidth for Rosette
(current-bitwidth 32)

;; Phase A: Test machine, parser, printer
(pretty-display "Phase A: test machine, parser, and printer.")
(define parser (new ptx-parser%))
(define machine (new ptx-machine% [config (cons 16 1)]))
(define printer (new ptx-printer% [machine machine]))

(define code
(send parser ir-from-string "
	mul.wide.u32 	%r2, %r1, %r0, -1431655765;
	shr.u32 	%r1, %r1, 1;
	mul.lo.s32 	%r1, %r1, 3;
	sub.s32 	%r1, %r0, %r1;
	and.b32  	%r2, %r1, 1;
	setp.eq.b32	%p0, %r2, 0;
	selp.b32	%r10, %r4, %r6, %p0;
	selp.b32	%r11, %r5, %r7, %p0;
	selp.b32	%r12, %r6, %r8, %p0;
	selp.b32	%r13, %r7, %r9, %p0;
	selp.b32	%r14, %r8, %r4, %p0;
	selp.b32	%r15, %r9, %r5, %p0;
	and.b32  	%r2, %r1, 2;
	setp.eq.s32	%p0, %r2, 0;
	selp.b32	%r4, %r10, %r14, %p0;
	selp.b32	%r5, %r11, %r15, %p0;
	selp.b32	%r6, %r12, %r10, %p0;
	selp.b32	%r7, %r13, %r11, %p0;
	selp.b32	%r8, %r14, %r12, %p0;
	selp.b32	%r9, %r15, %r13, %p0;
"))

#|
	mul.wide.u32 	%r2, %r1, %r0, -1431655765;
	shr.u32 	%r1, %r1, 1;
	mul.lo.s32 	%r1, %r1, 3;
	sub.s32 	%r0, %r0, %r1;

rem.u32 %r0, %r0, 3;
|#

(pretty-display ">>> Source")
(send printer print-syntax code)

(pretty-display ">>> String-IR")
(send printer print-struct code)

(pretty-display ">>> Encoded-IR")
(define encoded-code (send printer encode code))
(send printer print-struct encoded-code)
(newline)

;; Phase B: Interpret concrete program with concrete inputs
(pretty-display "Phase B: interpret program using simulator writing in Rosette.")
;; define number of bits used for generating random test inputs
(define test-bit 4)
;; create random input state
#;(define input-state (send machine get-state (get-rand-func test-bit)))
;; define our own input test, but memory content is random
;; modify program state to match your program state structure
(define input-state (progstate (vector 23 0 0 0 1 2 3 4 5 6 0 0 0 0 0 0)
                               (vector 5)))
(define simulator-rosette (new ptx-simulator-rosette% [machine machine]))
(pretty-display `(input ,input-state))
(pretty-display `(output ,(send simulator-rosette interpret encoded-code input-state)))
(newline)

;; Phase C: Interpret concrete program with symbolic inputs
(pretty-display "Phase C: interpret concrete program with symbolic inputs.")
(define input-state-sym (send machine get-state sym-input))
(pretty-display `(input ,input-state-sym))
(pretty-display `(output ,(send simulator-rosette interpret encoded-code input-state-sym)))
(newline)

;; Phase D: Duplicate rosette simulator to racket simulator
(pretty-display "Phase D: interpret program using simulator writing in Racket.")
(define simulator-racket (new ptx-simulator-racket% [machine machine]))
(send simulator-racket interpret encoded-code input-state)
(newline)

;; Phase E: Interpret symbolic program with symbolic inputs
(pretty-display "Phase E: interpret symbolic program.")
(define validator (new ptx-validator% [machine machine] [simulator simulator-rosette]))
(define symbolic (new ptx-symbolic% [machine machine] [printer printer]
                      [parser parser]
                      [validator validator] [simulator simulator-rosette]))
(define sym-code (for/vector ([i 2]) (send symbolic gen-sym-inst)))
(send simulator-rosette interpret sym-code input-state-sym)
