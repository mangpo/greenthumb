#lang racket

(require "main.rkt" "arm-parser.rkt")

(define code (send (new arm-parser%) ir-from-string "
	eor	r0, r0, r0, asr #1
	movw	r3, #4369
	movt	r3, 4369
	eor	r0, r0, r0, asr #2
	and	r3, r0, r3
	add	r3, r3, r3, asl #4
	add	r3, r3, r3, asl #8
	add	r0, r3, r3, asl #16
	ubfx	r0, r0, #28, #1
"))
                   

(arm-generate-inputs code (list 5 1) "input_r5_m1")
