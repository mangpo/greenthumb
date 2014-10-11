#lang racket

(require "main.rkt" "arm-parser.rkt" "arm-compress.rkt")

(define code (send (new arm-parser%) ast-from-string "
str r0, r4, -16
str r1, r4, -20
str r2, r4, -24
ldr r2, r4, -16
ldr r3, r4, -24
mov r3, r2, asr r3
str r3, r4, -12
ldr r2, r4, -12
ldr r3, r4, -16
eor r3, r2, r3
str r3, r4, -12
ldr r2, r4, -12
ldr r3, r4, -20
and r3, r2, r3
str r3, r4, -12
ldr r2, r4, -12
ldr r3, r4, -24
mov r3, r2, lsl r3
str r3, r4, -8
ldr r2, r4, -8
ldr r3, r4, -12
eor r3, r2, r3
str r3, r4, -8
ldr r2, r4, -8
ldr r3, r4, -16
eor r3, r2, r3
mov r0, r3
"))
                   

(arm-generate-inputs code (list 6 8) "input_p19_r6_m8")
