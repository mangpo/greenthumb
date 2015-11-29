#lang racket

(require "../printer.rkt" 
         "../inst.rkt")

(provide $-printer%)

(define $-printer%
  (class printer%
    (super-new)
    (inherit-field machine)
    (override encode-inst decode-inst print-syntax-inst
              ;; >> Required method for cooperative search
              ;; config-from-string-ir output-constraint-string
              )

    ;; Print in the assembly format.
    ;; x: string IR
    (define (print-syntax-inst x [indent ""]) ?)

    ;; Convert an instruction x from string-IR to encoded-IR format.
    (define (encode-inst x) ?)

    ;; Convert an instruction x from encoded-IR to string-IR format.
    (define (decode-inst x) ?)

    ;;;;;;;;;;;;;;;;;;;;;;;;; For cooperative search ;;;;;;;;;;;;;;;;;;;;;;;
    #|
    ;; Return program state config from a given program in string-IR format.
    ;; program: string IR format
    ;; output: program state config, an input to machine:set-config
    (define (config-from-string-ir program) ?)

    ;; Convert live-out (which is one of the outputs from 
    ;; parser::info-from-file) into string. 
    ;; The string will be used as a piece of code the search driver generates as
    ;; the live-out argument to the method superoptimize of 
    ;; stochastics%, forwardbackward%, and symbolic%.
    (define (output-constraint-string live-out) ?)
    |#
    
    ))
