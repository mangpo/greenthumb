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
    (define (print-syntax-inst x [indent ""])
      ? ;; modify this function

      ;; Example
      (pretty-display (format "~a ~a"
                              (inst-op x)
                              (string-join (vector->list (inst-args x)) ", "))))

    ;; Convert an instruction x from string-IR to encoded-IR format.
    (define (encode-inst x)
      ? ;; modify this function
      
      ;; Example
      (define opcode-name (inst-op x))
      (define args (inst-args x))
      (define last-arg (vector-ref args (sub1 (vector-length args))))

      ;; If last arg is not a register, append "#" to opcode-name.
      ;; This is for distinguishing instructions that take (reg reg reg) vs (reg reg const).
      ;; They need distinct opcode names.
      (unless (equal? (substring last-arg 0 1) "r")
            (set! opcode-name (string-append opcode-name "#")))

      ;; A function to convert argument in string format to number.
      (define (convert-arg arg)
        (if (equal? (substring arg 0 1) "r")
            (string->number (substring arg 1))
            (string->number arg)))
      
      (inst (send machine get-opcode-id opcode-name)
            (vector-map convert-arg args)))
            

    ;; Convert an instruction x from encoded-IR to string-IR format.
    (define (decode-inst x)
      ? ;; modify this function
      
      ;; Example
      (define opcode-id (inst-op x))
      (define opcode-name (send machine get-opcode-name opcode-id))
      (define str-len (string-length opcode-name))
      (define arg-types (send machine get-arg-types opcode-id))
      (define args (inst-args x))

      ;; Remove #
      (when (equal? "#" (substring opcode-name (sub1 str-len)))
            (set! opcode-name (substring opcode-name 0 (sub1 str-len))))

      (define new-args
        (for/vector ([arg args] [type arg-types])
                    (cond
                     [(equal? type 'reg) (format "r~a" arg)]
                     [else (number->string arg)])))

      (inst opcode-name new-args))

    ;;;;;;;;;;;;;;;;;;;;;;;;; For cooperative search ;;;;;;;;;;;;;;;;;;;;;;;
    #|
    ;; Convert live-out (which is one of the outputs from parser::info-from-file) into string. 
    ;; The string will be used as a piece of code the search driver generates as
    ;; the live-out argument to the method superoptimize of 
    ;; stochastics%, forwardbackward%, and symbolic%.
    (define (output-constraint-string live-out) ?)
    |#
    
    ))
