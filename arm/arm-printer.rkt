#lang racket

(require "../printer.rkt" 
         "../ast.rkt"
         "arm-machine.rkt")

(provide arm-printer%)

(define arm-printer%
  (class printer%
    (super-new)
    (inherit-field machine)
    (override encode-inst decode-inst)

    (define (name->id name)
      ;;(pretty-display `(name->id ,name))
      (cond
       [(string->number name)
        (string->number name)]
       
       [(and (> (string-length name) 1) (equal? (substring name 0 1) "r"))
        (string->number (substring name 1))]
       
       [else 
        (raise (format "encode: name->id: undefined for ~a" name))]))

    ;; Convert an instruction in string format into
    ;; into an instructions encoded using numbers.
    (define (encode-inst x)
      (inst (send machine get-inst-id (string->symbol (inst-op x)))
            (vector-map name->id (inst-args x))))

    ;; Convert an instruction encoded using numbers
    ;; into an instructions in string format.
    (define (decode-inst x)
      (define opcode (send machine get-inst-name (inst-op x)))
      (define args (inst-args x))
      (define class-id (send machine get-class-id opcode))
      
      (define-syntax-rule (make-inst x ...)
        (make-inst-main (list x ...)))
      
      (define (make-inst-main fs)
        (define new-args (for/vector ([f fs] [arg args]) (f arg)))
        (inst (symbol->string opcode) new-args))

      (define (reg x) (format "r~a" x))
      (define imm number->string)

      (cond
       [(equal? class-id 0) (make-inst reg reg reg)]
       [(equal? class-id 1) (make-inst reg reg imm)]
       [(equal? class-id 2) (make-inst reg reg imm)]
       [(equal? class-id 3) (make-inst reg reg)]
       [(equal? class-id 4) (make-inst reg imm)]
       [(equal? class-id 5) (make-inst reg reg reg reg)]
       [(equal? class-id 6) (make-inst reg reg imm imm)]
       [(member opcode '(bfc)) (make-inst reg imm imm)]
       [(equal? opcode `nop) (inst "nop" (vector))]
       [else (raise (format "decode-inst: undefined for ~a" opcode))]))
    ))
