#lang racket

(require "../printer.rkt" "../inst.rkt" "$-machine.rkt")

(provide $-printer%)

(define $-printer%
  (class printer%
    (super-new)
    (inherit-field machine)
    (override encode-inst decode-inst print-syntax-inst)

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

      (cond
       [opcode-name
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

        (inst (send machine get-opcode-id (string->symbol opcode-name))
              ;; get-opcode-id takes symbol (not string) as argument
              (vector-map convert-arg args))]

       ;; opcode-name is #f, x is an unknown instruction (a place holder for synthesis)
       ;; just return x in this case
       [else x]))

            

    ;; Convert an instruction x from encoded-IR to string-IR format.
    (define (decode-inst x)
      ? ;; modify this function
      
      ;; Example
      (define opcode-id (inst-op x))
      ;; get-opcode-name returns symbol, so we need to convert it to string
      (define opcode-name (symbol->string (send machine get-opcode-name opcode-id)))
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
    ;; Convert live-out (the output from parser::info-from-file) into string. 
    ;; The string will be used as a piece of code the search driver generates as
    ;; the live-out argument to the method superoptimize of 
    ;; stochastics%, forwardbackward%, and symbolic%.
    ;; The string should be evaluated to a program state that contains 
    ;; #t and #f, where #t indicates that the corresponding element is live.
    (define/override (output-constraint-string live-out)
      ? ;; modify this funcion.
      
      ;; Example:
      ;; Method encode-live is implemented below, returning
      ;; live infomation in a program state format.
      (format "(send printer encode-live '~a)" live-out))

    ;; Convert liveness infomation to the same format as program state.
    (define/public (encode-live x)
      ? ;; modify this funcion.
      
      ;; Example:
      ;; If x is a list, iterate over elements in x, and set those elements to be live.
      (define reg-live (make-vector (send machine get-config) #f))
      (define mem-live #f)
      (for ([v x])
           (cond
            [(number? v) (vector-set! reg-live v #t)]
            [(equal? v 'memory) (set! mem-live #t)]))
      (progstate reg-live mem-live))
    
    ;; Return program state config from a given program in string-IR format.
    ;; program: string IR format
    ;; output: program state config
    (define/override (config-from-string-ir program)
      ? ;; modify this funcion.
      
      ;; Example:
      ;; config = number of registers
      ;; Find the highest register ID and return that as a config
      (define max-reg 0)
      (for* ([x program]
	     [arg (inst-args x)])
            (when (equal? "r" (substring arg 0 1))
                  (let ([id (string->number (substring arg 1))])
                    (when (> id max-reg) (set! max-reg id)))))
      (add1 max-reg))
    |#
    
    ))
