#lang racket

(require "../printer.rkt" 
         "../inst.rkt"
         "GA-machine.rkt")

(provide GA-printer%)

(define GA-printer%
  (class printer%
    (super-new)
    (inherit-field machine report-mutations)
    (override encode-inst decode-inst print-syntax-inst
              output-constraint-string output-assume-string config-from-string-ir)
    (set! report-mutations (vector-append report-mutations '#(rotate)))

    (define UP (get-field UP machine))
    (define DOWN (get-field DOWN machine))
    (define LEFT (get-field LEFT machine))
    (define RIGHT (get-field RIGHT machine))
    (define IO (get-field IO machine))
    (define encode-port-dict (hash "up" UP "down" DOWN "left" LEFT "right" RIGHT "io" IO))

    (define (encode-inst x)
      (define arg (inst-args x))
      (if (inst-op x)
	  (inst (send machine get-opcode-id (string->symbol (inst-op x)))
		(and arg
		     (if (string->number arg)
			 (vector (string->number arg))
			 (vector (hash-ref encode-port-dict arg)))))
	  x))

    (define (decode-inst x)
      (define args (inst-args x))
      (define arg (and args (> (vector-length args) 0) (vector-ref args 0)))
      (inst (symbol->string (send machine get-opcode-name (inst-op x)))
	    (and (number? arg) (number->string arg))))

    (define (print-syntax-inst x [indent ""])
      (if (equal? (inst-op x) "@p") 
          (display (inst-args x))
          (display (inst-op x)))
      (display " "))

    
    (define (output-assume-string x)
      (if x
          (format "(send machine constrain-stack '~a)" x)
          #f))
    (define (output-constraint-string live-out)
      ;; live-out is something like '((data . 0) (return . 1) memory a)
      (if live-out
          (format "(send machine output-constraint '~a)" live-out)
          #f))
    (define (config-from-string-ir program) #f)

    ))
