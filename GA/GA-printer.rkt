#lang racket

(require "../printer.rkt" 
         "../ast.rkt"
         "GA-machine.rkt")

(provide GA-printer%)

(define GA-printer%
  (class printer%
    (super-new)
    (inherit-field machine report-mutations)
    (override encode-inst decode-inst print-syntax-inst)
    (set! report-mutations (vector-append report-mutations '#(rotate)))

    (define UP (get-field UP machine))
    (define DOWN (get-field DOWN machine))
    (define LEFT (get-field LEFT machine))
    (define RIGHT (get-field RIGHT machine))
    (define IO (get-field IO machine))
    (define encode-port-dict (hash "up" UP "down" DOWN "left" LEFT "right" RIGHT "io" IO))

    (define (encode-inst x)
      (define arg (inst-args x))
      (inst (send machine get-inst-id (string->symbol (inst-op x)))
	    (and arg
		 (if (string->number arg)
		     (string->number arg)
		     (hash-ref encode-port-dict arg)))))

    (define (decode-inst x)
      (define arg (inst-args x))
      (inst (symbol->string (send machine get-inst-name (inst-op x)))
	    (and (number? arg) (number->string arg))))

    (define (print-syntax-inst x [indent ""])
      (if (equal? (inst-op x) "@p") 
          (display (inst-args x))
          (display (inst-op x)))
      (display " "))

    ))
