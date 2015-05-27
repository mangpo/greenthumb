#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt" "arm-simulator-rosette.rkt" 
         "arm-enumerative.rkt" "arm-symbolic.rkt" "arm-stochastic.rkt" "arm-psql.rkt")


(define time (new time% [total-start (current-seconds)]))
(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 4 0 4))
(define printer (new arm-printer% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))
(define enum (new arm-enumerative% [machine machine] [printer printer] [parser parser]))
(define validator (new arm-validator% [machine machine] [printer printer] [simulator simulator-rosette]))


;; Input machine state
(define input-state (progstate (vector 0 0 0)
                               (vector) -1 4))

(define code
(send parser ast-from-string "
	eor	r2, r1, r0
	and	r0, r1, r0
	add	r0, r0, r2, asr #1
"))

(define encoded-code (send printer encode code))

;(send machine analyze-opcode (vector) encoded-code (vector))
(send machine reset-inst-pool)
(send enum reset-generate-inst (list input-state) (list 0 1) #f `rest #f #:no-args #t)
(define iterator (get-field generate-inst enum))

(define (loop count)
  (define p (iterator))
  (if (car p)
      (begin
        (send printer print-syntax (send printer decode (car p)))
        (loop (add1 count))
        )
      count))

(loop 0)

#|
(define random-state (car (send validator generate-input-states 2 encoded-code #f #f)))
(send machine display-state random-state)
(define k 3)
(define type `high)
(define f
  (cond
    [(equal? type `mod) 
     (let ([base (arithmetic-shift 1 k)])
       (lambda (x) (modulo x base)))]
    [(equal? type `high) 
     (let ([mask (arithmetic-shift -1 (- 32 k))])
       (lambda (x) (bitwise-and x mask)))]))
(send enum abstract (send machine progstate->vector random-state) (list 0 1 2) f)|#