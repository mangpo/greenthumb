#lang s-exp rosette

(require "../ast.rkt" "arm-ast.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" 
         "arm-simulator-rosette.rkt" "arm-simulator-racket.rkt" 
         "arm-enumerator.rkt" "arm-inverse.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine% [bit 4] [config (list 3 0 4)]))
(define printer (new arm-printer% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))
(define simulator (new arm-simulator-racket% [machine machine]))
(define inverse (new arm-inverse% [machine machine] [simulator simulator]))

(define enum (new arm-enumerator%
                  [machine machine]
                  [printer printer]))
;(send machine analyze-opcode (vector) encoded-code (vector))
(send machine reset-inst-pool)

;; Input machine state
(define input-state (progstate (vector 0 0 0)
                               (vector) -1 4))

(define code
(send parser ast-from-string "
	eor	r2, r1, r0
	and	r0, r1, r0
	add	r0, r0, r2, asr #1
"))

(define iterator
  (send enum generate-inst
        (cons (list 0 1 2) (list))
        (cons (list 0) (list)) #f #f))

(define encoded-code (send printer encode code))
(define behavior (make-hash))

(define (loop count)
  (define p (iterator))
  (define my-inst (car p))
  (if my-inst
      (begin
        ;(send printer print-syntax (send printer decode my-inst))
        (loop (add1 count))
        )
      count))

(define mem0 (quotient (current-memory-use) 1000000))
(define t0 (current-seconds))
(pretty-display `(count ,(loop 0)))
(define t1 (current-seconds))
(define mem1 (quotient (current-memory-use) 1000000))
(pretty-display (format "TIME(s): ~a" (- t1 t0)))
(pretty-display (format "MEM(MB): ~a ~a" mem0 mem1))
