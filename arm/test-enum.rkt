#lang s-exp rosette

(require "../ast.rkt" "arm-ast.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-simulator-rosette.rkt" 
         "arm-enumerative.rkt" "arm-abstract.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 4 0 4))
(define printer (new arm-printer% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))
(define enum (new arm-enumerative% [machine machine] [printer printer] [parser parser]))
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

(send enum reset-generate-inst (list input-state) (list 0) #f 
      `mod-high #f #:no-args #t)
(define abst (new arm-abstract% [k 3]))
(send abst set-type! `high)

(define encoded-code (send printer encode code))
(define iterator (get-field generate-inst enum))

(define (loop count)
  (define p (iterator))
  (define my-inst (car p))
  (if my-inst
      (begin
        (with-output-to-file "progress.log" #:exists 'append
          (thunk (send printer print-syntax (send printer decode my-inst))))
        (send printer print-syntax (send printer decode my-inst))
	(send abst gen-abstract-behavior my-inst)
        (loop (add1 count))
        )
      count))

(system "rm progress.log")
;(system "rm abstract.csv")
(define mem0 (quotient (current-memory-use) 1000000))
(define t0 (current-seconds))
(pretty-display `(count ,(loop 0)))
(define t1 (current-seconds))
(define mem1 (quotient (current-memory-use) 1000000))
(pretty-display (format "TIME(s): ~a" (- t1 t0)))
(pretty-display (format "MEM(MB): ~a ~a" mem0 mem1))
(pretty-display (format "yes: ~a, no: ~a" 
                        (get-field all-yes abst)
                        (get-field all-no abst)))

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
