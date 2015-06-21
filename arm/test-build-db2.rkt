#lang racket

(require "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt"
         "arm-simulator-racket.rkt" "arm-simulator-rosette.rkt" 
         "arm-validator.rkt"
         "arm-enumerative.rkt" "arm-database.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine% [bit 4]))
(send machine set-config (list 4 0 1))
(send machine reset-inst-pool)

(define printer (new arm-printer% [machine machine]))
(define simulator-racket (new arm-simulator-racket% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))
(define validator (new arm-validator% [machine machine] [printer printer] [simulator simulator-rosette]))

(define enum (new arm-enumerative% [machine machine] [printer printer] [parser parser]))

(define db (new arm-database% [machine machine] [enum enum]
                [simulator simulator-racket] [simulator-precise simulator-racket]
                [printer printer] [validator validator]
                [validator-precise validator] [parser parser]))

(define mem0 (quotient (current-memory-use) 1000000))
(define t0 (current-seconds))
(send db gen-behavior-base)
;;(send db save-to-db)
;;(send db expand-behaviors)
(define t1 (current-seconds))
(define mem1 (quotient (current-memory-use) 1000000))
(pretty-display (format "TIME(s): ~a" (- t1 t0)))
(pretty-display (format "MEM(MB): ~a ~a" mem0 mem1))
