#lang racket

(require "../main.rkt" "../GA-parser.rkt")

(define parser (new GA-parser%))

;; complexC                                                                            
(optimize (send parser ast-from-string
                "drop 65536 over - 1 + + push drop pop")
          '((data . 2))
          #t #t 0 #:cores 8 #:time-limit 3600
          #:input-file "data-iii2/inputs")
