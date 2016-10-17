#lang racket

(require "../main.rkt" "../GA-parser.rkt")

(define parser (new GA-parser%))

;; complexC                                                                            
(optimize (send parser ast-from-string
                "drop 65536 over - 1 + + push drop pop")
          '((data . 2))
          `hybrid `syn 
	  #:cores 4 #:time-limit 3600
          #:input-file "data-iii2/inputs")
