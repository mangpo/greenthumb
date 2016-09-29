#lang racket

(require "../main.rkt" "../GA-parser.rkt")

(define parser (new GA-parser%))

;; complexB                                                                             
(optimize (send parser ast-from-string
                "drop 3 and + push drop pop dup 0 b! @b")
          '((data . 2))
          #t #f #:cores 4 #:time-limit 3600 #:size 7
          #:input-file "data-iii2/inputs")
