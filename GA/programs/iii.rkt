#lang racket

(require "../main.rkt" "../GA-parser.rkt")

(define parser (new GA-parser%))

;; iii                                                                             
(optimize (send parser ast-from-string
                "0 a! !+ push !+ pop dup 1 b! @b 0 b! @b 65535 or over - and + or push drop pop")
          '((data . 2) memory)
          #t #f #:cores 4 #:time-limit 3600 #:size 9                            
          #:assume '((<= . 65535) (<= . 65535) (<= . 65535))
          #:input-file "data-iii2/inputs")
