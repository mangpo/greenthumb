#lang racket

(require "../main.rkt" "../GA-parser.rkt")

(define parser (new GA-parser%))

;; interp                                                                  
(optimize (send parser ast-from-string
                "2 b! !b 2 b! @b b! @b 2 b! @b 1 + b! @b 2 b! @b b! @b - 1 + +")
          '((data . 2))
          #t #t #:cores 8 #:time-limit 3600
          #:input-file "data-interp/inputs")
