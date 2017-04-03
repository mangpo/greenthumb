#lang racket

(require "../main.rkt" "../GA-parser.rkt")

(define parser (new GA-parser%))

;; complexA                                                                             
(optimize (send parser ast-from-string
                "drop pop a 3 b! !b 0 b! !b 3 b! @b up b! !b 0 b! @b 2* 2* up b! @b 3 and +")
          '((data . 2))
          #t #f #:cores 4 #:time-limit 3600
          #:input-file "data-rrotate/inputs")
