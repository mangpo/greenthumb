#lang racket

(require "../main.rkt" "../GA-parser.rkt")

(define parser (new GA-parser%))

;; rrotate                                                                             
(optimize (send parser ast-from-string
                "2 b! !b push drop pop 2 b! @b 0 b! !b up b! @b 0 b! @b 2/ 2/ + 65535 and")
          '((data . 2) (return . 1))
          `stoch `syn #:cores 8 #:time-limit 3600
          #:input-file "data-rrotate/inputs")
