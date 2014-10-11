#lang racket

(require "../main.rkt" "../GA-parser.rkt")

(define parser (new GA-parser%))

;; shaf
(optimize (send parser ast-from-string
		"0 a! !+ !+ push pop dup 1 b! @b and over - 0 b! @b and or push drop pop")
          '((data . 2) (return . 1))
          #t #f 0 #:cores 4 #:time-limit 3600 
          #:input-file "data-iii2/inputs")
