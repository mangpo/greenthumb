#lang racket

(require "main.rkt" "GA-parser.rkt")

(define parser (new GA-parser%))
;; iii
(optimize (send parser ast-from-string 
		"a! push a - over 65535 or and or pop or")
          '((data . 2) memory)
          #t #t 0 #:cores 8 #:time-limit 2000 #:size 9
          #:assume '((<= . 65535) (<= . 65535) (<= . 65535))
	  #:input-file "data-iii3/inputs")
