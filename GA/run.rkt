#lang racket

(require "main.rkt" "GA-parser.rkt")

(define parser (new GA-parser%))
;; (optimize (send parser ast-from-string 
;;                 "push over - push and pop pop and over 65535 or and or")
;;           '((data . 2) memory)
;;           #t #t 0 #:cores 8 #:time-limit 3600 #:size 6
;;           #:assume '((<= . 65535) (<= . 65535) (<= . 65535)))

(optimize (send parser ast-from-string "2 b! @b 3 b! !b 1 b! @b 2 b! !b")
          '((data . 2) memory)
          #t #t 0 #:cores 8 #:time-limit 3600 #:size 8)

;; (optimize (send parser ast-from-string "dup drop 2 a! !") 
;;           '((data . 2) memory)
;;           #t #t 0 #:cores 2 #:time-limit 3600 #:size 3)