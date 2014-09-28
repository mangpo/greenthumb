#lang racket

(require "main.rkt" "GA-parser.rkt")

(define parser (new GA-parser%))
;; fff
;; (optimize (send parser ast-from-string 
;;                 "push over - push and pop pop and over 65535 or and or")
;;           '((data . 2) memory)
;;           #t #t 0 #:cores 8 #:time-limit 3600 #:size 6
;;           #:assume '((<= . 65535) (<= . 65535) (<= . 65535)))

;; mem
(optimize (send parser ast-from-string "2 b! @b 3 b! !b 1 b! @b 2 b! !b")
          '((data . 2) memory)
          #t #t 0 #:cores 4 #:time-limit 60 #:size 8
          #:input-file "data-ex/inputs"
          );#:start-prog (send parser ast-from-string "3 2/ nop a! @+ @ b! !"))
;; (optimize (send parser ast-from-string "2 b! @b 3 b! !b 1 b! @b 2 b! !b")
;;           '((data . 2) memory)
;;           #t #t 0 #:cores 2 #:time-limit 3600 #:size 8
;;           #:input-file "data-ex/inputs" 
;;           #:start-prog "2 b! @b 3 b! !b 1 !b @b 2 b! b!")

;; has zero byte?
;; (optimize (send parser ast-from-string 
;;                 "dup 32639 and 32639 + over over - and + 32639 over - and + -")
;;           '((data . 1))
;;           #t #t 0 #:cores 8 #:time-limit 3600 #:size 6
;;           #:assume '((<= . 65535)))

;; (optimize (send parser ast-from-string 
;;                 "over - and + - push drop pop")
;;           '((data . 1))
;;           #t #t 0 
;;           #:cores 2 #:time-limit 3600 #:size 5 #:input-file "data-fff/inputs")

;; (optimize (send parser ast-from-string "dup drop 2 a! !") 
;;           '((data . 2) memory)
;;           #t #t 0 #:cores 2 #:time-limit 3600 #:size 3)
