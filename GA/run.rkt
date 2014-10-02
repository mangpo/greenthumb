#lang racket

(require "main.rkt" "GA-parser.rkt")

(define parser (new GA-parser%))
;; fff
;; (optimize (send parser ast-from-string 
;;                 "push over - push and pop pop and over 65535 or and or")
;;           '((data . 2) memory)
;;           #t #t 0 #:cores 8 #:time-limit 3600 #:size 6
;;           #:assume '((<= . 65535) (<= . 65535) (<= . 65535)))

;; iii                                                                             
;; (optimize (send parser ast-from-string
;;                 "0 a! !+ push !+ pop dup 1 b! @b 0 b! @b 65535 or over - and + or p\
;; ush drop pop")
;;           '((data . 2) memory)
;;           #t #t 0 #:cores 8 #:time-limit 36000 ;#:size 9                            
;;           #:assume '((<= . 65535) (<= . 65535) (<= . 65535))
;;           #:input-file "data-iii2/inputs")

;; shaf                                                                            
;; (optimize (send parser ast-from-string
;; 		"0 a! !+ !+ push pop dup 1 b! @b and over - 0 b! @b and or push drop pop")
;;           '((data . 2) (return . 1))
;;           #t #t 0 #:cores 8 #:time-limit 36000 
;;           #:input-file "data-iii2/inputs")

;; shag                                                                            
;; (optimize (send parser ast-from-string
;; 		"0 a! !+ !+ push pop dup 1 b! @b and over 0 b! @b and or 1 b! @b 0 b! @b and or push drop pop")
;;           '((data . 2) (return . 1))
;;           #t #t 0 #:cores 8 #:time-limit 3600
;;           #:input-file "data-iii2/inputs")

;; roundpower                                                                      
(optimize (send parser ast-from-string
                "-1 + push drop pop dup over 2/ over - and + push drop pop")
          '((data . 2))
          #t #t 0 #:cores 8 #:time-limit 800
          #:input-file "data-fff-small/inputs")


;; zero byte
;; (optimize (send parser ast-from-string 
;; 		"32639 and 32639 + over over - and + 32639 over - and + - 65535 and")
;;           '((data . 2) (return . 1) memory)
;;           #t #t 0 #:cores 8 #:time-limit 36000
;; 	  #:input-file "data-fff/inputs")

;; has zero byte?                                                                  
;; (optimize (send parser ast-from-string
;;                 "32639 and 32639 + over over - and + 32639 over - and + -")
;;           '((data . 1))
;;           #t #t 0 #:cores 8 #:time-limit 3600
;;           #:assume '((<= . 65535))
;;           #:input-file "data-fff/inputs")

;; fir
;; (optimize (send parser ast-from-string 
;;                 "push drop pop pop a! right b! !b dup 1 + 15 and push drop pop")
;;           '((data . 1) a memory)
;;           #t #t 0 #:cores 8 #:time-limit 360
;;           #:assume '((<= . 65535))
;;           #:input-file "data-fir/inputs")


;; mem
;; (optimize (send parser ast-from-string "2 b! @b 3 b! !b 1 b! @b 2 b! !b")
;;           '((data . 2) memory)
;;           #t #f 0 #:cores 2 #:time-limit 3600 #:size 8
;;           #:input-file "data-ex/inputs"
;;           #:binary-search #t
;;           );#:start-prog (send parser ast-from-string "3 2/ nop a! @+ @ b! !"))
;; (optimize (send parser ast-from-string "2 b! @b 3 b! !b 1 b! @b 2 b! !b")
;;           '((data . 2) memory)
;;           #t #t 0 #:cores 2 #:time-limit 3600 #:size 8
;;           #:input-file "data-ex/inputs" 
;;           #:start-prog "2 b! @b 3 b! !b 1 !b @b 2 b! b!")

;; (optimize (send parser ast-from-string 
;;                 "over - and + - push drop pop")
;;           '((data . 1))
;;           #t #t 0 
;;           #:cores 2 #:time-limit 3600 #:size 5 #:input-file "data-fff/inputs")

;; (optimize (send parser ast-from-string "dup drop 2 a! !") 
;;           '((data . 2) memory)
;;           #t #t 0 #:cores 2 #:time-limit 3600 #:size 3)
