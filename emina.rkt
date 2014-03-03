#lang racket

(require "controller.rkt" "state.rkt" "ast.rkt" "f18a.rkt")

;; for loop with symbolic bound
(superoptimize (encode
                (forloop 
                 (list 
                  (block "3" "3" #f))
                 (list 
                  (block
                   "@b drop"
                   "@b drop" #f))
                 16))
               (encode
                (forloop 
                 (list 
                  (block "_" "3" #f))
                 (list 
                  (block
                   "_ _"
                   "@b drop" #f))
                 16))
               (cons 5 0)
               (constraint r t))

;; much slower when using (interpret-spec)
;; search for ATTN(emina) in controller.rkt
;; (superoptimize (encode "0 a! !+ push !+ pop dup 1 b! @b 0 b! @b 65535 or over - and + or push drop pop")
;;                (encode "_ _ _ _ _ _ _ _ _")
;;                (cons 2 0)
;;                (constraint s t)
;;                #:assume (constrain-stack '((<= . 65535) (<= . 65535) (<= . 65535))))
