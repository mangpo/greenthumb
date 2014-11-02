#lang racket

(require "../parallel.rkt" "../fitness-learner.rkt" "../ast.rkt"
         "GA-parser.rkt" "GA-meta.rkt" "GA-machine.rkt" 
         "GA-printer.rkt" "../compress.rkt" 
         "GA-solver.rkt" "GA-stochastic.rkt"
         "GA-simulator-racket.rkt")

(provide optimize)

(define parser (new GA-parser%))
(define meta (new GA-meta%))
(define machine (new GA-machine%))
(send machine set-config 4)
(define printer (new GA-printer% [machine machine]))
(define compress (new compress% [machine machine]))
(define solver (new GA-solver% [machine machine] [printer printer]))
(define backward-stochastic (new GA-stochastic% [machine machine] [printer printer] [syn-mode #t] [forward #f]))

(define simulator (new GA-simulator-racket% [machine machine]))

(define (optimize code live-out search-type mode recv
                  #:assume [assume #f]
                  #:need-filter [need-filter #f]
                  #:dir [dir "output"] 
                  #:cores [cores 12]
                  #:time-limit [time-limit 3600]
                  #:size [size #f]
                  #:input-file [input-file #f]
                  #:start-prog [start-prog #f])

  (define parallel (new parallel% [meta meta] [parser parser] [machine machine] 
                        [printer printer] [compress compress] [solver solver]
                        [search-type search-type] [mode mode]))
  (send parallel optimize code live-out #f
        #:assume assume
        #:extra-info recv
        #:need-filter need-filter #:dir dir #:cores cores 
        #:time-limit time-limit #:size size 
        #:input-file input-file #:start-prog start-prog)
  )

(define (GA-generate-inputs code extra-info dir #:assume [assume #f])
  (generate-inputs (send printer encode code) extra-info dir 
                   machine printer solver #:assume (constrain-stack machine assume)))

(define (GA-generate-outputs-steps code dir subdir)
  (generate-outputs-steps (send printer encode code) dir subdir
                          machine printer simulator backward-stochastic))

(define (GA-calculate-cost dir name live-out)
  (calculate-cost dir name live-out machine backward-stochastic))

(define (GA-calculate-cost2 dir name live-out n)
  (calculate-cost2 dir name live-out n machine backward-stochastic))

(define (GA-generate-tree code dir subdir degree n)
  (generate-tree (send printer encode code) dir subdir degree n
                 machine printer simulator backward-stochastic))


;; (GA-generate-inputs 
;;  (send (new GA-parser%) ast-from-string 
;;        "2 b! !b 2 b! @b b! @b 2 b! @b 1 + b! @b 2 b! @b b! @b - 1 + +")
;;  0
;;  "data-interp")

;; (GA-generate-outputs-steps 
;;  (send (new GA-parser%) ast-from-string 
;;        "2 b! @b 3 b! !b 1 b! @b 2 b! !b")
;;  "data-ex" "n100-s3")


;(GA-calculate-cost "data-ex/n100-s3" "v1" '((data . 2) memory))

#|
(GA-generate-tree 
 (send (new GA-parser%) ast-from-string 
       "2 b! @b 3 b! !b 1 b! @b 2 b! !b")
 "data-ex" "tree-n5000" 4 5000)|#
  
;(GA-calculate-cost2 "data-ex/tree-n5000" "v3" '((data . 2) memory) 5005)
