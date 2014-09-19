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
(define stochastic (new GA-stochastic% [machine machine] [printer printer] [syn-mode #t]))

(define simulator (new GA-simulator-racket% [machine machine]))

(define (optimize code live-out mode stochastic? recv
                  #:assume [assume #f]
                  #:need-filter [need-filter #f]
                  #:dir [dir "output"] 
                  #:cores [cores 12]
                  #:time-limit [time-limit 3600]
                  #:size [size #f]
                  #:input-file [input-file #f])

  (define parallel (new parallel% [meta meta] [parser parser] [machine machine] 
                        [printer printer] [compress compress] [solver solver]
                        [stochastic? stochastic?]
                        [input-file input-file]))
  (send parallel optimize code live-out mode 
        #:assume assume
        #:extra-info recv
        #:need-filter need-filter #:dir dir #:cores cores 
        #:time-limit time-limit #:size size)
  )

(define (GA-generate-inputs code extra-info dir)
  (generate-inputs (send printer encode code) extra-info dir 
                   machine printer solver))

(define (GA-generate-outputs-steps code dir subdir)
  (generate-outputs-steps (send printer encode code) dir subdir
                          machine simulator stochastic))

(define (GA-calculate-cost dir subdir live-out)
  (calculate-cost dir subdir live-out machine stochastic))

#|
(GA-generate-inputs 
 (send (new GA-parser%) ast-from-string 
       "2 b! @b 3 b! !b 1 b! @b 2 b! !b")
 0
 "data-ex")|#

#|
(GA-generate-outputs-steps 
 (send (new GA-parser%) ast-from-string 
       "2 b! @b 3 b! !b 1 b! @b 2 b! !b")
 "data-ex" "spec-n10-3")|#

;(GA-calculate-cost "data-ex" "spec-n10-3" '((data . 2) memory))


  
  