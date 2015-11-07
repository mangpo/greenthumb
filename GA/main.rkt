#lang racket

(require "../parallel.rkt" "../fitness-learner.rkt"
         "GA-parser.rkt" "GA-machine.rkt" 
         "GA-printer.rkt" "../compress.rkt" 
         "GA-validator.rkt")

(provide optimize)

(define parser (new GA-parser%))
(define machine (new GA-machine% [config 1]))
(define printer (new GA-printer% [machine machine]))
(define compress (new compress% [machine machine]))
(define validator (new GA-validator% [machine machine] [printer printer]))

(define (optimize code live-out search-type mode recv
                  #:assume [assume #f]
                  #:dir [dir "output"] 
                  #:cores [cores 12]
                  #:time-limit [time-limit 3600]
                  #:size [size #f]
                  #:window [window #f]
                  #:input-file [input-file #f]
                  #:start-prog [start-prog #f])

  (define parallel (new parallel% [isa "GA"] [parser parser] [machine machine] 
                        [printer printer] [compress compress] [validator validator]
                        [search-type search-type] [mode mode]
                        [window window]))
  (send parallel optimize code live-out #f
        #:assume assume
        #:extra-info recv
        #:dir dir #:cores cores 
        #:time-limit time-limit #:size size 
        #:input-file input-file #:start-prog start-prog)
  )

(define (GA-generate-inputs code extra-info dir #:assume [assume #f])
  (generate-inputs (send printer encode code) extra-info dir 
                   machine printer validator #:assume (constrain-stack machine assume)))
