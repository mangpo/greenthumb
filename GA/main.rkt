#lang racket

(require "../parallel.rkt" "../ast.rkt"
         "GA-parser.rkt" "GA-meta.rkt" "GA-machine.rkt" 
         "GA-printer.rkt" "../compress.rkt" "GA-solver.rkt")

(provide optimize)

(define (optimize code live-out mode stochastic? recv
                  #:assume [assume #f]
                  #:need-filter [need-filter #f]
                  #:dir [dir "output"] 
                  #:cores [cores 12]
                  #:time-limit [time-limit 3600]
                  #:size [size #f])
  
  (define parser (new GA-parser%))
  (define meta (new GA-meta%))
  (define machine (new GA-machine%))
  (define printer (new GA-printer% [machine machine]))
  (define compress (new compress% [machine machine]))
  (define solver (new GA-solver% [machine machine] [printer printer]))
  (define parallel (new parallel% [meta meta] [parser parser] [machine machine] 
                        [printer printer] [compress compress] [solver solver]
			[stochastic? stochastic?]))

  (send parallel optimize code live-out mode 
        #:assume assume
        #:extra-info recv
        #:need-filter need-filter #:dir dir #:cores cores 
        #:time-limit time-limit #:size size)
  )