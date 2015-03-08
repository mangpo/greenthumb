#lang racket

(require "../parallel.rkt" 
         "neon-parser.rkt" "neon-meta.rkt" "neon-machine.rkt" 
         "neon-printer.rkt" "neon-compress.rkt" "neon-validator.rkt")

(provide optimize)

(define (optimize code live-out synthesize 
                  #:need-filter [need-filter #f]
                  #:dir [dir "output"] 
                  #:cores [cores 12]
                  #:time-limit [time-limit 3600]
                  #:size [size #f])
  
  (define parser (new neon-parser%))
  (define meta (new neon-meta%))
  (define machine (new neon-machine%))
  (define printer (new neon-printer% [machine machine]))
  (define compress (new neon-compress% [machine machine]))
  (define validator (new neon-validator% [machine machine] [printer printer]))
  (define parallel (new parallel% [meta meta] [parser parser] [machine machine] 
                        [printer printer] [compress compress] [validator validator]))

  (send parallel optimize code live-out synthesize 
        #:need-filter need-filter #:dir dir #:cores cores 
        #:time-limit time-limit #:size size)
  )