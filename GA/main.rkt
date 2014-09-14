#lang racket

(require "../parallel.rkt" "../ast.rkt"
         "GA-parser.rkt" "GA-meta.rkt" "GA-machine.rkt" 
         "GA-printer.rkt" "../compress.rkt" "GA-solver.rkt")

(provide optimize)

(define (optimize code live-out mode stochastic? recv
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
        #:extra-info recv
        #:need-filter need-filter #:dir dir #:cores cores 
        #:time-limit time-limit #:size size)
  )

(define parser (new GA-parser%))
(optimize (send parser ast-from-string "2 b! @b 3 b! !b 1 b! @b 2 b! !b") 
          '((data . 2) memory)
          #t #f 0 #:cores 8 #:time-limit 300 #:size 8)