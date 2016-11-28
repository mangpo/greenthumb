#lang racket

(require "../parallel-driver.rkt" 
         "GA-parser.rkt" "GA-machine.rkt" 
         "GA-printer.rkt" 
         "GA-simulator-rosette.rkt"
         "GA-validator.rkt")

(provide optimize)

(define parser (new GA-parser%))
(define machine (new GA-machine% [config 1]))
(define printer (new GA-printer% [machine machine]))
(define simulator (new GA-simulator-rosette% [machine machine]))
(define validator (new GA-validator% [machine machine] [simulator simulator]))

(define (optimize code live-out search-type mode 
                  #:assume [assume #f]
                  #:dir [dir "output"] 
                  #:cores [cores 4]
                  #:time-limit [time-limit 3600]
                  #:size [size #f]
                  #:window [window #f]
                  #:input-file [input-file #f]
                  #:start-prog [start-prog #f])

  (define parallel (new parallel-driver% [isa "GA"] [parser parser] [machine machine] 
                        [printer printer] [validator validator]
                        [search-type search-type] [mode mode]
                        [window window]))
  (send parallel optimize code live-out
        #:assume assume
        #:dir dir #:cores cores 
        #:time-limit time-limit #:size size 
        #:input-file input-file #:start-prog start-prog)
  )

;; (define (GA-generate-inputs code dir #:assume [assume #f])
;;   (generate-inputs (send printer encode code) dir 
;;                    machine printer validator #:assume (constrain-stack machine assume)))
