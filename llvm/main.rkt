#lang racket

(require "../parallel-driver.rkt" "../inst.rkt"
         "llvm-parser.rkt" "llvm-machine.rkt" 
         "llvm-printer.rkt" 
	 ;; simulator, validator
	 "llvm-simulator-racket.rkt" 
	 "llvm-simulator-rosette.rkt"
         "llvm-validator.rkt")

(provide optimize)

;; Main function to perform superoptimization on multiple cores.
(define (optimize code live-out search-type mode
                  #:dir [dir "output"] 
                  #:cores [cores 4]
                  #:time-limit [time-limit 3600]
                  #:size [size #f]
                  #:window [window #f]
                  #:input-file [input-file #f])
  
  (define parser (new llvm-parser%))
  (define machine (new llvm-machine%))
  (define printer (new llvm-printer% [machine machine]))
  (define simulator (new llvm-simulator-rosette% [machine machine]))
  (define validator (new llvm-validator% [machine machine] [simulator simulator]))
  (define parallel (new parallel-driver% [isa "llvm"] [parser parser] [machine machine] 
                        [printer printer] [validator validator]
                        [search-type search-type] [mode mode]
                        [window window]))

  (send parallel optimize code live-out
        #:dir dir #:cores cores 
        #:time-limit time-limit #:size size #:input-file input-file)
  )
