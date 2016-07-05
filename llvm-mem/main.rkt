#lang racket

(require "../parallel-driver.rkt" "../inst.rkt"
         "llvm-mem-parser.rkt" "llvm-mem-machine.rkt" 
         "llvm-mem-printer.rkt" 
	 ;; simulator, validator
	 "llvm-mem-simulator-racket.rkt" 
	 "llvm-mem-simulator-rosette.rkt"
         "llvm-mem-validator.rkt")

(provide optimize)

;; Main function to perform superoptimization on multiple cores.
(define (optimize code live-out live-in search-type mode
                  #:dir [dir "output"] 
                  #:cores [cores 4]
                  #:time-limit [time-limit 3600]
                  #:size [size #f]
                  #:window [window #f]
                  #:input-file [input-file #f])
  
  (define parser (new llvm-mem-parser%))
  (define machine (new llvm-mem-machine%))
  (define printer (new llvm-mem-printer% [machine machine]))
  (define simulator (new llvm-mem-simulator-rosette% [machine machine]))
  (define validator (new llvm-mem-validator% [machine machine] [simulator simulator]))
  (define parallel (new parallel-driver% [isa "llvm-mem"] [parser parser] [machine machine] 
                        [printer printer] [validator validator]
                        [search-type search-type] [mode mode]
                        [window window]))

  (send parallel optimize code live-out live-in
        #:dir dir #:cores cores 
        #:time-limit time-limit #:size size #:input-file input-file)
  )
