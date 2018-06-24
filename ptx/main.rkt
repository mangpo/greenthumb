#lang racket

(require "../parallel-driver.rkt" "../inst.rkt"
         "ptx-parser.rkt" "ptx-machine.rkt" 
         "ptx-printer.rkt"
	 ;; simulator, validator
	 "ptx-simulator-racket.rkt" 
	 "ptx-simulator-rosette.rkt"
         "ptx-validator.rkt")

(provide optimize)

;; Main function to perform superoptimization on multiple cores.
;; >>> INPUT >>>
;; code: program to superoptimized in string-IR format
;; >>> OUTPUT >>>
;; Optimized code in string-IR format.
(define (optimize code live-out search-type mode
                  #:assume [assume #f]
                  #:dir [dir "output"] 
                  #:cores [cores 4]
                  #:time-limit [time-limit 3600]
                  #:size [size #f]
                  #:window [window #f]
                  #:input-file [input-file #f])
  
  (define parser (new ptx-parser%))
  (define machine (new ptx-machine%))
  (define printer (new ptx-printer% [machine machine]))
  (define simulator (new ptx-simulator-rosette% [machine machine]))
  (define validator (new ptx-validator% [machine machine] [simulator simulator]))
  (define parallel (new parallel-driver% [isa "ptx"] [parser parser] [machine machine] 
                        [printer printer] [validator validator]
                        [search-type search-type] [mode mode]
                        [window window]))

  (send parallel optimize code live-out
        #:assume assume
        #:dir dir #:cores cores 
        #:time-limit time-limit #:size size #:input-file input-file)
  )

