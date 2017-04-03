#lang racket

(require "../parallel-driver.rkt" "../inst.rkt"
         "$-parser.rkt" "$-machine.rkt" 
         "$-printer.rkt"
	 ;; simulator, validator
	 "$-simulator-racket.rkt" 
	 "$-simulator-rosette.rkt"
         "$-validator.rkt")

(provide optimize)

;; Main function to perform superoptimization on multiple cores.
;; >>> INPUT >>>
;; code: program to superoptimized in string-IR format
;; >>> OUTPUT >>>
;; Optimized code in string-IR format.
(define (optimize code live-out search-type mode
                  #:dir [dir "output"] 
                  #:cores [cores 4]
                  #:time-limit [time-limit 3600]
                  #:size [size #f]
                  #:window [window #f]
                  #:input-file [input-file #f])
  
  (define parser (new $-parser%))
  (define machine (new $-machine%))
  (define printer (new $-printer% [machine machine]))
  (define simulator (new $-simulator-rosette% [machine machine]))
  (define validator (new $-validator% [machine machine] [simulator simulator]))
  (define parallel (new parallel-driver% [isa "$"] [parser parser] [machine machine] 
                        [printer printer] [validator validator]
                        [search-type search-type] [mode mode]
                        [window window]))

  (send parallel optimize code live-out 
        #:dir dir #:cores cores 
        #:time-limit time-limit #:size size #:input-file input-file)
  )
