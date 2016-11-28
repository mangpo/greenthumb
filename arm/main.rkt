#lang racket

(require "../parallel-driver.rkt" "../inst.rkt"
         "arm-parser.rkt" "arm-machine.rkt" 
         "arm-printer.rkt"
	 ;; simulator, validator
	 "arm-simulator-racket.rkt" 
	 "arm-simulator-rosette.rkt"
         "arm-validator.rkt")

(provide optimize)

;; Main function to perform superoptimization on multiple cores.
;; >>> INPUT >>>
;; code: program to superoptimized in string-IR format
;; >>> OUTPUT >>>
;; Optimized code in string-IR format
(define (optimize code live-out search-type mode
                  #:dir [dir "output"] 
                  #:cores [cores 4]
                  #:time-limit [time-limit 3600]
                  #:size [size #f]
                  #:window [window #f]
                  #:input-file [input-file #f])
  (define parser (new arm-parser%))
  (define machine (new arm-machine%))
  (define printer (new arm-printer% [machine machine]))
  (define simulator (new arm-simulator-rosette% [machine machine]))
  (define validator (new arm-validator% [machine machine] [simulator simulator]))
  (define parallel (new parallel-driver% [isa "arm"] [parser parser] [machine machine] 
                        [printer printer] [validator validator]
                        [search-type search-type] [mode mode]
                        [window window]))

  (send parallel optimize code live-out 
        #:dir dir #:cores cores 
        #:time-limit time-limit #:size size #:input-file input-file)
  )
  
;; (define (arm-generate-inputs code machine-config dir)
;;   (define machine (new arm-machine% [config machine-config]))
;;   (define printer (new arm-printer% [machine machine]))
;;   (define simulator (new arm-simulator-rosette% [machine machine]))
;;   (define validator (new arm-validator% [machine machine] [simulator simulator]))
;;   (generate-inputs (send printer encode code) #f dir machine printer validator))
