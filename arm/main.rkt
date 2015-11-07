#lang racket

(require "../parallel.rkt" "../ast.rkt" "../fitness-learner.rkt"
         "arm-parser.rkt" "arm-machine.rkt" 
         "arm-printer.rkt" "arm-compress.rkt" "arm-validator.rkt"
	 ;; for enumerative search
	 "arm-simulator-racket.rkt"
	 "arm-enumerative.rkt" "arm-inverse.rkt" "arm-forwardbackward.rkt")

(provide optimize arm-generate-inputs)

;; Main function to perform stochastic superoptimization on multiple cores.
;; >>> INPUT >>>
;; code: program to superoptimized, parsed but not encoded.
;; live-out: a list of live register ids
;; synthesize: #t if synthesize mode, #f if optimize mode
;; size (optional)
;; >>> OUTPUT >>>
;; Optimized code, not encoded.
(define (optimize code live-out live-in search-type mode
                  #:dir [dir "output"] 
                  #:cores [cores 12]
                  #:time-limit [time-limit 3600]
                  #:size [size #f]
                  #:window [window #f]
                  #:input-file [input-file #f])
  
  (define parser (new arm-parser%))
  (define machine (new arm-machine%))
  (define printer (new arm-printer% [machine machine]))
  (define compress (new arm-compress% [machine machine]))
  (define validator (new arm-validator% [machine machine] [printer printer]))
  (define parallel (new parallel% [isa "arm"] [parser parser] [machine machine] 
                        [printer printer] [compress compress] [validator validator]
                        [search-type search-type] [mode mode]
                        [window window]))

  (send parallel optimize code live-out live-in
        #:dir dir #:cores cores 
        #:time-limit time-limit #:size size #:input-file input-file)
  )
  
(define (arm-generate-inputs code machine-config dir)
  (define machine (new arm-machine%))
  (send machine set-config machine-config)
  (define printer (new arm-printer% [machine machine]))
  (define validator (new arm-validator% [machine machine] [printer printer]))
  (generate-inputs (send printer encode code) #f dir machine printer validator))
