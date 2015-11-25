#lang racket

(require "../parallel-driver.rkt" "../ast.rkt"
         "llvm-demo-parser.rkt" "llvm-demo-machine.rkt" 
         "llvm-demo-printer.rkt" "llvm-demo-compress.rkt"
	 ;; simulator, validator
	 "llvm-demo-simulator-racket.rkt" 
	 "llvm-demo-simulator-rosette.rkt"
         "llvm-demo-validator.rkt")

(provide optimize)

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
                  #:cores [cores 4]
                  #:time-limit [time-limit 3600]
                  #:size [size #f]
                  #:window [window #f]
                  #:input-file [input-file #f])
  
  (define parser (new llvm-demo-parser%))
  (define machine (new llvm-demo-machine%))
  (define printer (new llvm-demo-printer% [machine machine]))
  (define compress (new llvm-demo-compress% [machine machine]))
  (define simulator (new llvm-demo-simulator-rosette% [machine machine]))
  (define validator (new llvm-demo-validator% [machine machine] [simulator simulator]))
  (define parallel (new parallel-driver% [isa "llvm-demo"] [parser parser] [machine machine] 
                        [printer printer] [compress compress] [validator validator]
                        [search-type search-type] [mode mode]
                        [window window]))

  (send parallel optimize code live-out live-in
        #:dir dir #:cores cores 
        #:time-limit time-limit #:size size #:input-file input-file)
  )
