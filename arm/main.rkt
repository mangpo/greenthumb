#lang racket

(require "../parallel.rkt" "../ast.rkt" "../fitness-learner.rkt"
         "arm-parser.rkt" "arm-meta.rkt" "arm-machine.rkt" 
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
;; need-filter: if #t, filter out some instructions (branches) before superoptimize
;; size (optional)
;; >>> OUTPUT >>>
;; Optimized code, not encoded.
(define (optimize code live-out live-in search-type mode base-cost
                  #:need-filter [need-filter #f]
                  #:dir [dir "output"] 
                  #:cores [cores 12]
                  #:time-limit [time-limit 3600]
                  #:size [size #f]
                  #:window [window #f]
                  #:input-file [input-file #f])
  
  (define parser (new arm-parser%))
  (define meta (new arm-meta%))
  (define machine (new arm-machine%))
  (define printer (new arm-printer% [machine machine]))
  (define compress (new arm-compress% [machine machine]))
  (define validator (new arm-validator% [machine machine] [printer printer]))
  (define parallel (new parallel% [meta meta] [parser parser] [machine machine] 
                        [printer printer] [compress compress] [validator validator]
                        [search-type search-type] [mode mode] [base-cost base-cost]
                        [window window]))

  (send parallel optimize code live-out live-in
        #:need-filter need-filter #:dir dir #:cores cores 
        #:time-limit time-limit #:size size #:input-file input-file)
  )

(define (optimize-enum code-org live-out-org live-in-org size)
  (define machine-precise (new arm-machine%))
  (define compress (new arm-compress% [machine machine-precise]))
  (define-values (code live-out live-in map-back machine-info) 
    (send compress compress-reg-space code-org live-out-org live-in-org))
  (send machine-precise set-config machine-info)
  
  (define machine (new arm-machine% [bit 3]))
  (send machine set-config machine-info)

  (define parser (new arm-parser%))
  (define printer (new arm-printer% [machine machine]))

  (define simulator-racket (new arm-simulator-racket% [machine machine]))
  (define simulator-racket-precise (new arm-simulator-racket% [machine machine-precise]))
  (define validator (new arm-validator% [machine machine]))
  (define validator-precise (new arm-validator% [machine machine-precise]))

  (define enum (new arm-enumerative% [machine machine] [printer printer] [parser parser]))
  (define inverse (new arm-inverse% [machine machine] [simulator simulator-racket]))
  (define backward (new arm-forwardbackward% [machine machine] [enum enum] 
			[simulator simulator-racket]
			[simulator-precise simulator-racket-precise]
			[printer printer] [parser parser] [inverse inverse]
			[validator validator] [validator-precise validator-precise]))

     (send backward synthesize-window
         (send printer encode code)
         size
         (vector) (vector)
         (send machine-precise output-constraint live-out) #f #f 3600)
   )
  
(define (arm-generate-inputs code machine-config dir)
  (define machine (new arm-machine%))
  (send machine set-config machine-config)
  (define printer (new arm-printer% [machine machine]))
  (define validator (new arm-validator% [machine machine] [printer printer]))
  (generate-inputs (send printer encode code) #f dir machine printer validator))
