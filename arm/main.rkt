#lang racket

(require "../parallel.rkt" "../ast.rkt" "../fitness-learner.rkt"
         "arm-parser.rkt" "arm-meta.rkt" "arm-machine.rkt" 
         "arm-printer.rkt" "arm-compress.rkt" "arm-solver.rkt")

(provide optimize optimize-solver arm-generate-inputs)

;; Main function to perform stochastic superoptimization on multiple cores.
;; >>> INPUT >>>
;; code: program to superoptimized, parsed but not encoded.
;; live-out: a list of live register ids
;; synthesize: #t if synthesize mode, #f if optimize mode
;; need-filter: if #t, filter out some instructions (branches) before superoptimize
;; size (optional)
;; >>> OUTPUT >>>
;; Optimized code, not encoded.
(define (optimize code live-out live-in search-type mode
                  #:need-filter [need-filter #f]
                  #:dir [dir "output"] 
                  #:cores [cores 12]
                  #:time-limit [time-limit 3600]
                  #:size [size #f]
                  #:input-file [input-file #f])
  
  (define parser (new arm-parser%))
  (define meta (new arm-meta%))
  (define machine (new arm-machine%))
  (define printer (new arm-printer% [machine machine]))
  (define compress (new arm-compress% [machine machine]))
  (define solver (new arm-solver% [machine machine] [printer printer]))
  (define parallel (new parallel% [meta meta] [parser parser] [machine machine] 
                        [printer printer] [compress compress] [solver solver]
                        [search-type search-type] [mode mode]))

  (send parallel optimize code live-out live-in
        #:need-filter need-filter #:dir dir #:cores cores 
        #:time-limit time-limit #:size size #:input-file input-file)
  )

;;
;; Main function to perform solver-based superoptimization on one core.
;; It will return the output program that produces the same values of 
;; live regsiters and memory as the input program.
;; Notice that this function will output the optimal output (in term of length or cost).
;; Therefore, user can find the optimal output in term of length by
;; keep calling this function with decreasing 'size' argument until
;; it can no longer find an output.
;; 
;; >>> INPUTS >>>
;; code: program to superoptimized, parsed but not encoded.
;; live-out: a list of live register ids
;; size (optional): number of output instructions. Default to the length of the input code.
;;
;; >>> OUTPUTS >>>
;; Equivalent code to the input, not encoded
(define (optimize-solver code live-out machine-config
                         #:time-limit [time-limit 3600] 
                         #:size [size (vector-length code)])
  (define machine (new arm-machine%))
  (send machine set-config machine-config)
  (define printer (new arm-printer% [machine machine]))
  (define solver (new arm-solver% [machine machine] [printer printer] [syn-mode `binary]))
  
  (define encoded-code (send printer encode code))
  
  (define x
    (send solver superoptimize encoded-code 
          (send machine output-constraint live-out) "name" time-limit size))
  (send printer print-syntax x)
  x)
  
(define (arm-generate-inputs code machine-config dir)
  (define machine (new arm-machine%))
  (send machine set-config machine-config)
  (define printer (new arm-printer% [machine machine]))
  (define solver (new arm-solver% [machine machine] [printer printer]))
  (generate-inputs (send printer encode code) #f dir machine printer solver))
