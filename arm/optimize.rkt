#lang racket

(require "arm-parser.rkt"
         "main.rkt")

;; (define live-out (make-parameter (list)))
;; (define live-in (make-parameter #f))
;; (define live-mem (make-parameter #t))

(define size (make-parameter #f))
(define cores (make-parameter 12))
(define search-type (make-parameter `hybrid))
(define mode (make-parameter `syn))

(define dir (make-parameter "output"))
(define time-limit (make-parameter 3600))
(define input-file (make-parameter #f))
(define window (make-parameter #f))
(define base-cost (make-parameter #f))
 
(define file-to-optimize
  (command-line
   #:once-each
   ;; [("-r" "--register") r
   ;;                      "Number of registers (default=5)"
   ;;                      (nregs (string->number r))]
   ;; [("-m" "--memory")   m
   ;;                      "Memory size (default=1)"
   ;;                      (nmems (string->number m))]
   ;; [("--live-out")      live-o
   ;;                      "A list of live-out registers separated by , with no space (default=none)"
   ;;                      (live-out (map string->number (string-split live-o ",")))]
   ;; [("--live-in")       live-i
   ;;                      "A list of live-in registers separated by , with no space (default=none)"
   ;;                      (live-in (map string->number (string-split live-i ",")))]
   ;; [("--dead-mem")      "Memory is not live-out."
   ;;                      (live-mem #f)]
   [("-c" "--core")      c
                        "Number of cores to run on (default=12)"
                        (cores (string->number c))]
   [("-d" "--dir")      d
                        "Output directory (default=output)"
                        (dir d)]
   [("-t" "--time-limit") t
                        "Time limit in seconds (default=36000)."
                        (time-limit t)]
   [("-n" "--size")     n
                        "Time limit in seconds (default=36000)."
                        (size n)]
   [("-i" "--input")    i
                        "Path to inputs."
                        (input-file i)]
   [("-w" "--window")    w
                        "Path to inputs."
                        (window (string->number w))]
   #:once-any
   [("--solver") "Use solver-based search."
                        (search-type `solver)]
   [("--stoch") "Use stochastic search."
                        (search-type `stoch)]
   [("--hybrid") "Use stochastic search."
                        (search-type `hybrid)]

   #:once-any
   [("-l" "--linear")   "Linear search."
                        (mode `linear)]
   [("-b" "--binary")   "Binary search."
                        (mode `binary)]

   #:once-any
   [("-o" "--optimize") "Optimize mode starts searching from the original program"
                        (mode `opt)]
   [("-s" "--synthesize") "Synthesize mode starts searching from random programs (default)"
                        (mode `syn)]

   #:once-any
   [("--inter")  "Cost function with intermediates."
                 (base-cost #f)]
   [("--base")   "Baseline cost function."
                 (base-cost #t)]


   #:args (filename) ; expect one command-line argument: <filename>
   ; return the argument as a filename to compile
   filename))

(define parser (new arm-parser%))
(define code (send parser ast-from-file file-to-optimize))
(define-values (live-mem live-out live-in) (send parser info-from-file (string-append file-to-optimize ".info")))

(optimize code (list live-out live-mem) (list live-in live-mem) (search-type) (mode) (base-cost)
          #:need-filter #f #:dir (dir) #:cores (cores) 
          #:time-limit (time-limit) #:size (size) #:window (window)
          #:input-file (input-file)
	  )
