#lang racket

(require "arm-parser.rkt"
         "main.rkt")

(define live-out (make-parameter (list)))
(define live-in (make-parameter #f))
(define live-mem (make-parameter #t))

(define size (make-parameter #f))
(define cores (make-parameter 12))
(define synthesize (make-parameter #t))
(define stochastic? (make-parameter #t))
(define dir (make-parameter "output"))
(define time-limit (make-parameter 3600))
(define input-file (make-parameter #f))
(define binary (make-parameter #f))
 
(define file-to-optimize
  (command-line
   #:once-each
   ;; [("-r" "--register") r
   ;;                      "Number of registers (default=5)"
   ;;                      (nregs (string->number r))]
   ;; [("-m" "--memory")   m
   ;;                      "Memory size (default=1)"
   ;;                      (nmems (string->number m))]
   [("--live-out")      live-o
                        "A list of live-out registers separated by , with no space (default=none)"
                        (live-out (map string->number (string-split live-o ",")))]
   [("--live-in")       live-i
                        "A list of live-in registers separated by , with no space (default=none)"
                        (live-in (map string->number (string-split live-i ",")))]
   [("--dead-mem")      "Memory is not live-out."
                        (live-mem #f)]
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

   [("-b" "--binary")   "Binary search."
                        (binary #t)]

   #:once-any
   [("-o" "--optimize") "Optimize mode starts searching from the original program"
                        (synthesize #f)]
   [("-s" "--synthesize") "Synthesize mode starts searching from random programs (default)"
                        (synthesize #t)]

   #:once-any
   [("--solver") "Use solver-based search."
                        (stochastic? #f)]
   [("--stoch") "Use stochastic search."
                        (stochastic? #t)]

   #:args (filename) ; expect one command-line argument: <filename>
   ; return the argument as a filename to compile
   filename))

(define parser (new arm-parser%))
(define code (send parser ast-from-file file-to-optimize))

(optimize code (list (live-out) (live-mem)) (list (live-in) (live-mem)) 
          (synthesize) (stochastic?)
          #:need-filter #f #:dir (dir) #:cores (cores) 
          #:time-limit (time-limit) #:size (size)
          #:input-file (input-file)
	  #:binary-search (binary)
	  )
