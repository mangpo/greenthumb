#lang racket

(require "arm-parser.rkt" "llvm-parser.rkt"
         "main.rkt")

(define live-regs (make-parameter (list)))

(define llvm (make-parameter #f))
(define size (make-parameter #f))
(define cores (make-parameter 12))
(define synthesize (make-parameter #t))
(define stochastic? (make-parameter #t))
(define dir (make-parameter "output"))
(define time-limit (make-parameter 3600))
(define input-file (make-parameter #f))
 
(define file-to-optimize
  (command-line
   #:once-each
   ;; [("-r" "--register") r
   ;;                      "Number of registers (default=5)"
   ;;                      (nregs (string->number r))]
   ;; [("-m" "--memory")   m
   ;;                      "Memory size (default=1)"
   ;;                      (nmems (string->number m))]
   [("--live-reg")      live-r
                        "A list of live registers separated by , with no space (default=none)"
                        (live-regs (map string->number (string-split live-r ",")))]
   [("-c" "--core")      c
                        "Number of cores to run on (default=12)"
                        (cores (string->number c))]
   [("-d" "--dir")      d
                        "Output directory (default=output)"
                        (dir d)]
   [("-l" "--llvm")     "Input is in LLVM IR format."
                        (llvm #t)]
   [("-t" "--time-limit") t
                        "Time limit in seconds (default=36000)."
                        (time-limit t)]
   [("-n" "--size")     n
                        "Time limit in seconds (default=36000)."
                        (size n)]
   [("-i" "--input")    i
                        "Path to inputs."
                        (input-file i)]

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

(define parser (if (llvm) (new llvm-parser%) (new arm-parser%)))
(define code (send parser ast-from-file file-to-optimize))

(optimize code (live-regs) (synthesize) (stochastic?)
          #:need-filter (llvm) #:dir (dir) #:cores (cores) 
          #:time-limit (time-limit) #:size (size)
          #:input-file (input-file))
