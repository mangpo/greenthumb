#lang racket

(require "GA-parser.rkt"
         "main.rkt")

(define size (make-parameter #f))
(define cores (make-parameter 8))
(define search-type (make-parameter `hybrid))
(define mode (make-parameter `syn))
(define dir (make-parameter "output"))
(define time-limit (make-parameter 3600))
(define window (make-parameter #f))
 
(define file-to-optimize
  (command-line
   #:once-each
   [("-c" "--core")      c
                        "Number of search instances (default=8)"
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
   [("--sym") "Use symbolic search."
                        (search-type `solver)]
   [("--stoch") "Use stochastic search."
                        (search-type `stoch)]
   [("--enum") "Use enumerative search."
                        (search-type `enum)]
   [("--hybrid") "Use stochastic search."
                        (search-type `hybrid)]

   #:once-any
   [("-l" "--linear")   "Linear search."
                        (mode `linear)]
   [("-b" "--binary")   "Binary search."
                        (mode `binary)]
   [("-p" "--partial")   "Partial search."
                        (mode `partial)]

   #:once-any
   [("-o" "--optimize") "Optimize mode starts searching from the original program"
                        (mode `opt)]
   [("-s" "--synthesize") "Synthesize mode starts searching from random programs (default)"
                        (mode `syn)]

   #:args (filename) ; expect one command-line argument: <filename>
   ; return the argument as a filename to compile
   filename))

(define parser (new GA-parser%))
(define code (send parser ir-from-file file-to-optimize))
(define-values (live-out assume input-file) (send parser info-from-file (string-append file-to-optimize ".info")))

(pretty-display `(live-out ,live-out ,assume ,input-file))
(optimize code live-out (search-type) (mode) 
	  #:assume assume
          #:dir (dir) #:cores (cores) 
          #:time-limit (time-limit) #:size (size) #:window (window)
          #:input-file input-file
	  )
