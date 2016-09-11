#lang s-exp rosette

(require "../inst.rkt"
         "llvm-mem-parser.rkt" "llvm-mem-machine.rkt" "llvm-mem-printer.rkt"
         "llvm-mem-simulator-rosette.rkt" "llvm-mem-simulator-racket.rkt"
         "llvm-mem-validator.rkt"
         "llvm-mem-symbolic.rkt" "llvm-mem-stochastic.rkt"
         "llvm-mem-forwardbackward.rkt"
         "llvm-mem-enumerator.rkt" "llvm-mem-inverse.rkt"
         )

(define parser (new llvm-mem-parser% [compress? #f]))
(define machine (new llvm-mem-machine% [config 4]))
(define printer (new llvm-mem-printer% [machine machine]))
(define simulator-racket (new llvm-mem-simulator-racket% [machine machine]))
(define simulator-rosette (new llvm-mem-simulator-rosette% [machine machine]))
(define validator (new llvm-mem-validator% [machine machine] [simulator simulator-rosette]))


(define prefix 
(send parser ir-from-string "
"))

(define postfix
(send parser ir-from-string "
"))

;; TODO: error
#;(define code
(send parser ir-from-string "
%in = load i32, i32* %1
%out = load i32, i32* %2
%in = add i32 %in, 0
%out = add i32 %in, %out
"))

(define code
(send parser ir-from-string "
%1 = load i32, i32* %2
%1 = add i32 %1, 0
%1 = add i32 %1, 1
store i32 %1, i32* %2
"))

(define sketch
(send parser ir-from-string "
? ? ?
"))


(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))
(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))

(send machine analyze-args encoded-prefix encoded-code encoded-postfix #f #f)

;; Step 1: use printer to convert liveout into progstate format
(define constraint (send printer encode-live (vector '() #t)))

;; Step 2: create symbolic search
(define symbolic (new llvm-mem-symbolic% [machine machine] [printer printer]
                      [parser parser]
                      [validator validator] [simulator simulator-rosette]))

#;(send symbolic synthesize-window
      encoded-code ;; spec
      encoded-sketch ;; sketch
      encoded-prefix encoded-postfix
      constraint ;; live-out
      #f ;; extra parameter (not use in llvm)
      #f ;; upperbound cost, #f = no upperbound
      3600 ;; time limit in seconds
      )

;; Step 3: create stochastic search
(define stoch (new llvm-mem-stochastic% [machine machine] [printer printer]
                      [parser parser]
                      [validator validator] [simulator simulator-rosette]
                      [syn-mode #t] ;; #t = synthesize, #f = optimize mode
                      ))
#;(send stoch superoptimize encoded-code 
      constraint ;; constraint
      (send printer encode-live (vector '(%in %1) #t)) ;; live-in
      "./driver-0" 3600 #f)

;; Step 4: create enumerative search
(define backward (new llvm-mem-forwardbackward% [machine machine] 
                      [printer printer] [parser parser] 
                      [validator validator] [simulator simulator-racket]
                      [inverse% llvm-mem-inverse%]
                      [enumerator% llvm-mem-enumerator%]
                      [syn-mode `linear]))
(send backward synthesize-window
      encoded-code ;; spec
      encoded-sketch ;; sketch => start from searching from length 1, number => only search for that length
      encoded-prefix encoded-postfix
      constraint ;; live-out
      #f ;; extra parameter (not use in llvm)
      #f ;; upperbound cost, #f = no upperbound
      3600 ;; time limit in seconds
      )
