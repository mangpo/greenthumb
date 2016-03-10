#lang s-exp rosette

(require "../inst.rkt"
         "llvm-demo-parser.rkt" "llvm-demo-machine.rkt" "llvm-demo-printer.rkt"
         "llvm-demo-simulator-rosette.rkt" "llvm-demo-simulator-racket.rkt"
         "llvm-demo-validator.rkt"
         "llvm-demo-symbolic.rkt" "llvm-demo-stochastic.rkt"
         "llvm-demo-forwardbackward.rkt"
         "llvm-demo-enumerator.rkt" "llvm-demo-inverse.rkt"
         )

(define parser (new llvm-demo-parser% [compress? #t]))
(define machine (new llvm-demo-machine% [config 3]))
(define printer (new llvm-demo-printer% [machine machine]))
(define simulator-racket (new llvm-demo-simulator-racket% [machine machine]))
(define simulator-rosette (new llvm-demo-simulator-rosette% [machine machine]))
(define validator (new llvm-demo-validator% [machine machine] [simulator simulator-rosette]))


(define prefix 
(send parser ir-from-string "
"))

(define postfix
(send parser ir-from-string "
"))

;; clearing 3 lowest bits
#;(define code
(send parser ir-from-string "
%1 = lshr i32 %in, 3
%out = shl nuw i32 %1, 3
"))
;; %out = and i32 %in, -8

#;(define code
(send parser ir-from-string "
  %1 = add nsw i32 %in, -1
  %2 = ashr i32 %1, 1
  %3 = or i32 %2, %1
  %out = add i32 %3, 0
"))
;; solver: > 30 sec

;; rounding to power of 2
;; geneated from clang -O3
(define code
(send parser ir-from-string "
  %1 = add nsw i32 %in, -1
  %2 = ashr i32 %1, 1
  %3 = or i32 %2, %1
  %4 = ashr i32 %3, 2
  %5 = or i32 %4, %3
  %6 = ashr i32 %5, 4
  %7 = or i32 %6, %5
  %8 = ashr i32 %7, 8
  %9 = or i32 %8, %7
  %10 = ashr i32 %9, 16
  %11 = or i32 %10, %9
  %out = add nsw i32 %11, 1
")) ;; 12 instructions

(define sketch
(send parser ir-from-string "
? ? ? ?
"))


(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))
(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))

;; Step 1: use printer to convert liveout into progstate format
(define constraint (send printer encode-live '(%out)))

;; Step 2: create symbolic search
(define symbolic (new llvm-demo-symbolic% [machine machine] [printer printer]
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
(define stoch (new llvm-demo-stochastic% [machine machine] [printer printer]
                      [parser parser]
                      [validator validator] [simulator simulator-rosette]
                      [syn-mode #t] ;; #t = synthesize, #f = optimize mode
                      ))
#;(send stoch superoptimize encoded-code 
      constraint ;; constraint
      (send printer encode-live '(%in)) ;; live-in
      "./driver-0" 3600 #f)

;; Step 4: create enumerative search
(define backward (new llvm-demo-forwardbackward% [machine machine] 
                      [printer printer] [parser parser] 
                      [validator validator] [simulator simulator-racket]
                      [inverse% llvm-demo-inverse%]
                      [enumerator% llvm-demo-enumerator%]
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
