#lang s-exp rosette

(require "../inst.rkt"
         "llvm-parser.rkt" "llvm-machine.rkt" "llvm-printer.rkt"
         "llvm-simulator-rosette.rkt" "llvm-simulator-racket.rkt"
         "llvm-validator.rkt"
         "llvm-symbolic.rkt" "llvm-stochastic.rkt"
         "llvm-forwardbackward.rkt"
         "llvm-enumerator.rkt" "llvm-inverse.rkt"
         )

(define parser (new llvm-parser%))
(define machine (new llvm-machine% [config (cons 3 0)]))
(define printer (new llvm-printer% [machine machine]))
(define simulator-racket (new llvm-simulator-racket% [machine machine]))
(define simulator-rosette (new llvm-simulator-rosette% [machine machine]))
(define validator (new llvm-validator% [machine machine]
                       [simulator simulator-rosette] [printer printer]))


(define prefix 
(send parser ir-from-string "
"))

(define postfix
(send parser ir-from-string "
"))


#;(define code
(send parser ir-from-string "
%1 = lshr i32 %in, 3
%out = shl nuw i32 %1, 3
"))
;%out = and i32 %in, -8

#;(define code
(send parser ir-from-string "
%in = add i32 %in, 0
store i32 %in, i32* %1
"))

#;(define code
(send parser ir-from-string "
%out = load i32, i32* %1
%out = add i32 %out, 0
"))

#;(define code
(send parser ir-from-string "
%in = load i32, i32* %1
%out = load i32, i32* %2
%in = add i32 %in, 0
%out = add i32 %in, %out
"))

#;(define code
(send parser ir-from-string "
%1 = load i32, i32* %2
%1 = add i32 %1, 0
%1 = add i32 %1, 1
store i32 %1, i32* %2
"))

#;(define code
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
"))

(define code
(send parser ir-from-string "
%in = sub i32 %in, %1
%in = sub i32 %in, %1
%out = sub i32 %in, %1
"))

#;(define code
(send parser ir-from-string "
%2 = add i32 %in, %in
%2 = add i32 %2, %2
%2 = sub i32 %2, %in
%2 = sub i32 %2, %in
%2 = sub i32 %2, %in
%out = sub i32 %2, %in"))

#;(define code
(send parser ir-from-string "
%out = add <4 x i32> %1, <i32 0, i32 1, i32 2, i32 3>
%out = add <4 x i32> %out, <i32 -1, i32 -1, i32 -1, i32 -1>
%out = add <4 x i32> %out, %2
"))

#;(define code
(send parser ir-from-string "
%out = add <4 x i32> %1, %1
%out = add <4 x i32> %out, %1
%out = add <4 x i32> %out, %1
%out = add <4 x i32> %out, %1
%out = add <4 x i32> %out, %1
%out = add <4 x i32> %out, %1
%out = add <4 x i32> %out, %1
"))

;; Define search space of candidate programs.
;; # of ?'s = # of instructions in a candidate program.
;; ? represents one instruction.
(define sketch
(send parser ir-from-string "
? ?
"))


(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))
(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))
(send validator adjust-memory-config encoded-code)

(send machine reset-arg-ranges)
(send machine analyze-args encoded-prefix encoded-code encoded-postfix #f #f)

;; Step 1: use printer to convert liveout into progstate format
(define constraint (send printer encode-live (vector '(%out) '() #t)))

;; Step 2: create symbolic search
(define symbolic (new llvm-symbolic% [machine machine] [printer printer]
                      [parser parser]
                      [validator validator] [simulator simulator-rosette]))

#;(send symbolic synthesize-window
      encoded-code ;; spec
      encoded-sketch ;; sketch
      encoded-prefix encoded-postfix
      constraint ;; live-out
      #f ;; upperbound cost, #f = no upperbound
      60 ;; time limit in seconds
      )

;; Step 3: create stochastic search
(define stoch (new llvm-stochastic% [machine machine] [printer printer]
                      [parser parser]
                      [validator validator] [simulator simulator-rosette]
                      [syn-mode #t] ;; #t = synthesize, #f = optimize mode
                      ))
(send stoch superoptimize encoded-code 
      constraint ;; constraint
      "./driver-0" 3600 #f)

;; Step 4: create enumerative search
(define backward (new llvm-forwardbackward% [machine machine] 
                      [printer printer] [parser parser] 
                      [validator validator] [simulator simulator-racket]
                      [inverse% llvm-inverse%]
                      [enumerator% llvm-enumerator%]
                      [syn-mode `linear]))
#;(send backward synthesize-window
      encoded-code ;; spec
      encoded-sketch ;; sketch => start from searching from length 1, number => only search for that length
      encoded-prefix encoded-postfix
      constraint ;; live-out
      #f ;; upperbound cost, #f = no upperbound
      3600 ;; time limit in seconds
      )
