#lang s-exp rosette

(require "../ast.rkt"
         "llvm-demo-parser.rkt" "llvm-demo-machine.rkt" "llvm-demo-printer.rkt"
         "llvm-demo-simulator-rosette.rkt" "llvm-demo-simulator-racket.rkt"
         "llvm-demo-validator.rkt"
         "llvm-demo-symbolic.rkt"
         "llvm-demo-stochastic.rkt"
         )

(define parser (new llvm-demo-parser%))
(define machine (new llvm-demo-machine% [config 3]))
(define printer (new llvm-demo-printer% [machine machine]))
(define simulator-racket (new llvm-demo-simulator-racket% [machine machine]))
(define simulator-rosette (new llvm-demo-simulator-rosette% [machine machine]))
(define validator (new llvm-demo-validator% [machine machine] [printer printer]
                       [simulator simulator-rosette]))


(define prefix 
(send parser ast-from-string "
"))

(define postfix
(send parser ast-from-string "
"))

#;(define code
(send parser ast-from-string "
%1 = lshr i32 %in, 3
%out = shl nuw i32 %1, 3
"))

(define code
(send parser ast-from-string "
  %1 = add i32  %in, 0
  %out = shl i32 -8, %1
"))

(define sketch
(send parser ast-from-string "
?
"))

(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))
(define encoded-code (send printer encode code))
(define encoded-sketch (send validator encode-sym sketch))


;; Step 1: use printer to convert liveout into progstate format
(define constraint (send printer encode-live '(%out)))

;; Step 2: create symbolic search
(define symbolic (new llvm-demo-symbolic% [machine machine] [printer printer]
                      [parser parser]
                      [validator validator] [simulator simulator-rosette]))

(send symbolic synthesize-window
      encoded-code ;; spec
      encoded-sketch ;; sketch
      encoded-prefix encoded-postfix
      constraint ;; live-out
      #f ;; extra parameter (not use in llvm)
      #f ;; upperbound cost, #f = no upperbound
      3600 ;; time limit in seconds
      )

;; Step 3: create symbolic search
(define stoch (new llvm-demo-stochastic% [machine machine] [printer printer]
                      [parser parser]
                      [validator validator] [simulator simulator-rosette]
                      [syn-mode #t] ;; #t = synthesize, #f = optimize mode
                      ))
#;(send stoch superoptimize encoded-code 
      constraint ;; constraint
      (send printer encode-live '(%in)) ;; live-in
      "./driver-0" 3600 #f)
