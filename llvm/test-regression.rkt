#lang s-exp rosette

(require "llvm-validator.rkt" "llvm-machine.rkt" "llvm-printer.rkt"
         "llvm-parser.rkt" "../inst.rkt"
         "llvm-simulator-racket.rkt" "llvm-simulator-rosette.rkt"
         "llvm-symbolic.rkt" "llvm-stochastic.rkt" "llvm-forwardbackward.rkt"
         "llvm-inverse.rkt" "llvm-enumerator.rkt")


(define (test id code-str size liveout-str #:sym [sym #f] #:enum [enum #t] #:assume [assume #f])
  (pretty-display (format "TEST = ~a" id))

  (define parser (new llvm-parser%))
  (define machine (new llvm-machine% [config (cons 3 3)]))

  (define printer (new llvm-printer% [machine machine]))
  (define simulator-racket (new llvm-simulator-racket% [machine machine]))
  (define simulator-rosette (new llvm-simulator-rosette% [machine machine]))
  (define validator (new llvm-validator% [machine machine] [simulator simulator-rosette]))

  (define symbolic (new llvm-symbolic% [machine machine]
                        [printer printer] [parser parser]
                        [validator validator] [simulator simulator-rosette]))
  (define backward (new llvm-forwardbackward% [machine machine] 
                        [printer printer] [parser parser] 
                        [validator validator] [simulator simulator-racket]
                        [inverse% llvm-inverse%]
                        [enumerator% llvm-enumerator%]
                        [syn-mode `linear]))
  
  (define code (send parser ir-from-string code-str))
  (define encoded-code (send printer encode code))
  (send printer print-struct encoded-code)
  (send validator adjust-memory-config encoded-code)
  
  (define sketch (for/vector ([i size]) (send symbolic gen-sym-inst)))
  (define constraint (send printer encode-live liveout-str))
  
  ;; symbolic
  (when sym
    (define-values (out-sym cost-sym)
      (send symbolic synthesize-from-sketch encoded-code sketch constraint
            #:assume (and assume (send machine constrain-stack assume))))
    
    (unless out-sym (raise (format "TEST ~a: fail to synthesize [symbolic]" id)))
    (define ce-sym (send validator counterexample encoded-code out-sym constraint
                         #:assume (and assume (send machine constrain-stack assume))))
    (when ce-sym (raise (format "TEST ~a: counter-example [symbolic]" id))))

  (when enum
    (define out-enum
      (send backward synthesize-window
            encoded-code
            sketch
            (vector) (vector)
            constraint ;; live-out
            #f ;; upperbound cost, #f = no upperbound
            3600 ;; time limit in seconds
            #:assume (and assume (send machine constrain-stack assume))
            ))
    
    (unless out-enum (raise (format "TEST ~a: fail to synthesize [enumerative]" id)))
    (define ce-enum (send validator counterexample encoded-code out-enum constraint
                          #:assume (and assume (send machine constrain-stack assume))))
    (when ce-enum (raise (format "TEST ~a: counter-example [enumerative]" id))))
  )

(test 'clear3bits "
%1 = lshr i32 %in, 3
%out = shl nuw i32 %1, 3
" 1 '#((%out) () #f) #:sym #t)

(test 'loadstore "
%1 = load i32, i32* %2
%1 = add i32 %1, 0
%1 = add i32 %1, 3
store i32 %1, i32* %2
" 3 '#(() () #t) #:sym #f)

(test 'p24 "
%9 = add i32 %in, -1
%out = ashr i32 %9, 1
%out = or i32 %out, %9
%9 = ashr i32 %out, 2
%9 = or i32 %9, %out
%out = ashr i32 %9, 4
%out = or i32 %out, %9
%9 = ashr i32 %out, 8
%9 = or i32 %9, %out
%out = ashr i32 %9, 16
%out = or i32 %out, %9
%out = add i32 %out, 1
" 4 '#((%out) () #f))

(test 'vec1 "
%out = add <4 x i32> %1, <i32 0, i32 0, i32 0, i32 0>
%out = add <4 x i32> %1, <i32 0, i32 1, i32 2, i32 3>
%out = add <4 x i32> %out, <i32 -1, i32 -1, i32 -1, i32 -1>
%out = add <4 x i32> %out, %2
" 3 '#(() (%out) #f))

(test 'vec2 "
%out = add <4 x i32> %1, %1
%out = add <4 x i32> %out, %1
%out = add <4 x i32> %out, %1
%out = add <4 x i32> %out, %1
%out = add <4 x i32> %out, %1
%out = add <4 x i32> %out, %1
%out = add <4 x i32> %out, %1
" 3 '#(() (%out) #f))
