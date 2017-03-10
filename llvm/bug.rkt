#lang rosette

(require "llvm-parser.rkt" "llvm-machine.rkt" "llvm-printer.rkt"
         "llvm-simulator-rosette.rkt"
         "llvm-validator.rkt"
         "llvm-symbolic.rkt"
         )

(define parser (new llvm-parser%))
(define machine (new llvm-machine% [config (cons 3 3)]))
(define printer (new llvm-printer% [machine machine]))
(define simulator-rosette (new llvm-simulator-rosette% [machine machine]))
(define validator (new llvm-validator% [machine machine]
                       [simulator simulator-rosette] [printer printer]))


(define prefix 
(send parser ir-from-string "
"))

(define postfix
(send parser ir-from-string "
"))


(define code
(send parser ir-from-string "
%1 = lshr i32 %in, 3
%out = shl nuw i32 %1, 3
"))
;%out = and i32 %in, -8

;; Define search space of candidate programs.
;; # of ?'s = # of instructions in a candidate program.
;; ? represents one instruction.
(define sketch
(send parser ir-from-string "
?
"))

(define answer
(send parser ir-from-string "
%out = and i32 %in, -8
"))

(define encoded-answer (send printer encode answer))


(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))
(send validator adjust-memory-config encoded-code)

(send machine reset-arg-ranges)
(send machine analyze-args (vector) encoded-code (vector) #f #f)

;; Step 1: use printer to convert liveout into progstate format
(define constraint (send printer encode-live (vector '(%out) '() #t)))

;; Step 2: create symbolic search
(define symbolic (new llvm-symbolic% [machine machine] [printer printer]
                      [parser parser]
                      [validator validator] [simulator simulator-rosette]))

;; First verify X to find counterexample
(define ex 
  (send validator counterexample encoded-code encoded-answer 
        (send printer encode-live (vector '(%out) '() #f))))

(pretty-display "Counterexample:")
(if ex 
  (send machine display-state ex)
  (pretty-display "No"))
(newline)

;; Synthesize Y
(define-values (out-sym cost-sym)
(send symbolic synthesize-window
      encoded-code ;; spec
      encoded-sketch ;; sketch
      (vector) (vector)
      constraint ;; live-out
      #f ;; upperbound cost, #f = no upperbound
      120 ;; time limit in seconds
      ))

;; Second verify X to find counterexample
(define ex2
  (send validator counterexample encoded-code encoded-answer 
        (send printer encode-live (vector '(%out) '() #f))))

(pretty-display "Counterexample:")
(if ex2 
  (send machine display-state ex2)
  (pretty-display "No"))
(newline)
