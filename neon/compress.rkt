#lang racket

(require "../ast.rkt" "machine.rkt")
(provide 
 compress-reg-space decompress-reg-space pre-constraint-rename
 select-code combine-code combine-live-out)

;; Default: no compression
(define (compress-reg-space program live-out)
  (values program
          live-out
          #f
          (list nregs-d nregs-r nmems)))

(define (decompress-reg-space program reg-map)
  program)

(define (pre-constraint-rename live-output reg-map)
  live-output)

;; Select an interesting portion of code to superoptimize.
;; Exclude COPY, branching instructions.
;; Outputs
;; 1. selected-code to be optimized, #f if no code to be optimized
;; 2. starting position
;; 3. stopping position
;; 4. additional live-out in the same format as one used by optimize.rkt--- a list of live registers
(define (select-code code)
  (values code 0 (vector-length code) (list)))

(define (combine-code original select start stop)
  (vector-append (vector-take original start)
                 select
                 (vector-drop original (add1 stop))))


;; Combine 2 live-out info into one.
;; Inputs are in the same format as live-out used by optimize.rkt and returned by (select-code)--- a list of live registers.
(define (combine-live-out org extra) 
  (list
   (remove-duplicates (append (first org) (first extra)))
   (remove-duplicates (append (second org) (second extra)))))
  