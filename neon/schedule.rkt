#lang racket

(require "../inst.rkt" "neon-inst.rkt" "neon-machine.rkt")
(provide (all-defined-out))

;; unit: load, store, bit-permute (lsbp) or alu
;; issue: numbers of issue cycles
;; latency: numbers of cycles take starting from when inputs are ready until the output is ready
;; from: fast-forward path ID
;; to: fast-forward path ID
(struct schd-info (unit issue latency from to))

(define schedule
  (list
   (cons (neon-inst `nop #f #f #f) (schd-info `lsbp 0 0 #f #f))
   (cons (neon-inst `vld1 #f #f #f) (schd-info `lsbp 2 1 #f #f))
   (cons (neon-inst `vld1! #f #f #f) (schd-info `lsbp 2 1 #f #f))
   (cons (neon-inst `vld2 #f #f #f) (schd-info `lsbp 2 1 #f #f))
   (cons (neon-inst `vld2! #f #f #f) (schd-info `lsbp 2 1 #f #f))
   (cons (neon-inst `vst1 #f #f #f) (schd-info `lsbp 2 1 #f #f))
   (cons (neon-inst `vst1! #f #f #f) (schd-info `lsbp 2 1 #f #f))
   (cons (neon-inst `vst2 #f #f #f) (schd-info `lsbp 2 1 #f #f))
   (cons (neon-inst `vst2! #f #f #f) (schd-info `lsbp 2 1 #f #f))

   (cons (neon-inst `vext# `d #f #f) (schd-info `lsbp 1 2 #f #f))
   (cons (neon-inst `vext# `q #f #f) (schd-info `lsbp 2 2 #f #f))

   (cons (neon-inst `vtrn `d #f #f) (schd-info `lsbp 1 2 #f #f))
   (cons (neon-inst `vtrn `q #f #f) (schd-info `lsbp 2 2 #f #f))
   (cons (neon-inst `vzip `d #f #f) (schd-info `lsbp 1 2 #f #f))
   (cons (neon-inst `vzip `q #f #f) (schd-info `lsbp 3 2 #f #f))
   (cons (neon-inst `vuzp `d #f #f) (schd-info `lsbp 1 2 #f #f))
   (cons (neon-inst `vuzp `q #f #f) (schd-info `lsbp 3 2 #f #f))
   (cons (neon-inst `vswp `d #f #f) (schd-info `lsbp 1 2 #f #f))
   (cons (neon-inst `vswp `q #f #f) (schd-info `lsbp 2 2 #f #f))

   (cons (neon-inst `vmla `d 8 #f) (schd-info `alu 1 5 1 1))
   (cons (neon-inst `vmla `d 16 #f) (schd-info `alu 1 5 1 1))
   (cons (neon-inst `vmla `d 32 #f) (schd-info `alu 2 5 1 1))
   (cons (neon-inst `vmla `q 8 #f) (schd-info `alu 2 5 1 1))
   (cons (neon-inst `vmla `q 16 #f) (schd-info `alu 2 5 1 1))
   (cons (neon-inst `vmla `q 32 #f) (schd-info `alu 4 5 1 1))

   (cons (neon-inst `vmlal #f 8 #f) (schd-info `alu 1 5 1 1))
   (cons (neon-inst `vmlal #f 16 #f) (schd-info `alu 1 5 1 1))
   (cons (neon-inst `vmlal #f 32 #f) (schd-info `alu 2 5 1 1))

   (cons (neon-inst `vmla@ `d 16 #f) (schd-info `alu 1 5 1 1))
   (cons (neon-inst `vmla@ `d 32 #f) (schd-info `alu 2 5 1 1))
   (cons (neon-inst `vmla@ `q 16 #f) (schd-info `alu 1 5 1 1))
   (cons (neon-inst `vmla@ `q 32 #f) (schd-info `alu 2 5 1 1))

   (cons (neon-inst `vmlal@ #f 16 #f) (schd-info `alu 1 5 1 1))
   (cons (neon-inst `vmlal@ #f 32 #f) (schd-info `alu 2 5 1 1))

   (cons (neon-inst `vmov #f #f #f) (schd-info `lsbp 1 2 #f #f))
   ;;(cons (neon-inst `vmov@ #f #f #f) (schd-info `lsbp 1 2 #f #f)) ;; vmov Dd, Dm[x]
   (cons (neon-inst `vmov# #f #f #f) (schd-info `alu 1 1 #f #f)) ;; vmov Dd, #imm

   (cons (neon-inst `vand #f #f #f) (schd-info `alu 1 2 #f #f))
   (cons (neon-inst `vand# #f #f #f) (schd-info `alu 1 2 #f #f))
   (cons (neon-inst `vorr #f #f #f) (schd-info `alu 1 2 #f #f))
   (cons (neon-inst `vorr# #f #f #f) (schd-info `alu 1 2 #f #f))
   (cons (neon-inst `vbsl `d #f #f) (schd-info `alu 1 2 #f #f))
   (cons (neon-inst `vbsl `q #f #f) (schd-info `alu 2 2 #f #f))
   
   (cons (neon-inst `vadd #f #f #f) (schd-info `alu 1 2 #f #f))
   (cons (neon-inst `vsub #f #f #f) (schd-info `alu 1 3 #f #f))
   (cons (neon-inst `vhadd #f #f #f) (schd-info `alu 1 3 #f #f))
   (cons (neon-inst `vhsub #f #f #f) (schd-info `alu 1 4 #f #f))

   (cons (neon-inst `vshr# #f #f #f) (schd-info `alu 1 3 #f #f))
   ))     
 
;; Construct schedule-info table (vector) from schdule in schedule.rkt file
(define (init-schedule-info machine)
  (define ninsts (get-field ninsts machine))
  (define nregs-d (send machine get-nregs-d))
  (define schedule-info (make-vector ninsts (list)))

  (define (is-d? args) 
    (let ([x (vector-ref args 0)])
      (and (>= x 0) (< x nregs-d))))
  
  (define (is-q? args) 
    (let ([x (vector-ref args 0)])
      (and (>= x nregs-d) (< x (+ nregs-d (quotient nregs-d 2))))))

  (define (update-schedule-info schd)
    (define key (car schd))
    (define val (cdr schd))

    ;; Represent neon-inst using numbers so that solver can reason about it
    (define opcode-id (send machine get-inst-id (inst-op key)))
    (when opcode-id
      (define args (inst-args key))
      (define byte (inst-byte key))
      (define new-key (neon-inst opcode-id
                                 args
                                 (and byte (quotient byte 8))
                                 #f))
      
      ;; Represent schd-info using numbers so that solver can reason about it
      (define unit (schd-info-unit val))
      (define new-val (schd-info (if (equal? unit `lsbp) 0 1)
                                 (schd-info-issue val)
                                 (schd-info-latency val)
                                 (schd-info-from val)
                                 (schd-info-to val)))
      
      (vector-set! schedule-info opcode-id
                   (cons (cons new-key new-val) (vector-ref schedule-info opcode-id)))))

  ;; Main loop
  (for ([schd schedule])
       (update-schedule-info schd))

  ;; If an instruction has only one policy, 
  ;; reduce a list of one element to just that element.
  (for ([i ninsts])
       (let ([lst (vector-ref schedule-info i)])
         (when (and (list? list) (length list))
               (vector-set! schedule-info i (car list)))))

  ;;(pretty-display `(schedule-info ,schedule-info))
  schedule-info)