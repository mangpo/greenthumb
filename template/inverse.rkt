#lang racket

(require "../ops-racket.rkt" "../inst.rkt" "../inverse.rkt" "../enumerator.rkt")

(provide $-inverse%)

(define $-inverse%
  (class inverse%
    (super-new)
    (init-field machine simulator)
    (override gen-inverse-behavior interpret-inst)

    ;; Reduced-bit
    (define bit (get-field bit machine))
    (define inst-id (get-field inst-id machine))
    (define val-range
      (for/vector ([v (arithmetic-shift 1 bit)]) (finitize v bit)))

    ;; Inverse tables for all instructions.
    (define behaviors-bw (make-hash))

    ;; Generate inverse table behavior for my-inst.
    (define (gen-inverse-behavior my-inst)
      ;; Inverse table behavior
      (define behavior-bw (make-hash))
      ?
      (hash-set! behaviors-bw ? behavior-bw))

    
    ;; Inverse interpret my-inst using the pre-computed 'behaviors-bw'.
    ;; output: a list of program states
    (define (interpret-inst my-inst state liveout) ?)

    ))
