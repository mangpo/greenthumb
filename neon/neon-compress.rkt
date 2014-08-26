#lang racket

(require "../compress.rkt" "../ast.rkt")

(provide neon-compress%)

(define neon-compress%
  (class compress%
    (super-new)
    (inherit-field machine)
    (override compress-reg-space)

    
    (define (compress-reg-space program live-out)
      (define max-dreg 0)
      (define max-rreg 0)
      (define mem #f)

      ;; Collect all used register ids.
      (define (inner-collect args)
        (for ([r args])
             (cond
              [(vector? r) (inner-collect r)]
              [(equal? (substring r 0 1) "d")
               (let ([reg-id (string->number (substring r 1))])
                 (when (> reg-id max-dreg) (set! max-dreg reg-id)))]
              [(equal? (substring r 0 1) "q")
               (let ([reg-id (add1 (* 2 (string->number (substring r 1))))])
                 (when (> reg-id max-dreg) (set! max-dreg reg-id)))]
              [(equal? (substring r 0 1) "r")
               (let ([reg-id (string->number (substring r 1))])
                 (when (> reg-id max-rreg) (set! max-rreg reg-id)))]
              )))

      (define (check-mem op)
        (unless mem
                (set! mem (member (string->symbol op) '(vld1 vld1! vld2 vld2! vst1 vst1! vst2 vst2!)))))

      (for ([x program]) 
           (inner-collect (inst-args x))
           (check-mem (inst-op x))
           )

      (values program
              live-out
              #f
              (list (max (add1 max-dreg) 4) 
                    (max (add1 max-rreg) 1)
                    (if mem 4 1))))

    ))