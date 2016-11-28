#lang racket

(require "../forwardbackward.rkt" "../inst.rkt"
         "arm-machine.rkt")

(provide arm-forwardbackward%)

(define arm-forwardbackward%
  (class forwardbackward%
    (super-new)
    (inherit-field machine printer)
    (override len-limit window-size
              try-cmp? sort-live sort-live-bw)

    ;; Num of instructions that can be synthesized within a minute.
    (define (len-limit) 2)

    ;; Context-aware window decomposition size L.
    ;; The cooperative search tries L/2, L, 2L, 4L.
    (define (window-size) 4)

    ;; Mask in only the live values. If an entry in progstate is not live, set it to #f.
    ;; state-vec: progstate in vector/list/pair format
    ;; live-list: liveness in vector/list/pair format
    ;; keep-flag: if #f, set flag to default value.
    ;; output: masked progstate in vector/list/pair format
    (define/override (mask-in state-vec live-list #:keep-flag [keep #t])
      (define masked (super mask-in state-vec live-list #:keep-flag keep))
      (if keep
          (let ([z (progstate-z state-vec)])
            (set-progstate-z! masked z))
          (set-progstate-z! masked -1))
      masked)

    (define cmp-inst (get-field cmp-inst machine))
    
    ;; Analyze if we should include comparing instructions into out instruction pool.
    ;; code: input program
    ;; state: program state in progstate format
    ;; live: live-in information in progstate format
    (define (try-cmp? code state live)
      (define z (progstate-z state))
      (define live-z (progstate-z live))
      (define use-cond1 (for/or ([x code]) (member (vector-ref (inst-op x) 0) cmp-inst)))
      (define use-cond2 (for/or ([x code]) (not (= (vector-ref (inst-op x) 1) -1))))

      (cond
       [(and live-z (> z -1) use-cond1) 1] ;; must try cmp
       [(and (> z -1) (or use-cond1 use-cond2)) 2] ;; should try cmp
       [else 0] ;; don't try cmp
       ))

    ;; Sort liveness. Say we have program prefixes that have different live-outs.
    ;; If liveness A comes before B, program prefix with live-out A will be considered before program prefix with live-out B.
    (define (sort-live keys)
      (sort keys (lambda (x y) (> (vector-count identity (progstate-regs (entry-live x)))
                                  (vector-count identity (progstate-regs (entry-live y)))))))

    ;; Similar to 'sort-live' but for backward direction (program postfixes).
    (define (sort-live-bw keys)
      (sort keys (lambda (x y) (> (vector-count identity (progstate-regs x)) 0))))

    ))
