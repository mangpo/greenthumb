#lang racket

(require "../machine.rkt" "../enumerator.rkt" "arm-machine.rkt")
(require racket/generator)

(provide arm-enumerator%)

(define arm-enumerator%
  (class enumerator%
    (super-new)
    (inherit-field machine)
    
    (define cmp-inst (get-field cmp-inst machine))

    (define/override (get-pruning-info state-vec) (progstate-z state-vec))
    
    (define/override (filter-with-pruning-info opcode-pool flag-in flag-out
                                               #:no-args [no-args #f] #:try-cmp [try-cmp #f])
      (define ret
        (cond
         [try-cmp
          (cond
           ;; flags are different, need to insert cmp instructions.
           [(and (number? flag-in) (list? flag-out)
                 (not (member flag-in flag-out)))
            (filter (lambda (ops-vec) (member (vector-ref ops-vec 0) cmp-inst)) opcode-pool)]

           ;; no conditional flag, don't use conditional opcodes.
           [(equal? flag-in -1)
            (filter (lambda (ops-vec) (= (vector-ref ops-vec 1) -1)) opcode-pool)]

           ;; no restriction.
           [else opcode-pool]
           )
          ]

         [else
          ;; don't use cmp instructions and conditional opcodes.
          (filter (lambda (ops-vec)
                    (and (not (member (vector-ref ops-vec 0) cmp-inst))
                         (= (vector-ref ops-vec 1) -1)))
                  opcode-pool)
          ]))

      (if no-args
          ;; don't enumerate conditional opcodes
          (filter (lambda (ops-vec) (= -1 (vector-ref ops-vec 1))) ret)
          ret)
      )
    
    ))
