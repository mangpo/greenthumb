#lang racket

(require "../machine.rkt" "../enumerator.rkt" "../inst.rkt")
(require racket/generator)

(provide $-enumerator%)

(define $-enumerator%
  (class enumerator%
    (super-new)
    (init-field machine printer)
    (override generate-inst)
    
    (define opcodes (get-field opcodes machine))

    (define (generate-inst live-in live-out flag-in flag-out
                           #:no-args [no-args #f] #:try-cmp [try-cmp #f])

      (define mode (cond [no-args `no-args] [else `basic]))
      ;; Mode `no-args is for generating instructions to be used for creating
      ;; inverse behaviors, so we don't have to enumerate variables/registers.
      
      (define inst-pool (get-field inst-pool machine))
      
      (define iterator
        (generator 
         ()
         (for ([opcode-id (shuffle inst-pool)])
              (yield (inst opcode-id ?)))
         ))
      iterator)

    ))
