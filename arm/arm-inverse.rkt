#lang racket

(require "../ops-racket.rkt" "../inst.rkt" "../inverse.rkt" "../enumerator.rkt" "arm-machine.rkt")

(provide arm-inverse%)

(define arm-inverse%
  (class inverse%
    (super-new)
    (inherit-field machine simulator)
    (define/override (gen-inverse-behavior my-inst)
      (define my-config (get-field config machine))
      (set-field! config machine 4) ;; arm uses at most 4 registers.
      (super gen-inverse-behavior my-inst)
      (set-field! config machine my-config)
      )

    (define/override (get-val-range type)
      (if (equal? type 'z)
          -1
          (super get-val-range type)))

    (define cmp-inst (get-field cmp-inst machine))
    (define/override (interpret-inst my-inst state [ref #f])
      (define (exec)
        ;; Remove conditional so that we don't change the flag.
        (define ops-vec (vector-copy (inst-op my-inst)))
        (vector-set! ops-vec 1 -1) 
        ;; Call super class's method to look up the inverse behavior tables.
        (super interpret-inst (inst ops-vec (inst-args my-inst)) state ref))

      (define op (vector-ref (inst-op my-inst) 0))
      (cond
       [(member op cmp-inst) (super interpret-inst my-inst state ref)]
       [else (exec-flag-backward my-inst state exec)]))

    (define (exec-flag-backward my-inst state exec)
      (define cond-id (vector-ref (inst-op my-inst) 1))
      (define z (progstate-z state))
      (define cond-type (send machine get-cond-opcode-name cond-id))
     
      (define (same) (list state))
      ;; TODO: z != -1

      (cond
       [(or (equal? cond-id -1) (equal? z -1))
        (exec)]

       [(equal? cond-type `eq) ;; eq
        (if (equal? z 0) (exec) (same))]

       [(equal? cond-type `ne) ;; ne
        (if (member z (list 1 2 3 4 5)) (exec) (same))]

       [(equal? cond-type `ls) ;; ls
        (if (member z (list 0 2 5)) (exec) (same))]

       [(equal? cond-type `hi) ;; hi
        (if (member z (list 3 4)) (exec) (same))]

       [(equal? cond-type `cc) ;; cc
        (if (member z (list 2 5)) (exec) (same))]

       [(equal? cond-type `cs) ;; cs
        (if (member z (list 0 3 4)) (exec) (same))]

       [(equal? cond-type `lt) ;; lt
        (if (member z (list 2 4)) (exec) (same))]
       
       [(equal? cond-type `ge) ;; ge
        (if (member z (list 0 3 5)) (exec) (same))]
       
       [else (raise (format "illegal cond-type ~a" cond-type))]))
      
    ))

#|
(require "arm-simulator-racket.rkt" "arm-parser.rkt" "arm-printer.rkt"
         "../memory-racket.rkt")

(define (test)
  
  (define parser (new arm-parser%))
  (define machine (new arm-machine% [config 4] [bitwidth 4]))
  
  (define printer (new arm-printer% [machine machine]))
  (define simulator-racket (new arm-simulator-racket% [machine machine]))
  (define inverse (new arm-inverse% [machine machine] [simulator simulator-racket]))

  (define code
    (send parser ir-from-string "
        str r0, [r2, #0]
"))
  (define encoded-code (send printer encode code))
  (define my-inst (vector-ref encoded-code 0))
  
  (define code2
    (send parser ir-from-string "
        strcc r0, [r2, #0]
"))
  (define encoded-code2 (send printer encode code2))
  (define my-inst2 (vector-ref encoded-code2 0))
  
  (send inverse gen-inverse-behavior my-inst)

  (define state1 (progstate '#(#f #f #f #f)
                           (new memory-racket% [update (make-hash '((-2 . 1)))])
                           2))
  (define state2 (progstate '#(#f #f #f #f)
                           (new memory-racket%)
                           3))
  (define live-out (progstate '#(#f #f #f #f) #t #f))

  (send inverse interpret-inst my-inst2 state1 live-out)
  )
|#
