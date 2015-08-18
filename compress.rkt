#lang racket

(require "machine.rkt")

(provide compress%)

(define compress%
  (class object%
    (super-new)
    (init-field machine)
    (public compress-reg-space decompress-reg-space)

    ;; Default: no compression
    (define (compress-reg-space program live-out live-in)
      (values program
              live-out
              live-in
              #f
              (send machine get-config)))

    (define (decompress-reg-space program reg-map) program)
    

    ))