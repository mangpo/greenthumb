#lang racket

(require "../inst.rkt")
(provide (all-defined-out))

(struct neon-inst inst (byte type))

;; (define-syntax inst
;;   (syntax-rules ()
;;     ((inst a b) (instruction a b #f #f))
;;     ((inst a b c) (instruction a b c #f))
;;     ((inst a b c d) (instruction a b c d))))

(define-syntax-rule (inst-type x) (neon-inst-type x))
(define-syntax-rule (inst-byte x) (neon-inst-byte x))