#lang racket

(require "../ast.rkt")
(provide (all-defined-out))

(struct arm-inst inst (shfop shfarg cond))

(define-syntax-rule (inst-cond x) (arm-inst-cond x))
(define-syntax-rule (inst-shfop x) (arm-inst-shfop x))
(define-syntax-rule (inst-shfarg x) (arm-inst-shfarg x))
