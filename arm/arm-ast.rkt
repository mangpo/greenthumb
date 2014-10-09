#lang racket

(require "../ast.rkt")
(provide (all-defined-out))

(struct arm-inst inst (cond))

(define-syntax-rule (inst-cond x) (arm-inst-cond x))
