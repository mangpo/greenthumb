#lang racket

(require "../enumerator.rkt")

(provide llvm-enumerator%)

(define llvm-enumerator%
  (class enumerator%
    (super-new)))
