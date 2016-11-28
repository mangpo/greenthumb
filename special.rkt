#lang racket

(provide (all-defined-out))

(define (get-memory-type)    'mem%)
(define (get-queue-in-type)  'queue-in%)
(define (get-queue-out-type) 'queue-out%)

(define special-type-list
  (list (get-memory-type) (get-queue-in-type) (get-queue-out-type)))
(define (special-type? type) (member type special-type-list))

(define special% (class object% (super-new)))
