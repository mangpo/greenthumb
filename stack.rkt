;;; Some utilities for working with 8-word circular stacks
#lang s-exp rosette

(provide (except-out (all-defined-out) modulo- modulo+))

(struct stack (sp body) #:mutable #:transparent)

(define-syntax-rule (modulo- x y) (if (< x 0) (+ x y) x))
(define-syntax-rule (modulo+ x y) (if (>= x 8) (- x y) x))

;;; Copies the given stack. This keeps mutable vectors from being
;;; shared between different stacks.
(define (copy-stack s)
  (stack (stack-sp s) (vector-copy (stack-body s))))

;;; Print a circular stack:
(define (display-stack stack)
  (for [(i (in-range 0 8))]
       (display (format " ~a" (vector-ref (stack-body stack)
                                          (modulo- (- (stack-sp stack) i) 8))))))

;;; Get item i from the stack
(define (get-stack stack i)
  (vector-ref (stack-body stack) (modulo- (- (stack-sp stack) i) 8)))

;;; Set item i in the stack
(define (set-stack! stack i x)
  (vector-set! (stack-body stack) (modulo- (- (stack-sp stack) i) 8) x))

;;; Pushes a value to the given stack's body.
(define (push-stack! stack value)
  (set-stack-sp! stack (modulo+ (add1 (stack-sp stack)) 8))
  (vector-set! (stack-body stack) (stack-sp stack) value))

;;; Pops from the given stack's body.
(define (pop-stack! stack)
  (let ([ret-val (vector-ref (stack-body stack) (stack-sp stack))])
    (set-stack-sp! stack (modulo- (sub1 (stack-sp stack)) 8))
    ret-val))
