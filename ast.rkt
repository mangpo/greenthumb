#lang racket

(provide (all-defined-out))

(struct block (body org info) #:mutable) 
;; info may include output constraint and # of recv data depending on arch
(struct call (name))
(struct label (name body info)) 
;; info may include size of preserved stack and precondition depeding on arch
(struct vardecl (val))
(struct forloop (init body bound))
(struct ift (t))     ;; exit if cond = 0
(struct iftf (t f))  ;; jump if cond = 0
(struct -ift (t))    ;; exit if cond >= 0
(struct -iftf (t f)) ;; jump if cond >= 0
(struct program (code memsize indexmap id))
(struct special (name))
(struct assumption (cnstr))
(struct item (x size))

;; Traverse a given program AST recursively until (base? program) is true.
;; Then apply base-apply to program.
(define (traverse program base? base-apply)
  ;; (pretty-display `(traverse ,program))
  (define (f x)
    ;; (pretty-display `(traverse-f ,x))
    (cond
     [(base? x)    (base-apply x)]
     [(list? x)    (map f x)]
     [(block? x)   (block (f (block-body x)) (f (block-org x)) (block-info x))]
     [(forloop? x) (forloop (f (forloop-init x)) (f (forloop-body x)) (forloop-bound x))]
     [(ift? x)     (ift (f (ift-t x)))]
     [(iftf? x)    (iftf (f (iftf-t x)) (f (iftf-f x)))]
     [(-ift? x)    (-ift (f (-ift-t x)))]
     [(-iftf? x)   (-iftf (f (-iftf-t x)) (f (-iftf-f x)))]
     [(item? x)    (f (item-x x))]
     [else         (raise (format "traverse: unimplemented for ~a" x))]
     ))
  (f program))

(define (last-block x)
  (pretty-display `(last-block ,x))
  (cond
   [(block? x)   x]
   [(list? x)    (last-block (last x))]
   [(forloop? x) (last-block (forloop-body x))]
   [(ift? x)     (last-block (ift-t x))]
   [(iftf? x)    (last-block (iftf-t x))]
   [(-ift? x)    (last-block (-ift-t x))]
   [(-iftf? x)   (last-block (-iftf-t x))]
   [else         (raise (format "last-block: unimplemented for ~a" x))]))