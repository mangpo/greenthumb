#lang racket

(provide (all-defined-out))

(struct block (body org cnstr assume) #:mutable)
(struct jump (name))
(struct label (name body simple))
(struct vardecl (val))
(struct forloop (init body))
(struct ift (t))     ;; exit if cond = 0
(struct iftf (t f))  ;; jump if cond = 0
(struct -ift (t))    ;; exit if cond >= 0
(struct -iftf (t f)) ;; jump if cond >= 0
(struct program (code memsize bit indexmap id))
(struct special (name))

(struct linklist (prev entry next))

;; (define (list->linklist lst)
;;   (define (copy x)
;;     (if (block? x)
;;         (struct-copy block x)
;;         x))

;;   (define (inner lst)
;;     (if (empty? lst)
;;         (linklist #f #f #f)
;;         (let* ([rest (inner (cdr lst))]
;;                [me (linklist #f (copy (car lst)) rest)])
;;           (when rest
;;             (set-linklist-prev! rest me))
;;           me)))
  
;;   (define non-empty (filter (lambda (x) (not (empty-block? x))) lst))
;;   (define start (inner non-empty))
;;   (define head (linklist #f #f start))
;;   (set-linklist-prev! start head)
;;   head)