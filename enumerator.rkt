#lang racket

(require racket/generator)
(provide enumerator% all-combination-list all-combination-gen)

(define enumerator%
  (class object%
    (super-new)
    (abstract generate-inst)
    (public get-flag)

    (define (get-flag state) #f)

    ))

;; Return a list of all possible combinations of flat lists
(define (all-combination-list list-of-list)
  (define ret (list))
  (define (recurse in-list out-list)
    (cond
     [(empty? in-list) (set! ret (cons out-list ret))]
     [else
      (let ([rest (cdr in-list)]
            [x (car in-list)])
        (if (or (vector? x) (list? x))
            (for ([val x])
                 (recurse rest (cons val out-list)))
            (recurse rest (cons x out-list))))
      ]))

  (recurse (reverse list-of-list) (list))
  ret)

;; Return a generator of all possible combinations of flat lists
(define (all-combination-gen list-of-list)
  (define iterator
    (generator 
     ()
     (define (recurse in-list out-list)
       (cond
        [(empty? in-list) (yield out-list)]
        [else
         (let ([rest (cdr in-list)]
               [x (car in-list)])
           (if (or (vector? x) (list? x))
               (for ([val x])
                    (recurse rest (cons val out-list)))
               (recurse rest (cons x out-list))))
         ]))
     (recurse (reverse list-of-list) (list))
     (yield #f)
     ))

  iterator)
