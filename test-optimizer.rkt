#lang racket

(require "controller.rkt" "ast.rkt" "f18a.rkt")

(define x
  (program
   (list
    (vardecl '(32867 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (label "sumrotate"
      ;; linklist
      (list 
        (block
          "dup"
          "dup" (blockinfo '((data . 2)) 0))
        (block
          "right b! !b"
          "right b! !b" (blockinfo '((data . 1)) 0))
        (block
          "drop"
          "drop" (blockinfo '() 0)))
      (labelinfo 1  0 #f)
      )
    (label "main"
      (list
       (forloop 
        ;; linklist
        (list 
         (block
          "15"
          "15" (blockinfo '((data . 2)) 0))
         )
        ;; linklist
        (list 
         (block
          "dup"
          "dup" (blockinfo '((data . 2) (return . 1)) 0))
         (block
          "b! @b drop"
          "b! @b drop" (blockinfo '((data . 1) (return . 1)) 0)))
        16)
        (call "sumrotate")
       )
      (labelinfo 0 0 #f)
      )
    )
   5 #f #f))

(define y
  (program
   (list
    (label
     "rep"
     (list
      (assumption '(a . (= . right)))
      (block "right a! !" "right a! !" (blockinfo 0 0)))
     (labelinfo 0 0 #f))
    (label
     "main"
     (list
      (assumption '(b . (= . right)))
      (block "right b! !b" "right b! !b" (blockinfo 0 0)))
     (labelinfo 0 0 #f))
    )
   0 #f #f))

(define t (current-seconds))
(print-struct (optimize y))
(pretty-display `(time ,(- (current-seconds) t)))