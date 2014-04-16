#lang racket

(require "controller.rkt" "ast.rkt" "f18a.rkt" "state.rkt")

(define a
  (program 
   (list 
    (label "cadd"
      (list 
        (block
          "0 a! !+ !+ !+ !+"
          "0 a! !+ !+ !+ !+"
          (blockinfo '((data . -4) (return . 0) memory ) 0))
        (block
          "0"
          "0"
          (blockinfo '((data . -3) (return . 0) memory ) 0))
        (block
          "3 b! @b"
          "3 b! @b"
          (blockinfo '((data . -2) (return . 0) memory ) 0))
        (block
          "1 b! @b"
          "1 b! @b"
          (blockinfo '((data . -1) (return . 0) memory ) 0))
        (block
          "+"
          "+"
          (blockinfo '((data . -2) (return . 0) memory ) 0)) ;14
        (block
          "push drop pop"
          "push drop pop"
          (blockinfo '((data . -3) (return . 0) memory ) 0))
        (block
          "2 b! @b"
          "2 b! @b"
          (blockinfo '((data . -2) (return . 0) memory ) 0))
        (block
          "0 b! @b"
          "0 b! @b"
          (blockinfo '((data . -1) (return . 0) memory ) 0))
        (block
          "+"
          "+"
          (blockinfo '((data . -2) (return . 0) memory ) 0))
        (block
          "over"
          "over"
          (blockinfo '((data . -1) (return . 0) memory ) 0))
        (block
          "2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/"
          "2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/"
          (blockinfo '((data . -1) (return . 0) memory ) 0))
        (block
          "3"
          "3"
          (blockinfo '((data . 0) (return . 0) memory ) 0))
        (block
          "and"
          "and"
          (blockinfo '((data . -1) (return . 0) memory ) 0))
        (block
          "+"
          "+"
          (blockinfo '((data . -2) (return . 0) memory ) 0))
        (block
          "4 b! !b"
          "4 b! !b"
          (blockinfo '((data . -3) (return . 0) memory ) 0))
        (block
          "dup"
          "dup"
          (blockinfo '((data . -2) (return . 0) memory ) 0))
        (block
          "65535"
          "65535"
          (blockinfo '((data . -1) (return . 0) memory ) 0))
        (block
          "and"
          "and"
          (blockinfo '((data . -2) (return . 0) memory ) 0))
        (block
          "push drop pop"
          "push drop pop"
          (blockinfo '((data . -3) (return . 0) memory ) 0))
        (block
          "4 b! @b"
          "4 b! @b"
          (blockinfo '((data . -2) (return . 0) memory ) 0))
        (block
          "65535"
          "65535"
          (blockinfo '((data . -1) (return . 0) memory ) 0))
        (block
          "and"
          "and"
          (blockinfo '((data . -2) (return . 0) memory ) 0))
        (block
          "4 b! !b"
          "4 b! !b"
          (blockinfo '((data . -3) (return . 0) memory ) 0))
        (block
          "dup"
          "dup"
          (blockinfo '((data . -2) (return . 0) memory ) 0))
        (block
          "4 b! @b"
          "4 b! @b"
          (blockinfo '((data . -1) (return . 0) memory ) 0))
        (block
          "push push drop pop pop"
          "push push drop pop pop"
          (blockinfo '((data . -2) (return . 0) ) 0))
      )
      (labelinfo 4 3 #f))
    )
   7 #hash((0 . 0) (1 . 1) (2 . 2) (3 . 3) (4 . 4) (6 . 6) (7 . 7))))

(optimize a)