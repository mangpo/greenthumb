#lang racket

(require "../controller.rkt" "../inst.rkt" 
	 "interpret.rkt" "state.rkt" "print.rkt")

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


(define b
  (program 
  (list 
    (label "16rep"
      (list 
        (block
          "down b! @b"
          "down b! @b"
          (blockinfo '((data . 1) (return . 0) memory ) 1))
        (block
          "down b! @b"
          "down b! @b"
          (blockinfo '((data . 2) (return . 0) memory ) 1))
        (block
          "down b! @b"
          "down b! @b"
          (blockinfo '((data . 3) (return . 0) memory ) 1))
      )
      (labelinfo 1 2 #f))
    (label "main"
      (list 
        (block
          "0"
          "0"
          (blockinfo '((data . 1) (return . 0) memory ) 0))
        (forloop 
          (list 
            (block
              "1"
              "15"
              (blockinfo '((data . 2) (return . 0) memory ) 0))
          )
          (list 
            (call "16rep")
            (call "fff")
            (block
              "push drop pop"
              "push drop pop"
              (blockinfo '((data . 1) (return . 1) memory ) 0))
            (block
              "dup"
              "dup"
              (blockinfo '((data . 2) (return . 1) memory ) 0))
            (block
              "down b! !b"
              "down b! !b"
              (blockinfo '((data . 1) (return . 1) memory ) 0))
          )
          16)
        (forloop 
          (list 
            (block
              "1"
              "15"
              (blockinfo '((data . 2) (return . 0) memory ) 0))
          )
          (list 
            (call "16rep")
            (call "ggg")
            (block
              "push drop pop"
              "push drop pop"
              (blockinfo '((data . 1) (return . 1) memory ) 0))
            (block
              "dup"
              "dup"
              (blockinfo '((data . 2) (return . 1) memory ) 0))
            (block
              "down b! !b"
              "down b! !b"
              (blockinfo '((data . 1) (return . 1) memory ) 0))
          )
          16)
        (forloop 
          (list 
            (block
              "1"
              "15"
              (blockinfo '((data . 2) (return . 0) memory ) 0))
          )
          (list 
            (call "16rep")
            (call "hhh")
            (block
              "push drop pop"
              "push drop pop"
              (blockinfo '((data . 1) (return . 1) memory ) 0))
            (block
              "dup"
              "dup"
              (blockinfo '((data . 2) (return . 1) memory ) 0))
            (block
              "down b! !b"
              "down b! !b"
              (blockinfo '((data . 1) (return . 1) memory ) 0))
          )
          16)
        (forloop 
          (list 
            (block
              "1"
              "15"
              (blockinfo '((data . 2) (return . 0) memory ) 0))
          )
          (list 
            (call "16rep")
            (call "iii")
            (block
              "push drop pop"
              "push drop pop"
              (blockinfo '((data . 1) (return . 1) memory ) 0))
            (block
              "dup"
              "dup"
              (blockinfo '((data . 2) (return . 1) memory ) 0))
            (block
              "down b! !b"
              "down b! !b"
              (blockinfo '((data . 1) (return . 1) memory ) 0))
          )
          16)
      )
      (labelinfo 0 0 #f))
  )
3 #f))

(define c
  (program 
  (list 
    (label "main"
      (list 
        (forloop 
          (list 
            (block
              "0 a! 1"
              "0 a! 31"
              (blockinfo '((data . 1) (return . 0) memory a ) 0))
          )
          (list 
            (block
              "@+"
              "@+"
              (blockinfo '((data . 1) (return . 1) memory a ) 0))
            (block
              "a push"
              "a push"
              (blockinfo '((data . 1) (return . 2) memory a ) 0))
            (call "sumrotate")
            (block
              "pop a!"
              "pop a!"
              (blockinfo '((data . 0) (return . 1) memory a ) 0))
          )
          32)
        (forloop 
          (list 
            (block
              "1"
              "31"
              (blockinfo '((data . 1) (return . 0) memory a ) 0))
          )
          (list 
            (block
              "down b! @b"
              "down b! @b"
              (blockinfo '((data . 1) (return . 1) memory ) 1))
            (call "sumrotate")
          )
          32)
      )
      (labelinfo 0 0 #f))
  )
2 #hash((0 . 0) (2 . 32)))
)

(define d
(program 
  (list 
    (vardecl '(0 0 0 0 0))
    (label "leftrotate"
      (list 
        (block
          "0"
          "0"
          (blockinfo '((data . 1) (return . 0) memory ) 0))
        (block
          "right b! @b"
          "right b! @b"
          (blockinfo '((data . 2) (return . 0) memory ) 1))
        (block
          "push drop pop"
          "push drop pop"
          (blockinfo '((data . 1) (return . 0) memory ) 0))
        (block
          "dup"
          "dup"
          (blockinfo '((data . 2) (return . 0) memory ) 0))
        (block
          "push drop pop"
          "push drop pop"
          (blockinfo '((data . 1) (return . 0) ) 0))
      )
      (labelinfo 2 3 #t))
    (label "csum"
      (list 
        (block
          "0 a! !+ !+"
          "0 a! !+ !+"
          (blockinfo '((data . -2) (return . 0) memory ) 0))
        (block
          "0"
          "0"
          (blockinfo '((data . -1) (return . 0) memory ) 0))
        (block
          "1 b! @b"
          "1 b! @b"
          (blockinfo '((data . 0) (return . 0) memory ) 0))
        (block
          "0 b! @b"
          "0 b! @b"
          (blockinfo '((data . 1) (return . 0) memory ) 0))
        (block
          "+"
          "+"
          (blockinfo '((data . 0) (return . 0) memory ) 0))
        (block
          "down b! @b"
          "down b! @b"
          (blockinfo '((data . 1) (return . 0) memory ) 1))
        (block
          "3"
          "3"
          (blockinfo '((data . 2) (return . 0) memory ) 0))
        (block
          "and"
          "and"
          (blockinfo '((data . 1) (return . 0) memory ) 0))
        (block
          "+"
          "+"
          (blockinfo '((data . 0) (return . 0) memory ) 0))
        (block
          "push drop pop"
          "push drop pop"
          (blockinfo '((data . -1) (return . 0) memory ) 0))
        (block
          "dup"
          "dup"
          (blockinfo '((data . 0) (return . 0) memory ) 0))
        (block
          "65535"
          "65535"
          (blockinfo '((data . 1) (return . 0) memory ) 0))
        (block
          "and"
          "and"
          (blockinfo '((data . 0) (return . 0) memory ) 0))
        (block
          "push drop pop"
          "push drop pop"
          (blockinfo '((data . -1) (return . 0) memory ) 0))
        (block
          "dup"
          "dup"
          (blockinfo '((data . 0) (return . 0) memory ) 0))
        (block
          "push drop pop"
          "push drop pop"
          (blockinfo '((data . -1) (return . 0) ) 0))
      )
      (labelinfo 3 3 #f))
    (label "sumrotate"
      (list 
        (block
          "2 a! push !+ !+ pop"
          "2 a! push !+ !+ pop"
          (blockinfo '((data . -2) (return . 0) memory ) 0))
        (block
          "3 b! @b"
          "3 b! @b"
          (blockinfo '((data . -1) (return . 0) memory ) 0))
        (block
          "over"
          "over"
          (blockinfo '((data . 0) (return . 0) memory ) 0))
        (block
          "+"
          "+"
          (blockinfo '((data . -1) (return . 0) memory ) 0))
        (block
          "up b! @b"
          "up b! @b"
          (blockinfo '((data . 0) (return . 0) memory ) 1))
        (block
          "+"
          "+"
          (blockinfo '((data . -1) (return . 0) memory ) 0)) ; 15
        (block
          "left b! @b"
          "left b! @b"
          (blockinfo '((data . 0) (return . 0) memory ) 1))
        (call "csum")
        (block
          "4 b! !b"
          "4 b! !b"
          (blockinfo '((data . -2) (return . 0) memory ) 0))
        (block
          "2 b! @b"
          "2 b! @b"
          (blockinfo '((data . -1) (return . 0) memory ) 0))
        (block
          "4 b! @b"
          "4 b! @b"
          (blockinfo '((data . 0) (return . 0) memory ) 0))
        (block
          "right b! !b"
          "right b! !b"
          (blockinfo '((data . -1) (return . 0) memory ) 0))
        (call "leftrotate")
        (call "csum")
        (block
          "push drop pop"
          "push drop pop"
          (blockinfo '((data . -2) (return . 0) ) 0))
      )
      (labelinfo 3 2 #f))
    (label "main"
      (list 
        (forloop 
          (list 
            (block
              "1"
              "63"
              (blockinfo '((data . 1) (return . 0) memory a ) 0))
          )
          (list 
            (block
              "left b! @b"
              "left b! @b"
              (blockinfo '((data . 1) (return . 1) memory ) 1))
            (block
              "left b! @b"
              "left b! @b"
              (blockinfo '((data . 2) (return . 1) memory ) 1))
            (block
              "left b! @b"
              "left b! @b"
              (blockinfo '((data . 3) (return . 1) memory ) 1))
            (call "sumrotate")
            (block
              "left b! !b"
              "left b! !b"
              (blockinfo '((data . 0) (return . 1) memory ) 0))
          )
          64)
      )
      (labelinfo 0 0 #f))
  )
5 #f)
)

(print-struct
(optimize d))