#lang racket

(require "controller.rkt" "ast.rkt" "f18a.rkt")

(define a
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
   5 #f))

;; With assumption
(define b
  (program
   (list
    (label
     "rep"
     (list
      (assumption '(a . (= . right)))
      (block "right a! !" "right a! !" (blockinfo '() 0)))
     (labelinfo 0 0 #f))
    ;; (label
    ;;  "main"
    ;;  (list
    ;;   (assumption '(b . (= . right)))
    ;;   (block "right b! !b" "right b! !b" (blockinfo '() 0)))
    ;;  (labelinfo 0 0 #f))
    )
   0 #f))

;; For loop
(define c
  (program
   (list
    (label
     "main"
     (list
      (block "nop nop 0 a!" "nop nop 0 a!" (blockinfo '(a) 0))
      (forloop 
       (list 
        (block "3" "3" (blockinfo '((data . 1) a) 0)))
       (list 
        (block
         "@+ right b! !b"
         "@+ right b! !b" (blockinfo '((return . 1) a) 0)))
       4)
      )
     (labelinfo 0 0 #f))
    )
   4 #f))

;; Decompress
(define d
  (program
   (list
    (label
     "main"
     (list
      (block "1 b! @b 2 b! @b 1 +" "5 b! @b 6 b! @b 1 +" (blockinfo '((data . 2)) 0))
      )
     (labelinfo 0 0 #f))
    )
   3 #hash((0 . 0) (1 . 5) (2 . 6) (3 . 7))))

;; Sliding window
(define e
  (program
   (list
    (label
     "main"
     (list
      (block "1 2 3" "1 2 3" (blockinfo '((data . 3)) 0))
      (block "1 2 3" "1 2 3" (blockinfo '((data . 6)) 0))
      )
     (labelinfo 0 0 #f))
    )
   3 #f))

(define f
  (program
   (list
    (label
     "main"
     (list
      (assumption '(stack . ((<= . 65535) (<= . 65535) (<= . 65535))))
      (block 
       "0 a! !+ push !+ pop dup 1 b! @b 0 b! @b 65535 or over - and + or push drop pop"
       "0 a! !+ push !+ pop dup 1 b! @b 0 b! @b 65535 or over - and + or push drop pop"
       (blockinfo '((data . 1)) 0))
      (block "0 +" "0 +" (blockinfo '((data . 1)) 0))
      )
     (labelinfo 0 0 #f))
    )
   2 #f))

(define g
  (program 
   (list 
    (vardecl '())
    (label "main"
           (list 
            (block
             "right b! @b"
             "right b! @b"
             (blockinfo '((data . 1) (return . 0) memory ) 1))
            (call "out")
            )
           (labelinfo 0 0 #f))
    )
   0 #f))

(define h
  (program
   (list
    (label "main"
           (list
            (block "dup drop 1" "dup drop 1" (blockinfo '((data . 1)) 0))
            (forloop
             (list)
            (list (block "2*" "2*" (blockinfo '((data . 1)) 0)))
            2
            ))
           (labelinfo 0 0 #f)))
   0 #f))

(define i
  (program
   (list
    (label "main"
           (list
            (forloop 
             (list 
              (block
               "15"
               "15"
               (blockinfo '((data . 2) (return . 0) memory ) 0))
              )
             (list 
              (block
               "dup"
               "dup"
               (blockinfo '((data . 2) (return . 1) memory ) 0))
              (block
               "right b! @b"
               "right b! @b"
               (blockinfo '((data . 3) (return . 1) memory ) 1))
              (block
               "+"
               "+"
               (blockinfo '((data . 2) (return . 1) memory ) 0))
              (block
               "push drop pop"
               "push drop pop"
               (blockinfo '((data . 1) (return . 1) memory ) 0)))
             16)
            (block
             "0 b! @b"
             "0 b! @b"
             (blockinfo '((data . 2) (return . 0) memory ) 0)))
           (labelinfo 0 0 #f)))
   1 #f))

(define j
  (program 
   (list 
    (label "main"
	   (list 
	    (forloop 
	     (list 
	      (block
	       "255"
	       "255"
	       (blockinfo '((data . 1) (return . 0) memory ) 0))
	      )
	     (list 
	      (call "in")
	      (block
	       "right b! !b"
	       "right b! !b"
	       (blockinfo '((data . 0) (return . 1) memory ) 0))
	      (call "in")
	      (block
	       "down b! !b"
	       "down b! !b"
	       (blockinfo '((data . 0) (return . 1) memory ) 0))
	      )
	     256)
	    (call "25rep")
	    (assumption '(b . (= . down)))
	    (block
	     "@b"
	     "@b"
	     (blockinfo '((data . 1) (return . 0) memory ) 1))
	    (block
	     "right b! !b"
	     "right b! !b"
	     (blockinfo '((data . 0) (return . 0) memory ) 0))
	    (call "25rep")
	    (assumption '(b . (= . down)))
	    (block
	     "@b"
	     "@b"
	     (blockinfo '((data . 1) (return . 0) memory ) 1))
	    (block
	     "right b! !b"
	     "right b! !b"
	     (blockinfo '((data . 0) (return . 0) memory ) 0))
	    )
	   (labelinfo 0 0 #f))
    )
   0 #f))

(define t (current-seconds))
(print-syntax (optimize j) 2 2 0)
(pretty-display `(time ,(- (current-seconds) t)))
