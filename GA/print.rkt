#lang racket

(require "../ast.rkt" "state.rkt")

(provide (all-defined-out))

(define (print-syntax x w h id [original #t])

  (define node-offset 10)
  (define block-offset 800)
  (define-syntax-rule (core-id id w)
    (+ (* 100 (floor (/ id w))) (modulo id w) node-offset))
  (define-syntax-rule (inc indent)
    (string-append indent "  "))

  (define (small-body? l [size 0])
    (cond 
     [(empty? l) #t]
     [(or (block? (car l)) 
          (and (item? (car l)) (block? (item-x (car l)))))
      (define b (if (block? (car l)) (car l) (item-x (car l))))
      (for ([inst (string-split (block-body b))])
           (if (or (string->number inst)
                   (member inst (list "up" "down" "left" "right" "udlr" "io")))
               (set! size (+ size 5))
               (set! size (add1 size))))
      (and (< size 4) (small-body? (cdr l) size))]
     [else #f]))
  
  (define (f x indent)
    (cond
     [(string? x)
      (for ([i (string-split x)])
           (cond
            [(equal? i "nop") (void)]
            [(equal? i "+") (display ". + ")]
            [(equal? i "0") (display "dup dup or ")]
            [else (display i) (display " ")]))
      ]
      
     [(list? x)
      (for ([i x])
           (f i indent))]
     
     [(block? x)
      (f (block-body x) indent)]
     
     [(ift? x)
      (when original (display "| cr"))
      (newline)
      (display indent)
      (display ".. if ")
      (f (ift-t x) (inc indent))
      (display " then ")]
     
     [(iftf? x)
      (when original (display "| cr"))
      (newline)
      (display indent)
      (display ".. if ")
      (f (iftf-t x) (inc indent))
      (display " ; ] then ")
      (f (iftf-f x) (inc indent))]
     
     [(-ift? x)
      (when original (display "| cr"))
      (newline)
      (display indent)
      (display ".. -if ")
      (f (-ift-t x) (inc indent))
      (display " then ")]
     
     [(-iftf? x)
      (when original (display "| cr"))
      (newline)
      (display indent)
      (display ".. -if ")
      (f (-iftf-t x) (inc indent))
      (display " ; ] then ")
      (f (-iftf-f x) (inc indent))
      (newline)]
   
     [(forloop? x)
      (when original (display "| cr"))
      (newline)
      (display indent)
      
      (f (forloop-init x) indent)
      (display "for ")
      (when original (display "| cr"))
      (newline)
      (display (inc indent))
      (f (forloop-body x) (inc indent))
      (if (small-body? (forloop-body x))
          (display "unext ")
          (display "next "))]

     [(special? x)
      (when original (display "| cr"))
      (newline)
      (display indent)
      (cond
       [(equal? (special-name x) "mult")
        (display "a! dup dup or 17 for +* unext drop drop a")]
       [else (raise (format "print-syntax: unimplemented for special ~a" 
                            (special-name x)))])
      (when original (display "| cr"))
      (newline)
      (display indent)]
   
     [(call? x)
      (define name (call-name x))
      (when (or original (not (member name (list "in" "out"))))
            (display name)
            (display " "))
      ]
     
     [(item? x)
      (f (item-x x) indent)]
    
     [(label? x)
      (display (format ": ~a " (label-name x)))
      (when original (display "= $0 "))
      
      (f (label-body x) "  ")
      
      (when original 
            (when (equal? (label-name x) "main")
                  (display "warm "))
          (display "= $0 "))
      (display "; ")
      (when original (display "| cr"))
      (newline)
      ]
   
     [(vardecl? x)
      (for ([val (vardecl-val x)])
           (display val)
           (display " , "))
      (if original
          (pretty-display "| br")
          (pretty-display "green"))
      ]
     
     [(program? x)
      (define memsize (program-memsize x))
      (define node (core-id id w))

      (if original
          (begin
            (pretty-display (format "{block ~a}" (+ block-offset (* 2 id))))
            (pretty-display (format "( -) # ~a ( id ~a mem ~a) 0 org | cr" node id memsize)))
          (begin
            (pretty-display (format "yellow ~a node" id))
            (pretty-display (format "0 org"))))
      
      (f (program-code x) "")
      (newline)
      ]

     [(vector? x)
      (pretty-display "{block 790}")
      (pretty-display "host target | cr")
      (for ([id (* w h)])
           (when (vector-ref x id)
                 (pretty-display (format "~a node ~a load" 
                                         (core-id id w) (+ block-offset (* 2 id))))))
      
      (newline)
      (pretty-display "{block 792}")
      (pretty-display ": /node dup +node /ram ; | cr")
      (for ([id (* w h)])
           (when (vector-ref x id)
                 (pretty-display (format "~a /node $0 /p" (core-id id w)))))
      (newline)
      
      (for ([i (* w h)])
           (set! id i)
           (f (vector-ref x i) ""))
      ]

     [(or (equal? x #f) (assumption? x)) (void)]
      
     [else (raise (format "print-syntax: unimplemented for ~a" x))]))
  (f x ""))

(define (print-struct x [indent ""])
  (define (inc ind) (string-append ind "  "))
  (cond
   [(list? x)
    (pretty-display (format "~a(list" indent))
    (for ([i x]) (print-struct i (inc indent)))
    (pretty-display (format "~a)" indent))]

   [(block? x)
    (pretty-display (format "~a(block ~a" indent (block-body x)))
    (print-blockinfo (block-info x) (inc indent))
    (pretty-display ")")]

   [(forloop? x)
    (pretty-display (format "~a(forloop" indent))
    (print-struct (forloop-init x) (inc indent))
    (print-struct (forloop-body x) (inc indent))
    (pretty-display (format "~a)" indent))]

   [(ift? x)
    (pretty-display (format "~a(ift" indent))
    (print-struct (ift-t x) (inc indent))
    (pretty-display (format "~a)" indent))]

   [(iftf? x)
    (pretty-display (format "~a(iftf" indent))
    (print-struct (iftf-t x) (inc indent))
    (print-struct (iftf-f x) (inc indent))
    (pretty-display (format "~a)" indent))]

   [(-ift? x)
    (pretty-display (format "~a(-ift" indent))
    (print-struct (-ift-t x) (inc indent))
    (pretty-display (format "~a)" indent))]

   [(-iftf? x)
    (pretty-display (format "~a(-iftf" indent))
    (print-struct (-iftf-t x) (inc indent))
    (print-struct (-iftf-f x) (inc indent))
    (pretty-display (format "~a)" indent))]

   [(item? x)
    (pretty-display (format "~a(item" indent))
    (print-struct (item-x x) (inc indent))
    (pretty-display (format "~a)" indent))]

   [(vardecl? x)
    (pretty-display (format "~a(vardecl ~a)" indent (vardecl-val x)))]
   
   [(label? x)
    (pretty-display (format "~a(label ~a" indent (label-name x)))
    (print-struct (label-body x) (inc indent))
    (pretty-display (format "~a)" indent))]
   
   [else
    (pretty-display (format "~a~a" indent x))]))