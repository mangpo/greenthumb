;;; A bit-level arrayForth interpreter.
#lang racket

(require "stack.rkt")

(provide compress decompress)

(define (clean-stack)
  (stack 0 (vector (set) (set) (set) (set) (set) (set) (set) (set))))

;;; stacks:
(define data   (clean-stack))
(define return (clean-stack))

;;; registers:
(define a (set))
(define b (set))
(define p (set))
(define i (set))
(define r (set))
(define s (set))
(define t (set))
(define memory (set))
(define index-set (set))

(define (reset!)
  (set! index-set (set))
  (set! a (set))
  (set! b (set))
  (set! p (set))
  (set! i (set))
  (set! r (set))
  (set! s (set))
  (set! t (set))
  (set! memory (set))
  (set! data (clean-stack))
  (set! return (clean-stack)))

;;; Print the return stack:
(define (display-return)
  (display (format "|r> ~x" r))
  (display-stack return)
  (newline))

(define (display-vector vec n name)
  (when (> n 0)
	(display name)
	(for ([i (in-range 0 n)])
	     (display (format "~x " (vector-ref vec i))))
	(newline)))

;;; Pushes to the data stack.
(define (push! value)
  (push-stack! data s)
  (set! s t)
  (set! t value))

;;; Pushes to the return stack.
(define (r-push! value)
  (push-stack! return r)
  (set! r value))

;;; Pops from the data stack.
(define (pop!)
  (let ([ret-val t])
    (set! t s)
    (set! s (pop-stack! data))
    ret-val))

;;; Pops from the return stack.
(define (r-pop!)
  (let ([ret-val r])
    (set! r (pop-stack! return))
    ret-val))

;;; Return the value of p or a incremented as appropriately. If the
;;; register points to an IO region, does nothing. Otherwise increment
;;; the register circularly within the current memory region (RAM or
;;; ROM).
(define (incr curr)
  (cond [(< curr #x07F) (add1 curr)]
        [(= curr #x07F) #x000]
        [(< curr #x0FF) (add1 curr)]
        [(= curr #x0FF) #x080]
        [else curr]))


;;; Read from the given memory address or communication port. If it
;;; gets a communication port, it just returns a random number (for
;;; now).
(define (read-memory addr)
  (set! index-set (set-union index-set addr))
  memory)

;;; Write to the given memeory address or communication
;;; port. Everything written to any communication port is simply
;;; aggregated into a list.
(define (set-memory! addr value)
  (set! index-set (set-union index-set addr))

  (set! memory (if (set? memory)
                   (set-union memory value)
                   value)))


(define (track-index program)
  (reset!)

  (for ([inst program]
        [i (in-range (length program))])
    (cond
      [(member inst (list "@+" "@"))
       (push! (read-memory a))]
      
      [(member inst (list "@b"))
       (push! (read-memory b))]
      
      [(member inst (list "!+" "!"))
       (set-memory! a (pop!))]
      
      [(member inst (list "!b"))
       (set-memory! b (pop!))]
      
      [(equal? inst "+*")
       (define all (set-union t s a))
       (set! t all)
       (set! s all)
       (set! a all)]
      
      [(member inst (list "2*" "2/" "." "nop"))
       void]
        
      [(member inst (list "-" "+" "and" "or"))
       (push! (set-union (pop!) (pop!)))]
      
      [(equal? inst "drop")
       (pop!)]
      
      [(equal? inst "dup")
       (push! t)]
      
      [(equal? inst "over")
       (push! s)]
      
      [(equal? inst "pop")
       (push! (r-pop!))]
      
      [(equal? inst "push")
       (r-push! (pop!))]
      
      [(equal? inst "b!")
       (set! b (pop!))]
      
      [(equal? inst "a!")
       (set! a (pop!))]
      
      [else ;number
       (push! (set i))]))
  
  index-set)
