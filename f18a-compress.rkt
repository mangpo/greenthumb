#lang racket

(require "state.rkt" "stack.rkt" "f18a.rkt" "ast.rkt")

(provide decompress)

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

(define (decompress code-compressed info constraint assumption program-eq? 
                    #:prefix [prefix (list)])
  (define index-map (syninfo-indexmap info))
  (define mem-size (syninfo-memsize info))
  (define real-size (dict-ref index-map mem-size))

  (define (decompress-code code)
    (traverse code [block? modify-index] [forloop? modify-bound]))

  (define (decompress-constraint x)
    (define mem-small (progstate-memory x))
    (define mem-full (make-vector real-size #f))
    (define from-full 0)
    (define last-val #f)

    (for ([s (in-range mem-size)])
         (when (hash-has-key? index-map s)
               (let ([to-full (dict-ref index-map s)])
                 (for ([f (in-range from-full to-full)])
                      (vector-set! mem-full f last-val))
                 (set! from-full to-full)
                 (set! last-val (vector-ref mem-small s)))))

    (for ([f (in-range from-full real-size)])
         (vector-set! mem-full f last-val))

    ;; (pretty-display `(index-map ,index-map))
    ;; (pretty-display `(mem-small ,mem-small))
    ;; (pretty-display `(mem-full ,mem-full))

    (struct-copy progstate x [memory mem-full]))

  (define (modify-index ast)
    (define body (string-split (block-body ast)))
    (define modify-set (track-index body))
    (define new-body
      (for/list ([inst body]
                 [i (in-range (length body))])
                (if (and (set-member? modify-set i) 
                         (dict-has-key? index-map (string->number inst)))
                    (number->string (dict-ref index-map (string->number inst)))
                    inst)))
    (block (string-join new-body) (block-org ast) (block-info ast)))

  (define (modify-bound ast)
    (forloop (original (forloop-init ast)) 
             (decompress-code (forloop-body ast))
             (forloop-bound ast)))

  (define (decompress-verify)
    (pretty-display ">>> Decompress & verify")
    (define code (traverse code-compressed 
                           [block? modify-index]
                           [forloop? modify-bound]))
    (define spec (original code-compressed))
    (define org-prefix (original prefix))
    (pretty-display "decompressed-program:")
    (print-struct code)
    (pretty-display "original spec:")
    (print-struct spec)
    (if (program-eq? (encode (append org-prefix code)) 
		     (encode (append org-prefix spec))
                     (syninfo real-size
                              (syninfo-recv info)
                              (syninfo-indexmap info))
                     (decompress-constraint constraint) 
                     #:assume assumption)
        code
        "same"))
  
  (if index-map
      (decompress-verify)
      code-compressed))