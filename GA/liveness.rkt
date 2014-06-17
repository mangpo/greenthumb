#lang s-exp rosette

(require "state.rkt" "stack.rkt" "../ast.rkt")
(provide extract-liveness inline? 
         set-constraint! get-constraint
         contain union)

(define debug #f)

;; Return liveness of start
;; start    : symbolic input state
;; end      : symbolic end state
;; end-cnstr: liveness of end
(define (extract-liveness start end end-cnstr)
  (when debug
        (pretty-display "START:")
        (display-state start)
        (pretty-display "END:")
        (display-state end)
        (pretty-display "END-CNSTR:")
        (pretty-display end-cnstr))
  (define mem-len (vector-length (progstate-memory start)))
  (define start-cnstr (constraint))
  (define start-mem-cnstr (make-vector mem-len #f))
  (set-progstate-memory! start-cnstr start-mem-cnstr)

  (define-syntax-rule (check read-start set-start! target)
    (let ([x (read-start start)])
      ;; (pretty-display `(check ,x ,target))
      (when (concrete-in x target)
	    (set-start! start-cnstr #t))))

  (define-syntax-rule (check-stack progstate-stack target)
    ;; Don't change stack constraints
    (void))

  (define-syntax-rule (check-mem target)
    (let ([memory (progstate-memory start)])
      (for ([i (in-range (vector-length memory))])
           ;; (pretty-display `(check-mem ,i ,(vector-ref memory i) ,target))
           (when (concrete-in (vector-ref memory i) target)
                 (vector-set! start-mem-cnstr i #t)))))

  (define (check-entire target)
    ;; (pretty-display `(check-entire ,target))
    (check progstate-a set-progstate-a! target)
    (check progstate-b set-progstate-b! target)
    (check progstate-r set-progstate-r! target)
    (check progstate-s set-progstate-s! target)
    (check progstate-t set-progstate-t! target)
    (check-stack progstate-data target)
    (check-stack progstate-return target)
    (check-mem target))

  (define (live-reg progstate-x)
    (when (progstate-x end-cnstr)
          ;; (pretty-display `(live-reg ,progstate-x))
          (check-entire (symbolics (progstate-x end)))))

  (define (live-stack progstate-x)
    (when (progstate-x end-cnstr)
      (for ([i (in-range (progstate-x end-cnstr))])
           (check-entire (symbolics (get-stack (progstate-x end) i))))))

  (define (live-mem)
    (define end-mem-cnstr (progstate-memory end-cnstr))
    (define end-mem (progstate-memory end))
    (when end-mem-cnstr
          ;; (unless (vector? end-mem-cnstr)
          ;;         (set! end-mem-cnstr (make-vector mem-len #t)))
          (for ([i (in-range mem-len)])
               (let ([target (vector-ref end-mem-cnstr i)]
                     [val (vector-ref end-mem i)])
                 (when target (check-entire (symbolics val)))))))

  (define (live-comm)
    ;; (for ([i (progstate-comm end)])
    ;;      (check-entire (symbolics i)))
    (for/all ([o (progstate-comm end)])
	     (for ([i o])
		  (check-entire (symbolics i))))
    )
      
  (pretty-display "check live-reg")
  (live-reg progstate-a)
  (live-reg progstate-b)
  (live-reg progstate-r)
  (live-reg progstate-s)
  (live-reg progstate-t)
  (live-stack progstate-data)
  (live-stack progstate-return)
  (pretty-display "check live-mem")
  (live-mem)
  (pretty-display "check live-comm")
  (live-comm)
  (pretty-display "done check live")

  start-cnstr)

;; Return true if x contains y.
(define (contain x y)
  (define-syntax-rule (check progstate-x)
    (or (not (progstate-x y)) (progstate-x x)))

  (define-syntax-rule (check-stack progstate-x)
    #t)

  (define-syntax-rule (check-mem)
    (andmap (lambda (x y) (or (not y) x)) 
            (vector->list (progstate-memory x))
            (vector->list (progstate-memory y))))

  (and 
   (check progstate-a)
   (check progstate-b)
   ;(check progstate-r)
   ;(check progstate-s)
   ;(check progstate-t)
   ;(check-stack progstate-data)
   ;(check-stack progstate-return)
   (check-mem)))

(define (union x y)
  (define out (constraint))
  
  (define-syntax-rule (check progstate-x set-progstate-x!)
    (set-progstate-x! out (or (progstate-x x) (progstate-x y))))

  (define-syntax-rule (check-stack progstate-x)
    (void))

  (define (check-mem)
    (define memory-x (progstate-memory x))
    (define memory-y (progstate-memory y))
    (define mem-len (vector-length memory-x))
    (define memory (make-vector mem-len #f))

    (for ([i (in-range mem-len)])
         (vector-set! memory i
                      (or (vector-ref memory-x i) (vector-ref memory-y i))))
    (set-progstate-memory! out memory))

  (check progstate-a set-progstate-a!)
  (check progstate-b set-progstate-b!)
  (check progstate-r set-progstate-r!)
  (check progstate-s set-progstate-s!)
  (check progstate-t set-progstate-t!)
  (check-stack progstate-data)
  (check-stack progstate-return)
  (check-mem)
  out)

(define (set-constraint! b cnstr)
  (pretty-display `(set-constraint ,cnstr))
  (define old-cnstr (blockinfo-cnstr (block-info b)))
  (set-progstate-a! old-cnstr (progstate-a cnstr))
  (set-progstate-b! old-cnstr (progstate-b cnstr))
  (set-progstate-memory! old-cnstr (progstate-memory cnstr)))

(define (get-constraint b)
  (blockinfo-cnstr (block-info b)))

(define (inline? x)
  (or (regexp-match #rx"rep" (call-name x)) (regexp-match #rx"if" (call-name x))))
      
