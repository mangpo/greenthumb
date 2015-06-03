#lang racket

(require "../ast.rkt" "arm-ast.rkt"
	 "arm-machine.rkt")

(require "arm-printer.rkt" "arm-parser.rkt")

(provide arm-simulator-abstract%)

(define arm-simulator-abstract%
  (class object%
    (super-new)
    (init-field k [all-yes 0] [all-no 0])
    (public load-abstract-behavior interpret-inst reset-cache)


    (define machine (new arm-machine%))
    (send machine set-config (list 5 0 4)) ;; TODO
    (define nregs (send machine get-nregs))

    (define bit (get-field bit machine))
    (define inst-id (get-field inst-id machine))
    (define shf-inst-id (get-field shf-inst-id machine))
    (define fp (send machine get-fp))
    (define mask (sub1 (arithmetic-shift 1 k)))


    (define behavior-mod (make-hash))
    (define behavior-high (make-hash))

    (define (load-abstract-behavior)
      (define (inner type behavior)
	(define i (open-input-file (format "abstract_~a_k~a.csv" type k)))
	(define current-mapping (make-hash))
	(define key-str #f)
	
	(define (loop line)
	  (when 
	   (string? line)
	   (define tokens (string-split line ","))
	   (define prog-str (first tokens))
	   (define in (map string->number (string-split (second tokens) " ")))
	   (define out-list 
	     (if (equal? (third tokens) "#t")
		 #t
                 (for/list ([out-str (string-split (third tokens) ";")])
                           (map string->number (string-split out-str " ")))))

	   (unless (equal? prog-str key-str)
		   (when key-str
                         (when (= 0 (count list? (hash-values current-mapping)))
                               (set! current-mapping #t))
			 (hash-set! behavior 
                                    (map string->number (string-split key-str " "))
				    current-mapping))
		   (set! key-str prog-str)
		   (set! current-mapping (make-hash)))
	   (hash-set! current-mapping in out-list)
	   (loop (read-line i))))
	
	(loop (read-line i))
	(close-input-port i)

        (when (= 0 (count list? (hash-values current-mapping)))
              (set! current-mapping #t))

	(hash-set! behavior (map string->number (string-split key-str " ")) 
                   current-mapping)
	(pretty-display `(summary ,(hash-count behavior))))

      (inner `mod behavior-mod)
      (inner `high behavior-high)
      )

    (define (get-inst-in-out x)
      (define opcode (inst-op x))
      (define args (inst-args x))
      (define shfop (inst-shfop x))
      (define shfarg (inst-shfarg x))
      (unless shfop (set! shfop 0))

      (define inst-type (list shfop opcode))
      (define in (list))
      (define out (list))
      (when (> shfop 0)
	    (if (member (vector-ref shf-inst-id shfop) '(asr lsl lsr))
		(set! in (cons shfarg in))
		(set! inst-type (cons shfarg inst-type))))
      
      (for ([arg args]
	    [type (send machine get-arg-types (vector-ref inst-id opcode))])
	   (cond
	    [(equal? type `reg-o) (set! out (cons arg out))]
	    [(equal? type `reg-i) (set! in (cons arg in))]
	    [(equal? type `reg-io) (set! in (cons arg in)) (set! out (cons arg out))]
	    [else (set! inst-type (cons arg inst-type))]))

      (values (reverse inst-type) (list->vector (reverse in)) (list->vector (reverse out))))

    (define cache-mod (make-vector 3))
    (define cache-high (make-vector 3))
    (define cache-inst #f)

    (define (reset-cache)
      (for ([i 3])
           (vector-set! cache-mod i (make-hash))
           (vector-set! cache-high i (make-hash))))

    (define (normalize regs)
      (define index 0)
      (define use (make-vector nregs #f))
      (for/list ([r regs])
        (let ([x (vector-ref use r)])
          (if x
              x
              (let ([my-index index])
                (vector-set! use r my-index)
                (set! index (add1 index))
                my-index)))))

    (define (interpret-inst my-inst state type [abst-k k]) ;; TODO ???
      ;; TODO z
      (define behavior (if (equal? type `mod) behavior-mod behavior-high))
      (define my-cache (if (equal? type `mod)
                           (vector-ref cache-mod (- abst-k 1))
                           (vector-ref cache-high (- abst-k 1))))
      (define opcode-name (vector-ref inst-id (inst-op my-inst)))
      (define cond-type (arm-inst-cond my-inst))

      (define regs (progstate-regs state))
      (define z (progstate-z state))
      (define fp (progstate-fp state))
      (define mul 
	(if (equal? type `mod)
	    (arithmetic-shift 1 abst-k)
	    (arithmetic-shift 1 (- bit k))))
      (struct ref (x))

      (define (exec)
	(define-values (x regs-in regs-out) (get-inst-in-out my-inst))
        (define mapping (hash-ref behavior x))
        
        (define (do-it key)
          ;(pretty-display "[DO-IT]")
          (define regs-in-val 
            (for/list ([index (vector-length regs-in)]
                       [r regs-in]) 
                      (let ([index-1 (vector-member r regs-in)]
                            [val (vector-ref regs r)])
                        (if (= index index-1)
                            (for/list ([i (arithmetic-shift 1 (- k abst-k))])
                                      (+ val (* i mul)))
                            (ref index-1)))))
          

          (define ret (list))
          (define visit (list))

          (define (inner regs-in-val)
            ;(pretty-display `(input ,x ,regs-in-val))
            (define regs-out-val-list (hash-ref mapping regs-in-val))
            (if (equal? regs-out-val-list #t)
                (set! ret #t)
                (for ([regs-out-val regs-out-val-list])
                     (unless (member regs-out-val visit)
                             (let ([new-regs (vector-copy (progstate-regs state))]
                                   [new-memory (vector-copy (progstate-memory state))])
                               (for ([r regs-out]
                                     [v regs-out-val])
                                    (vector-set! new-regs r v))
                               (set! ret (cons (progstate new-regs new-memory z fp) ret))
                               (set! visit (cons regs-out-val visit))
                               )))))

          (define (recurse lst res)
            (cond
             [(empty? lst)
              (let ([res-vec (list->vector res)])
                (inner (map (lambda (x) (if (ref? x) (vector-ref res-vec (ref-x x)) x))
                            res)))]
             [(list? (car lst))
              (for ([x (car lst)] #:break (equal? ret #t))
                   (recurse (cdr lst) (cons x res)))
              ]
             [else (recurse (cdr lst) (cons (car lst) res))]
             ))

          (recurse (reverse regs-in-val) (list))

          (if (list? ret)
              (hash-set! my-cache key visit)
              (hash-set! my-cache key ret))
          ret)

        (cond
         [(equal? mapping #t) 
          ;;(pretty-display "ALL-TRUE")
          #t]

         [else

          (define key1 (for/list ([r regs-in]) (vector-ref regs r)))
          (define key2 (normalize regs-in))
          (define key (cons key1 key2))
          ;;(pretty-display `(key ,key))
          (if (hash-has-key? my-cache key)
              (let ([regs-out-val-list (hash-ref my-cache key)])
                ;;(pretty-display "[CACHE]")
                (if (list? regs-out-val-list)
                    (for/list ([regs-out-val regs-out-val-list])
                              (let ([new-regs (vector-copy (progstate-regs state))]
                                    [new-memory (vector-copy (progstate-memory state))])
                                (for ([r regs-out]
                                      [v regs-out-val])
                                     (vector-set! new-regs r v))
                                (progstate new-regs new-memory z fp)))
                    regs-out-val-list))
              (do-it key))
          ])
        )


      (define (same)
	(list
	 (progstate (vector-copy (progstate-regs state))
		    (vector-copy (progstate-memory state))
		    z fp)))

        (cond
         [(or (equal? z -1) (equal? cond-type 0) 
	      (member opcode-name '(tst cmp tst# cmp#)))
          (exec)]

	 [(equal? cond-type 1) ;; eq
	  (if (equal? z 0) (exec) (same))]

	 [(equal? cond-type 2) ;; ne
	  (if (member z (list 1 2 3)) (exec) (same))]

	 [(equal? cond-type 3) ;; ls
	  (if (member z (list 0 2)) (exec) (same))]

	 [(equal? cond-type 4) ;; hi
	  (if (equal? z 3) (exec) (same))]

	 [(equal? cond-type 5) ;; cc
	  (if (equal? z 2) (exec) (same))]

	 [(equal? cond-type 6) ;; cs
	  (if (member z (list 0 3)) (exec) (same))]
	 
         [else (raise (format "illegal cond-type ~a" cond-type))]
         ))
      
    ))

#|
(define abst (new arm-simulator-abstract% [k 3]))
(send abst load-abstract-behavior)
(send abst reset-cache)

(define machine (new arm-machine%))
(send machine set-config (list  6 0 4))
(define printer (new arm-printer% [machine machine]))
(define parser (new arm-parser%))
(define my-inst 
  (vector-ref (send printer encode 
                    (send parser ast-from-string "sub r5, r4, r3, lsr r2"))
              0))

;; (send abst gen-abstract-behavior my-inst)
;; (send abst test)

(define t0 (current-seconds))
(define t1 (current-seconds))
(pretty-display `(time ,(- t1 t0)))
(define input-state (progstate (vector 0 0 1 2 3 0)
                               (vector) -1 4))
(define output-states
  (send abst interpret-inst my-inst input-state `mod 3))
(pretty-display output-states)
(when (list? output-states)
      (for ([output-state output-states])
	   (send machine display-state output-state)))

(pretty-display "---------------")
(define output-states2
  (send abst interpret-inst my-inst input-state `mod 3))
(pretty-display output-states2)
(when (list? output-states2)
      (for ([output-state output-states2])
	   (send machine display-state output-state)))|#
