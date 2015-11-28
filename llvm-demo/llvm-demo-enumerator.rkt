#lang racket

(require "../machine.rkt" "../enumerator.rkt" "../inst.rkt")
(require racket/generator)

(provide llvm-demo-enumerator%)

(define llvm-demo-enumerator%
  (class enumerator%
    (super-new)
    (init-field machine printer)
    (override generate-inst)
    
    (define inst-id (get-field inst-id machine))

    (define commutative-1-2 '(and or xor add))
    
    ;; Mode `no-args is for generating instructions to be used for creating
    ;; inverse behaviors, so we don't have to enumerate variables/registers.
    ;; 
    ;; Examples of all possible instrucions with opcode add# that
    ;; the generator has to yeild.
    ;; Assume our program state has 3 variables/registers
    ;; const-range = #(0 1 -1), live-in = #(#t #t #f), live-out = #(#f #t #t)
    ;; (If live-in = #f, then everything is live. Similar for live-out.)
    ;; 
    ;; In mode `basic,
    ;; (inst add#-id #({1 or 2} {0 or1} {0, 1, or -1}))
    ;;
    ;; In mode `no-args, live-in and live-out don't matter.
    ;; (inst add#-id #(2 0 {0, 1, or -1}))
    ;; Only enumerate const and not variables, but make sure to assign
    ;; different IDs for the variables.
    (define (generate-inst 
             live-in live-out flag-in flag-out
             #:no-args [no-args #f] #:try-cmp [try-cmp #f])

      (define mode (cond [no-args `no-args] [else `basic]))

      (define inst-pool (get-field inst-pool machine))
      ;; (define inst-choice '(and#))
      ;; (define inst-pool (map (lambda (x) (vector-member x inst-id)) inst-choice))

      (define iterator
        (generator 
         ()
         (define (recurse-args opcode opcode-id args ranges v-reg)
	   ;; check for faster search
           (define (check-yield)  
	     (let* ([new-args (reverse args)]
		    [i (inst opcode-id (list->vector new-args))]
		    [new-live-in (and live-in (send machine update-live live-in i))]
		    [new-live-out (and live-out (send machine update-live-backward live-out i))]
		    [ret (list i new-live-in new-live-out)])
               (yield ret)))
           
           (when debug (pretty-display `(recurse-args ,args ,ranges ,v-reg)))
           ;; Symmetry reduction for commutative operations
           (define pass
             (cond
               [(and (= (length args) 3) (member opcode commutative-1-2))
                (<= (second args) (first args))]
               [else #t]))

           (when
               pass
             (cond
               [(empty? ranges) (check-yield)]
               
               [(equal? (car ranges) `var-o)
                ;; enumerate no-arg
                (recurse-args opcode opcode-id 
                              (cons (cdr v-reg) args)
                              (cdr ranges) (cons (car v-reg) (add1 (cdr v-reg))))
                ]
               
               [(equal? (car ranges) `var-i)
                ;; enumerate no-arg
                (recurse-args opcode opcode-id
                              (cons (car v-reg) args)
                              (cdr ranges) (cons (add1 (car v-reg)) (cdr v-reg)))
                ]
               
               [else
                (for ([arg (shuffle (vector->list (car ranges)))])
                  (recurse-args opcode opcode-id
                                (cons arg args)
                                (cdr ranges) v-reg))
                ])))
       
         (for ([opcode-id (shuffle inst-pool)])
           (let ([opcode-name (vector-ref inst-id opcode-id)])
             (unless 
	      (equal? opcode-name `nop)
	      (let* ([arg-ranges 
		      (vector->list 
		       (send machine get-arg-ranges opcode-name #f live-in 
			     #:live-out live-out
			     #:mode mode))]
                     [arg-types (send machine get-arg-types opcode-name)]
		     [v-reg (cond [no-args (cons 0 3)] [else #f])])
		;; (cond
                ;;  [(and live-in (not live-out))
                ;;   ;; use the first variable that is not in live-in.
                ;;   ;; (let ([index (vector-member #f live-in)])
                ;;   ;;   (set! arg-ranges (cons (vector index) (cdr arg-ranges))))
                ;;   (set! arg-ranges (cons (vector-copy (car arg-ranges) 0 1)
                ;;                          (cdr arg-ranges)))
                ;;   ]
                ;;  [(and live-out (not live-in))
                ;;   (let ([cnt 0])
                ;;     (set!
                ;;      arg-ranges
                ;;      (reverse
                ;;       (for/list
                ;;        ([range (reverse arg-ranges)]
                ;;         [type (reverse (vector->list arg-types))])
                ;;        (if (equal? type `var-i)
                ;;            (let ([index (- (vector-length range) 1 cnt)])
                ;;              (set! cnt (add1 cnt))
                ;;              (vector (vector-ref range index)))
                ;;            range)))))]
                ;;  )
                 
		(when debug (pretty-display `(iterate ,opcode-name ,arg-ranges,live-in)))
                (recurse-args opcode-name opcode-id (list) arg-ranges v-reg)))))
         (yield (list #f #f #f))))
      iterator)

    ))
