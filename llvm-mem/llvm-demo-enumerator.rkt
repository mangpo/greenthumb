#lang racket

(require "../machine.rkt" "../enumerator.rkt" "../inst.rkt")
(require racket/generator)

(provide llvm-demo-enumerator%)

(define llvm-demo-enumerator%
  (class enumerator%
    (super-new)
    (init-field machine printer)
    (override generate-inst)
    
    (define opcodes (get-field opcodes machine))

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
    ;; In mode `basic, yield all combinations of
    ;; (inst add#-id #({1 or 2} {0 or 1} {0, 1, or -1}))
    ;;
    ;; In mode `no-args, live-in and live-out don't matter.
    ;; (inst add#-id #(2 0 {0, 1, or -1}))
    ;; Only enumerate const and not variables, but make sure to assign
    ;; different IDs for the variables.
    (define (generate-inst live-in live-out flag-in flag-out
                           #:no-args [no-args #f] #:try-cmp [try-cmp #f])

      (define mode (cond [no-args `no-args] [else `basic]))

      (define opcode-pool (get-field opcode-pool machine))
      ;; (define inst-choice '(and#))
      ;; (define opcode-pool (map (lambda (x) (vector-member x opcodes)) inst-choice))

      (define iterator
        (generator 
         ()
         (define (finalize args)
           (define out 1)
           (define in -1)
           (for/list
            ([arg args])
            ;; If arg is `var-i or `var-o (mode=`no-args), assign fresh ID.
            (cond
             [(equal? arg `var-i) (set! in (add1 in)) in]
             [(equal? arg `var-o) (set! out (add1 out)) out]
             [else arg])))
         
         (define (enumerate opcode opcode-id ranges)
           ;; Get all combinations of args
           (for ([args (all-combination-list ranges)])
                (let* ([new-args (finalize args)]
                       [pass
                        ;; Symmetry reduction
                        (cond
                         [(and (= (length args) 3)
                               (member opcode commutative-1-2))
                          (<= (second new-args) (third new-args))]
                         [else #t])])
                  (when
                   pass
                   (let* ([i (inst opcode-id (list->vector new-args))]
                          [new-live-in
                           (and live-in (send machine update-live live-in i))]
                          [new-live-out
                           (and live-out (send machine update-live-backward live-out i))])              
                     (yield
                      (list i new-live-in new-live-out)))))))
       
         (for ([opcode-id (shuffle opcode-pool)])
           (let ([opcode-name (vector-ref opcodes opcode-id)])
             (unless 
	      (equal? opcode-name `nop)
	      (let* ([arg-ranges 
		      (vector->list 
		       (send machine get-arg-ranges opcode-name #f live-in 
			     #:live-out live-out #:mode mode))])
                (enumerate opcode-name opcode-id arg-ranges)
                ))))
         (yield (list #f #f #f))))
      iterator)

    ))
