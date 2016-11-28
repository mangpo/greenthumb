#lang racket

(require "inst.rkt")
(require racket/generator)
(provide enumerator% all-combination-list all-combination-gen)


;; Return a list of all possible combinations of flat lists
(define (all-combination-list list-of-list)
  (define ret (list))
  (define (recurse in-list out-list)
    (cond
     [(empty? in-list) (set! ret (cons out-list ret))]
     [else
      (let ([rest (cdr in-list)]
            [x (car in-list)])
        (if (or (vector? x) (list? x))
            (for ([val x])
                 (recurse rest (cons val out-list)))
            (recurse rest (cons x out-list))))
      ]))

  (recurse (reverse list-of-list) (list))
  ret)

;; Return a generator of all possible combinations of flat lists
(define (all-combination-gen list-of-list)
  (define iterator
    (generator 
     ()
     (define (recurse in-list out-list)
       (cond
        [(empty? in-list) (yield out-list)]
        [else
         (let ([rest (cdr in-list)]
               [x (car in-list)])
           (if (or (vector? x) (list? x))
               (for ([val x])
                    (recurse rest (cons val out-list)))
               (recurse rest (cons x out-list))))
         ]))
     (recurse (reverse list-of-list) (list))
     (yield #f)
     ))

  iterator)


(define enumerator%
  (class object%
    (super-new)
    (init-field machine printer)
    (public generate-inst get-pruning-info filter-with-pruning-info)

    (define (get-pruning-info state) #f)
    
    (define opcodes (get-field opcodes machine))

    (define (filter-with-pruning-info opcodes prune-in prune-out
                                      #:no-args [no-args #f] #:try-cmp [try-cmp #f])
      opcodes)
    
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
    (define (generate-inst live-in live-out prune-in prune-out
                           #:no-args [no-args #f] #:try-cmp [try-cmp #f]
                           #:step-fw [step-fw #f] #:step-bw [step-bw #f])

      (define mode (cond [no-args `no-args] [else `basic]))

      (define opcode-pool
        (cond
         [(and step-fw step-bw)
          ;;(pretty-display `(gen-pos ,step-fw ,(+ step-fw step-bw 1)))
          (send machine get-valid-opcode-pool step-fw (+ step-fw step-bw 1) live-in)]
         [else (get-field opcode-pool machine)]))
      ;; (define inst-choice '(! @+ a!))
      ;; (define opcode-pool (map (lambda (x) (send machine get-opcode-id x)) inst-choice))

      (set! opcode-pool (filter-with-pruning-info opcode-pool prune-in prune-out
                                                  #:no-args no-args
                                                  #:try-cmp try-cmp))
      
      ;; (pretty-display `(generate-inst ,(length opcode-pool) ,(length (get-field opcode-pool machine))))
      ;; (pretty-display (map (lambda (x) (send machine get-opcode-name x)) opcode-pool))

      (define iterator
        (generator 
         ()
         (define (finalize args)
           (define in -1)
           (for/list
            ([arg args])
            ;; If arg is `var, assign fresh ID.
            (cond
             [(symbol? arg) (set! in (add1 in)) in]
             [else arg])))
         
         (define (enumerate opcode-id ranges)
           ;; Get all combinations of args
           (for ([args (all-combination-list ranges)])
                (let* ([new-args (finalize args)]
                       [pass (send machine is-cannonical opcode-id new-args)])
                  (when
                   pass
                   (let* ([my-inst (inst opcode-id (list->vector new-args))]
                          [new-live-in
                           (and live-in (send machine update-live live-in my-inst))]
                          [new-live-out
                           (and live-out (send machine update-live-backward live-out my-inst))])
                     (yield (list my-inst new-live-in new-live-out)))))))
       
         (for ([opcode-id (shuffle opcode-pool)])
              (unless 
               (equal? opcode-id (get-field nop-id machine))
               (let* ([arg-ranges 
                       (send machine get-arg-ranges opcode-id #f live-in 
                             #:live-out live-out #:mode mode)])
                 (when arg-ranges
                       (enumerate opcode-id (vector->list arg-ranges)))
                 )))
         (yield (list #f #f #f))))
      iterator)

    ))
