#lang racket

(require "../machine.rkt" "../enumerative.rkt" "arm-ast.rkt")
(require racket/generator)

(provide arm-enumerative%)

(define arm-enumerative%
  (class enumerative%
    (super-new)
    (init-field machine printer)
    (override get-flag generate-inst)
    
    (define inst-id (get-field inst-id machine))
    (define inst-with-shf (get-field inst-with-shf machine))
    (define cond-type-len (vector-length (get-field cond-inst-id machine)))
    (define shf-inst-id (get-field shf-inst-id machine))
    (define shf-inst-len (vector-length shf-inst-id))
    (define cmp-inst
      (map (lambda (x) (vector-member x inst-id)) '(cmp tst cmp# tst#)))
    (define ldst-inst
      (map (lambda (x) (vector-member x inst-id)) '(ldr# str#)))

    (define commutative-0-1 '(tst))
    (define commutative-1-2 '(add and orr eor mul smmul))
    (define commutative-2-3 '(mla smull umull smmla))

    (define nregs (send machine get-nregs))
    (define stack 4)

    ;; Return generator that enumerates all instructions given live-in, live-out, flag-in, and flag-out information
    ;; 
    ;; >>> INPUTS
    ;; live-in & live-out: compact format
    ;; flag-in: conditional flag
    ;; flag-out: a list of conditional flags
    ;; Different usages:
    ;;   1. When building forward search graph, only live-in and flag-in are given.
    ;;   2. When buliding backward search graph, only live-out and flag-out are given.
    ;;   3. When searching for the briding instruction, all live-in, live-out, flag-in, flag-out are given.
    ;; no-args: no-args = #t indicates enumerating instructions for creating inverse table.
    ;;          Thus, no need to enumerate all possible combination of operands.
    ;;          Only inverse%:gen-inverse-behaviors use generator created with no-args = #t.
    ;;          Developer needs to implement gen-inverse-behaviors.
    ;; try-cmp: include comparing instructions in the instruction pool.
    ;;
    ;; >>> OUTPUT
    ;; A generator that yield a list of 
    ;;  (a. inst, 
    ;;   b. liveness after executing inst given live-in,
    ;;   c. liveness before executing inst given live-out)
    ;; If live-in = #f, b is #f.
    ;; If live-out = #f, c is #f.
    (define (generate-inst 
             live-in live-out flag-in flag-out
             #:no-args [no-args #f] #:try-cmp [try-cmp #f])

      (define mode (cond [no-args `no-args] [else `stack]))
      ;; (define inst-choice '(add and))
      ;; (define inst-pool (map (lambda (x) (vector-member x inst-id)) inst-choice))
      (define inst-pool (get-field inst-pool machine))
      (define z
        (cond
         [try-cmp
          (or flag-in
              (and flag-out (findf (lambda (x) (not (= x -1))) flag-out))
              -1)]
         [else -1]))

      ;; Remove some opcode from inst-pool
      (cond
       [try-cmp 
        (when (and (number? flag-in) (list? flag-out)
                   (not (member flag-in flag-out)))
              (set! inst-pool (filter (lambda (x) (member x cmp-inst)) inst-pool)))]
       [no-args
        (set! inst-pool (remove* (append ldst-inst cmp-inst) inst-pool))]
       [else
        (set! inst-pool (remove* cmp-inst inst-pool))])
	
      (define iterator
        (generator 
         ()
         (define arg-types #f)
         ;; (define global-out
         ;;   (if (and live-in live-out)
         ;;       (set-subtract (car live-out) (car live-in))
         ;;       (list)))
         ;; (define global-in
         ;;   (if (and live-in live-out (not (empty? (car live-in)) ))
         ;;       (set-subtract (take (car live-in) 1) (car live-out))
         ;;       (list)))
         
         (define (recurse-args opcode opcode-id shfop shfarg cond-type args ranges v-reg)
           (define (check-yield)  
             (define new-args (reverse args))
             (define pass #t)
             ;; (define out global-out)
             ;; (define in global-in)
             
             ;; (unless (empty? out)
             ;;   (for ([r new-args]
             ;;         [type arg-types])
             ;;     (when (equal? type `reg-o) (set! out (remove r out))))
             ;;   (unless (empty? out)
             ;;     (set! pass #f)))
             ;; (when (and pass (not (empty? in)))
             ;;       (for ([r new-args]
             ;; 	     [type arg-types])
             ;; 	    (when (equal? type `reg-i) (set! in (remove r in))))
             ;;       (unless (empty? in)
             ;; 	       (set! pass #f)))
             
             (when
                 pass
               (let* ([i (arm-inst opcode-id (list->vector new-args) 
                                   shfop shfarg cond-type)]
		      [new-live-in (and live-in (send machine update-live live-in i #:mode mode))]
		      [new-live-out (and live-out (send machine update-live-backward live-out i #:mode mode))]
                      [pass2 (and (or (not new-live-in)
                                     (and (>= (third new-live-in) 0)
                                          (<= (third new-live-in) stack)))
                                 (or (not new-live-out)
                                     (and (>= (third new-live-out) 0)
                                          (<= (third new-live-out) stack))))])
                 (when #f ;;new-live-in
                       (pretty-display `(yield ,pass2
                                               ,live-out ,new-live-out
                                               ,(third new-live-in))))
                 (when pass2
                       (let ([ret
                              (list
                               i 
                               (and live-in (list (sort (first new-live-in) <)
                                                  (sort (second new-live-in) <)
                                                  (third new-live-in)))
                               (and live-out (list (sort (first new-live-out) <)
                                                   (sort (second new-live-out) <)
                                                   (third new-live-out)))
                               )])
                         (yield ret))))))
           
           (when debug (pretty-display `(recurse-args ,args ,ranges ,shfop ,v-reg)))
           ;; Symmetry reduction for commutative operations
           ;; (define pass
           ;;   (cond
           ;;     [(and (= (length args) 2) (member opcode commutative-0-1) (= shfop 0))
           ;;      (<= (second args) (first args))]
           ;;     [(and (= (length args) 3) (member opcode commutative-1-2) (= shfop 0))
           ;;      (<= (second args) (first args))]
           ;;     [(and (= (length args) 4) (member opcode commutative-2-3) (= shfop 0))
           ;;      (<= (second args) (first args))]
           ;;     [else #t]))
           (define pass #t)
           (when
              pass
             (cond
               [(empty? ranges) (check-yield)]
               
               [(equal? (car ranges) `reg-o)
                ;; enumerate no-arg
                (recurse-args opcode opcode-id shfop shfarg cond-type 
                              (cons (cdr v-reg) args)
                              (cdr ranges) (cons (car v-reg) (add1 (cdr v-reg))))
                ]
               
               [(equal? (car ranges) `reg-i)
                ;; enumerate no-arg
                (recurse-args opcode opcode-id shfop shfarg cond-type 
                              (cons (car v-reg) args)
                              (cdr ranges) (cons (add1 (car v-reg)) (cdr v-reg)))
                ]
               
               [else
                (for ([arg (shuffle (vector->list (car ranges)))])
                  (recurse-args opcode opcode-id shfop shfarg cond-type 
                                (cons arg args)
                                (cdr ranges) v-reg))
                ])))

         ;; For load/store intputs/outputs
         (when (equal? mode `stack)
               (let* ([opcode-id (vector-member `mov inst-id)]
                      [opcode-name (vector-ref inst-id opcode-id)]
                      [arg-ranges 
                       (send machine get-arg-ranges opcode-name
                             #f live-in 
                             #:live-out live-out
                             #:mode `basic)]
                      [to (vector-ref arg-ranges 0)]
                      [from (vector-ref arg-ranges 1)])
                 ;; load input
                 ;;(pretty-display "load input ...")
                 (if live-in
                     (when (< (third live-in) stack)
                           (recurse-args opcode-name opcode-id 0 #f 0 
                                         (list)
                                         (list (vector (+ (- nregs stack) (third live-in))) from)
                                         #f))
                     (when (> (third live-out) 0)
                           (recurse-args opcode-name opcode-id 0 #f 0 
                                         (list)
                                         (list (vector (+ (- nregs stack) (sub1 (third live-out)))) from)
                                         #f)))
                 ;; store output
                 ;;(pretty-display "store output ...")
                 (if live-in
                     (when (> (third live-in) 0)
                           (recurse-args opcode-name opcode-id 0 #f 0 
                                         (list)
                                         (list to (vector (+ (- nregs stack) (sub1 (third live-in)))))
                                         #f))
                     (when (< (third live-out) stack)
                           (recurse-args opcode-name opcode-id 0 #f 0 
                                         (list)
                                         (list to (vector (+ (- nregs stack) (third live-out))))
                                         #f)))
                 ))
         ;;(pretty-display "done ...")
           
         (for ([opcode-id (shuffle inst-pool)])
           (let ([opcode-name (vector-ref inst-id opcode-id)])
             (set! arg-types (send machine get-arg-types opcode-name))
             (unless 
                 (equal? opcode-name `nop)
               (let* ([shf? (member opcode-name inst-with-shf)]
                      [v-reg (cond [no-args (cons 0 3)] [else #f])]
                      [cond-bound 
                       (if (or (= z -1) (member opcode-id cmp-inst))
                           1 
                           ;;(list 0 3 4) 
                           cond-type-len
                           )])
                 (when debug (pretty-display `(iterate ,opcode-name ,cond-bound ,live-in)))
                 (for ([cond-type cond-bound])
                   (if shf?
                       (begin
                         ;; no shift
                         (let ([arg-ranges 
                                (vector->list 
                                 (send machine get-arg-ranges opcode-name
                                       #f live-in 
                                       #:live-out live-out
                                       #:mode mode))])
                           (recurse-args opcode-name opcode-id 0 #f cond-type 
                                         (list) arg-ranges v-reg))
                         ;; shift
                         (for ([shfop (range 1 shf-inst-len)])
                           (let* ([shfarg-range 
                                   (send machine get-shfarg-range shfop opcode-name live-in #:live-out live-out
                                         #:mode mode)]
                                  [arg-ranges 
                                   (vector->list 
                                    (send machine get-arg-ranges opcode-name
                                          shfop live-in 
                                          #:live-out live-out
                                          #:mode mode))])
                             (if (equal? shfarg-range `reg-i)
                                 (recurse-args opcode-name opcode-id shfop 0 cond-type 
                                               (list) arg-ranges (cons 1 3))
                                 (for ([shfarg (shuffle (vector->list shfarg-range))])
                                      (recurse-args opcode-name opcode-id 
                                                    shfop shfarg cond-type 
                                                    (list) arg-ranges v-reg)))))
                         )
                       ;; no shift
                       (let ([arg-ranges 
                              (vector->list 
                               (send machine get-arg-ranges opcode-name
                                     #f live-in 
                                     #:live-out live-out
                                     #:mode mode))])
                         (recurse-args opcode-name opcode-id 0 #f cond-type 
                                       (list) arg-ranges v-reg))
                       ))))))
         (yield (list #f #f #f #f))))
      iterator 
      )

    ;; Return flag given progstate in vector/list/pair format.
    (define (get-flag state-vec) (vector-ref state-vec 2))
    
    ))
