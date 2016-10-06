#lang racket

(require "../machine.rkt" "../enumerator.rkt" "arm-machine.rkt")
(require racket/generator)

(provide arm-enumerator%)

(define arm-enumerator%
  (class enumerator%
    (super-new)
    (inherit-field machine)
    
    (define cmp-inst (get-field cmp-inst machine))

    (define/override (get-flag state-vec) (progstate-z state-vec))
    
    (define/override (filter-with-flags opcode-pool flag-in flag-out
                                        #:no-args [no-args #f] #:try-cmp [try-cmp #f])
      (define ret
        (cond
         [try-cmp
          (cond
           ;; flags are different, need to insert cmp instructions.
           [(and (number? flag-in) (list? flag-out)
                 (not (member flag-in flag-out)))
            (filter (lambda (ops-vec) (member (vector-ref ops-vec 0) cmp-inst)) opcode-pool)]

           ;; no conditional flag, don't use conditional opcodes.
           [(equal? flag-in -1)
            (filter (lambda (ops-vec) (= (vector-ref ops-vec 1) -1)) opcode-pool)]

           ;; no restriction.
           [else opcode-pool]
           )
          ]

         [else
          ;; don't use cmp instructions and conditional opcodes.
          (filter (lambda (ops-vec)
                    (and (not (member (vector-ref ops-vec 0) cmp-inst))
                         (= (vector-ref ops-vec 1) -1)))
                  opcode-pool)
          ]))

      (if no-args
          ;; don't enumerate conditional opcodes
          (filter (lambda (ops-vec) (= -1 (vector-ref ops-vec 1))) ret)
          ret)
      )
    
    
    ;; (define opcodes (get-field opcodes machine))
    ;; (define inst-with-shf (get-field inst-with-shf machine))
    ;; (define cond-type-len (vector-length (get-field cond-opcodes machine)))
    ;; (define shf-opcodes (get-field shf-opcodes machine))
    ;; (define shf-inst-len (vector-length shf-opcodes))
    ;; (define cmp-inst
    ;;   (map (lambda (x) (vector-member x opcodes)) '(cmp tst cmp# tst#)))
    ;; (define ldst-inst
    ;;   (map (lambda (x) (vector-member x opcodes)) '(ldr# str#)))

    ;; (define commutative-0-1 '(tst))
    ;; (define commutative-1-2 '(add and orr eor mul smmul))
    ;; (define commutative-2-3 '(mla smull umull smmla))

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
    ;; (define (generate-inst 
    ;;          live-in live-out flag-in flag-out
    ;;          #:no-args [no-args #f] #:try-cmp [try-cmp #f])

    ;;   (define mode (cond [no-args `no-args] [else `basic]))
    ;;   ;; (define inst-choice '(add and))
    ;;   ;; (define opcode-pool (map (lambda (x) (vector-member x opcodes)) inst-choice))
    ;;   (define opcode-pool (get-field opcode-pool machine))
    ;;   (define z
    ;;     (cond
    ;;      [try-cmp
    ;;       (or flag-in
    ;;           (and flag-out (findf (lambda (x) (not (= x -1))) flag-out))
    ;;           -1)]
    ;;      [else -1]))

    ;;   ;; (pretty-display `(enumerate ,flag-in ,flag-out ,z))

    ;;   ;; Remove some opcode from opcode-pool
    ;;   (cond
    ;;    [try-cmp 
    ;;     (when (and (number? flag-in) (list? flag-out)
    ;;                (not (member flag-in flag-out)))
    ;;           (set! opcode-pool (filter (lambda (x) (member x cmp-inst)) opcode-pool)))]
    ;;    [no-args
    ;;     (set! opcode-pool (remove* (append ldst-inst cmp-inst) opcode-pool))]
    ;;    [else
    ;;     (set! opcode-pool (remove* cmp-inst opcode-pool))])
	
    ;;   (define iterator
    ;;     (generator 
    ;;      ()
         
    ;;      (define arg-types #f)
    ;;      (define global-out
    ;;        (if (and live-in live-out)
    ;;            (set-subtract (car live-out) (car live-in))
    ;;            (list)))
    ;;      (define global-in
    ;;        (if (and live-in live-out (not (empty? (car live-in)) ))
    ;;            (set-subtract (take (car live-in) 1) (car live-out))
    ;;            (list)))
         
    ;;      (define (recurse-args opcode opcode-id shfop shfarg cond-type args ranges v-reg)
    ;;        (define (check-yield)  
    ;;          (define new-args (reverse args))
    ;;          (define pass #t)
    ;;          (define out global-out)
    ;;          (define in global-in)
             
    ;;          (unless (empty? out)
    ;;            (for ([r new-args]
    ;;                  [type arg-types])
    ;;              (when (equal? type `reg-o) (set! out (remove r out))))
    ;;            (unless (empty? out)
    ;;              (set! pass #f)))
    ;;          ;; (when (and pass (not (empty? in)))
    ;;          ;;       (for ([r new-args]
    ;;          ;; 	     [type arg-types])
    ;;          ;; 	    (when (equal? type `reg-i) (set! in (remove r in))))
    ;;          ;;       (unless (empty? in)
    ;;          ;; 	       (set! pass #f)))
             
    ;;          (when
    ;;              pass
    ;;            (let* ([i (arm-inst opcode-id (list->vector new-args) 
    ;;                                shfop shfarg cond-type)]
    ;;     	      [new-live-in (and live-in (send machine update-live live-in i))]
    ;;     	      [new-live-out (and live-out (send machine update-live-backward live-out i))]
    ;;                   [ret (list i 
    ;;                              (and live-in (cons (sort (car new-live-in) <)
    ;;     					    (sort (cdr new-live-in) <)))
    ;;                              (and live-out (cons (sort (car new-live-out) <)
    ;;     					     (sort (cdr new-live-out) <)))
    ;;                              )])
    ;;              (yield ret))))
           
    ;;        (when debug (pretty-display `(recurse-args ,args ,ranges ,shfop ,v-reg)))
    ;;        ;; Symmetry reduction for commutative operations
    ;;        (define pass
    ;;          (cond
    ;;            [(and (= (length args) 2) (member opcode commutative-0-1) (= shfop 0))
    ;;             (<= (second args) (first args))]
    ;;            [(and (= (length args) 3) (member opcode commutative-1-2) (= shfop 0))
    ;;             (<= (second args) (first args))]
    ;;            [(and (= (length args) 4) (member opcode commutative-2-3) (= shfop 0))
    ;;             (<= (second args) (first args))]
    ;;            [else #t]))
    ;;        (when
    ;;            pass
    ;;          (cond
    ;;            [(empty? ranges) (check-yield)]
               
    ;;            [(equal? (car ranges) `reg-o)
    ;;             ;; enumerate no-arg
    ;;             (recurse-args opcode opcode-id shfop shfarg cond-type 
    ;;                           (cons (cdr v-reg) args)
    ;;                           (cdr ranges) (cons (car v-reg) (add1 (cdr v-reg))))
    ;;             ]
               
    ;;            [(equal? (car ranges) `reg-i)
    ;;             ;; enumerate no-arg
    ;;             (recurse-args opcode opcode-id shfop shfarg cond-type 
    ;;                           (cons (car v-reg) args)
    ;;                           (cdr ranges) (cons (add1 (car v-reg)) (cdr v-reg)))
    ;;             ]
               
    ;;            [else
    ;;             (for ([arg (shuffle (vector->list (car ranges)))])
    ;;               (recurse-args opcode opcode-id shfop shfarg cond-type 
    ;;                             (cons arg args)
    ;;                             (cdr ranges) v-reg))
    ;;             ])))
         
    ;;      (for ([opcode-id (shuffle opcode-pool)])
    ;;        (let ([opcode-name (vector-ref opcodes opcode-id)])
    ;;          (set! arg-types (send machine get-arg-types opcode-name))
    ;;          (unless 
    ;;              (equal? opcode-name `nop)
    ;;            (let* ([shf? (member opcode-name inst-with-shf)]
    ;;                   [arg-ranges 
    ;;                    (vector->list 
    ;;                     (send machine get-arg-ranges opcode-name #f live-in 
    ;;                           #:live-out live-out
    ;;                           #:mode mode))]
    ;;                   [v-reg (cond [no-args (cons 0 3)] [else #f])]
    ;;                   [cond-bound 
    ;;                    (if (or (= z -1) (member opcode-id cmp-inst))
    ;;                        1 
    ;;                        ;;(list 0 3 4) 
    ;;                        cond-type-len
    ;;                        )])
    ;;              (when debug (pretty-display `(iterate ,opcode-name ,arg-ranges ,cond-bound ,live-in)))
    ;;              (for ([cond-type cond-bound])
    ;;                (if shf?
    ;;                    (begin
    ;;                      ;; no shift
    ;;                      (recurse-args opcode-name opcode-id 0 #f cond-type 
    ;;                                    (list) arg-ranges v-reg)
    ;;                      ;; shift
    ;;                      (for ([shfop (range 1 shf-inst-len)])
    ;;                        (let ([shfarg-range 
    ;;                               (send machine get-shfarg-range shfop live-in 
    ;;                                     #:mode mode)])
    ;;                          (if (equal? shfarg-range `reg-i)
    ;;                              (recurse-args opcode-name opcode-id shfop 0 cond-type 
    ;;                                            (list) arg-ranges (cons 1 3))
    ;;                              (for ([shfarg (shuffle (vector->list shfarg-range))])
    ;;                                (recurse-args opcode-name opcode-id 
    ;;                                              shfop shfarg cond-type 
    ;;                                              (list) arg-ranges v-reg)))))
    ;;                      )
    ;;                    ;; no shift
    ;;                    (recurse-args opcode-name opcode-id 0 #f cond-type 
    ;;                                  (list) arg-ranges v-reg)
    ;;                    ))))))
    ;;      (yield (list #f #f #f))))
    ;;   iterator 
    ;;   )

    ;; ;; Return flag given progstate in vector/list/pair format.
    ;; (define (get-flag state-vec) (vector-ref state-vec 2))
    
    ))
