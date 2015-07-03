#lang racket

(require "../machine.rkt" "arm-ast.rkt")
(require racket/generator)

(provide arm-enumerative%)

(define arm-enumerative%
  (class object%
    (super-new)
    (init-field machine printer)
    (public get-flag generate-inst)
    
    (define inst-id (get-field inst-id machine))
    (define inst-with-shf (get-field inst-with-shf machine))
    (define cond-type-len (vector-length (get-field cond-inst-id machine)))
    (define shf-inst-id (get-field shf-inst-id machine))
    (define shf-inst-len (vector-length shf-inst-id))
    (define cmp-inst
      (map (lambda (x) (vector-member x inst-id))'(cmp tst cmp# tst#)))
    (define ldst-inst
      (map (lambda (x) (vector-member x inst-id))'(ldr# str#)))

    (define inst-mod '(add sub rsb
			and orr eor bic orn
                        mul uxtah
			add# sub# rsb#
			and# orr# eor# bic# orn#
			lsl#
			mov mvn
			uxth uxtb
			mov# mvn# movw# movt#
			mla mls
			bfc bfi))

    (define inst-high '(and orr eor bic orn
                         and# orr# eor# bic# orn#
                         lsr# asr#
                         mov mvn
                         mov# mvn# movw# movt#
                         bfc))

    (define commutative-0-1 '(tst))
    (define commutative-1-2 '(add and orr eor mul smmul))
    (define commutative-2-3 '(mla smull umull smmla))

    ;; If regs is not #f, use virtual registers
    ;; If lex is not #f, impose lexical order. This is only valid with virtual registers.
    (define (generate-inst 
             live-in live-out flag-in flag-out
             regs type lex #:no-args [no-args #f] #:try-cmp [try-cmp #f])

      (define mode (cond [regs `vir] [no-args `no-args] [else `basic]))
      ;; (define inst-choice '(mov#))
      ;; (define inst-pool (map (lambda (x) (vector-member x inst-id)) inst-choice))
      (define inst-pool (get-field inst-pool machine))
      (define z
        (cond
         [try-cmp
          (or flag-in
              (and flag-out (findf (lambda (x) (not (= x -1))) flag-out))
              -1)]
         [else -1]))

      ;; (pretty-display `(enumerate ,flag-in ,flag-out ,z))

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
      
      (cond
       [(equal? type `mod+high) 
	(set! inst-pool 
              (filter (lambda (x) (and (member (vector-ref inst-id x) inst-mod)
                                       (member (vector-ref inst-id x) inst-high)))
                      inst-pool))]
       [(equal? type `mod-high) 
	(set! inst-pool 
              (filter (lambda (x) (member (vector-ref inst-id x) inst-mod))
                      inst-pool))]
       [(equal? type `high-mod)
	(set! inst-pool 
              (filter (lambda (x) (member (vector-ref inst-id x) inst-high))
                      inst-pool))]
       )
	
      (define iterator
        (generator 
         ()
         
         (define arg-types #f)
         (define global-out
           (if (and live-in live-out)
               (set-subtract (car live-out) (car live-in))
               (list)))
         (define global-in
           (if (and live-in live-out (not (empty? (car live-in)) ))
               (set-subtract (take (car live-in) 1) (car live-out))
               (list)))
         
         (define (recurse-args opcode opcode-id shfop shfarg cond-type args ranges v-reg)
           (define (check-yield)  
             (define new-args (reverse args))
             (define pass #t)
             (define out global-out)
             (define in global-in)
             
             (unless (empty? out)
               (for ([r new-args]
                     [type arg-types])
                 (when (equal? type `reg-o) (set! out (remove r out))))
               (unless (empty? out)
                 (set! pass #f)))
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
                      [ret (list i 
                                 (and live-in (send machine update-live live-in i))
                                 (and live-out (send machine update-live-backward live-out i))
                                 v-reg)])
                 (yield ret))))
           ;; (if lex
           ;;     (let ([my-lex (lexical-skeleton i)])
           ;;       (if my-lex
           ;;           (when (>= (lexical-cmp my-lex lex) 0)
           ;;                 (yield ret))
           ;;           (yield ret)))
           ;;     (yield ret))))
           
           
           (when debug (pretty-display `(recurse-args ,args ,ranges ,shfop ,v-reg)))
           ;; Symmetry reduction for commutative operations
           (define pass
             (cond
               [(and (= (length args) 2) (member opcode commutative-0-1) (= shfop 0))
                (<= (second args) (first args))]
               [(and (= (length args) 3) (member opcode commutative-1-2) (= shfop 0))
                (<= (second args) (first args))]
               [(and (= (length args) 4) (member opcode commutative-2-3) (= shfop 0))
                (<= (second args) (first args))]
               [else #t]))
           (when
               pass
             (cond
               [(empty? ranges)
                
                (cond
                  [(equal? type `all) (check-yield)]
                  
                  [(equal? (vector-ref shf-inst-id shfop) `nop)
                   (cond
                     [(equal? type `mod+high) (check-yield)]
                     [(equal? type `mod-high)
                      (when (not (member (vector-ref inst-id opcode-id) inst-high)) 
                        (check-yield))]
                     [(equal? type `high-mod)
                      (when (not (member (vector-ref inst-id opcode-id) inst-mod)) 
                        (check-yield))]
                     [(equal? type `rest)
                      (when (and (not (member (vector-ref inst-id opcode-id) inst-mod))
                                 (not (member (vector-ref inst-id opcode-id) inst-high)))
                        (check-yield))]
                     )]
                  
                  [(equal? (vector-ref shf-inst-id shfop) `lsl#)
                   (cond
                     [(equal? type `mod-high) (check-yield)]
                     [(equal? type `rest)
                      (when (not (member (vector-ref inst-id opcode-id) inst-mod))
                        (check-yield))]
                     )]
                  
                  [(member (vector-ref shf-inst-id shfop) '(lsr# asr#))
                   (cond
                     [(equal? type `high-mod) (check-yield)]
                     [(equal? type `rest)
                      (when (not (member (vector-ref inst-id opcode-id) inst-high))
                        (check-yield))]
                     )]
                  
                  [(equal? type `rest) (check-yield)]
                  )] ;; end cond (empty? ranges)
               
               [(equal? (car ranges) `reg-o)
                (if (pair? v-reg)
                    ;; enumerate no-arg
                    (recurse-args opcode opcode-id shfop shfarg cond-type 
                                  (cons (cdr v-reg) args)
                                  (cdr ranges) (cons (car v-reg) (add1 (cdr v-reg))))
                    ;; enumerate virtual
                    (recurse-args opcode opcode-id shfop shfarg cond-type 
                                  (cons v-reg args)
                                  (cdr ranges) (add1 v-reg)))
                ]
               
               [(equal? (car ranges) `reg-i)
                ;; enumerate no-arg
                (recurse-args opcode opcode-id shfop shfarg cond-type 
                              (cons (car v-reg) args)
                              (cdr ranges) (cons (add1 (car v-reg)) (cdr v-reg)))
                ]
               
               [else
                (for ([arg (car ranges)])
                  (recurse-args opcode opcode-id shfop shfarg cond-type 
                                (cons arg args)
                                (cdr ranges) v-reg))
                ])))
         
         (for ([opcode-id inst-pool])
           (let ([opcode-name (vector-ref inst-id opcode-id)])
             (set! arg-types (send machine get-arg-types opcode-name))
             (unless 
                 (equal? opcode-name `nop)
               (let* ([shf? (member opcode-name inst-with-shf)]
                      [arg-ranges 
                       (vector->list 
                        (send machine get-arg-ranges opcode-name #f live-in 
                              #:live-out live-out
                              #:mode mode))]
                      [v-reg (cond [regs regs] [no-args (cons 0 3)] [else #f])]
                      [cond-bound 
                       (if (or (= z -1) regs (member opcode-id cmp-inst))
                           1 
                           ;;(list 0 3 4) 
                           cond-type-len
                           )])
                 (when debug (pretty-display `(iterate ,opcode-name ,arg-ranges ,cond-bound ,live-in)))
                 (for ([cond-type cond-bound])
                   (if shf?
                       (begin
                         ;; no shift
                         (recurse-args opcode-name opcode-id 0 #f cond-type 
                                       (list) arg-ranges v-reg)
                         ;; shift
                         (for ([shfop (range 1 shf-inst-len)])
                           (let ([shfarg-range 
                                  (send machine get-shfarg-range shfop live-in 
                                        #:mode mode)])
                             (if (equal? shfarg-range `reg-i)
                                 (recurse-args opcode-name opcode-id shfop 0 cond-type 
                                               (list) arg-ranges (cons 1 3))
                                 (for ([shfarg shfarg-range])
                                   (recurse-args opcode-name opcode-id 
                                                 shfop shfarg cond-type 
                                                 (list) arg-ranges v-reg)))))
                         )
                       ;; no shift
                       (recurse-args opcode-name opcode-id 0 #f cond-type 
                                     (list) arg-ranges v-reg)
                       ))))))
         (yield (list #f #f #f #f))))
      iterator 
      )

    (define (get-flag state-vec) (vector-ref state-vec 2))
    
    ))
