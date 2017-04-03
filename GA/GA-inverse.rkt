#lang racket

(require "../ops-racket.rkt" "GA-ops-racket.rkt"
         "../inst.rkt" "../inverse.rkt"
         "GA-machine.rkt")

(provide GA-inverse%)

(define GA-inverse%
  (class inverse%
         (super-new)
    (inherit-field machine simulator)
    (override gen-inverse-behavior interpret-inst)
    
    (define bit (get-field bitwidth machine))
    (define opcodes (get-field opcodes machine))
    (define val-range
      (for/list ([v (arithmetic-shift 1 bit)]) (finitize v bit)))
    
    (define behaviors-bw (make-hash))
    
    (define UP (get-field UP machine))
    (define DOWN (get-field DOWN machine))
    (define LEFT (get-field LEFT machine))
    (define RIGHT (get-field RIGHT machine))
    (define IO (get-field IO machine))

    ;; Generate inverse function for the following instructions:
    ;;   +* 2* 2/ - + and or drop
    (define (gen-inverse-behavior my-inst)
      (define opcode-id (inst-op my-inst))
      (define opcode-name (vector-ref opcodes opcode-id))
      (define (init #:min [min #f] #:max [max #f] #:const [const #f])
        (if const const 0))

      (define behavior-bw (make-hash))
      (cond
       [(member opcode-name '(2* 2/ -))
        (for ([v val-range])
             (let ([state (send machine get-state init)])
               (set-progstate-t! state v)
               (let* ([out-state 
                       (with-handlers*
                        ([exn? (lambda (e) #f)])
                        (send simulator interpret (vector my-inst) state))]
                      [out-v (and out-state (progstate-t out-state))])
                 (when out-v
                       (hash-insert-to-list behavior-bw out-v v)))))]


       [(member opcode-name '(+ and or))
        (for* ([v1 val-range]
               [v2 val-range])
              (let ([state (send machine get-state init)])
                (set-progstate-t! state v1)
                (set-progstate-s! state v2)
                (let* ([out-state 
                        (with-handlers*
                         ([exn? (lambda (e) #f)])
                         (send simulator interpret (vector my-inst) state))]
                       [out-v (and out-state (progstate-t out-state))])
                  (when out-v
                        (hash-insert-to-list behavior-bw out-v (list v1 v2))))))]

       [(member opcode-name '(+*))
        (for* ([v1 val-range]
               [v2 val-range]
               [a val-range])
              (let ([state (send machine get-state init)])
                (set-progstate-t! state v1)
                (set-progstate-s! state v2)
                (set-progstate-a! state a)
                (let* ([out-state 
                        (with-handlers*
                         ([exn? (lambda (e) #f)])
                         (send simulator interpret (vector my-inst) state))]
                       [key (and out-state
                                 (list (progstate-t out-state)
                                       (progstate-s out-state)
                                       (progstate-a out-state)))])
                  (when key
                        (for ([key (get-all-keys key)])
                             (hash-insert-to-list behavior-bw key (list v1 v2 a)))))))]
       )

      (hash-set! behaviors-bw opcode-id behavior-bw))

    (define-syntax-rule (stack->vector x) (send machine stack->vector x))

    (define (get-all-keys key)
      (drop ;; drop (list #f #f #f)
       (for*/list ([a 2]
                   [b 2]
                   [c 2])
         (list (and (= a 1) (first key))
               (and (= b 1) (second key))
               (and (= c 1) (third key))))
       1))
               
    (define (interpret-inst my-inst state [ref #f])
      (define a (progstate-a state))
      (define b (progstate-b state))
      (define r (progstate-r state))
      (define s (progstate-s state))
      (define t (progstate-t state))
      (define data (progstate-data state))
      (define data-sp (stack-sp (progstate-data state)))
      (define data-body (vector-copy (stack-body (progstate-data state))))
      (define return-sp (stack-sp (progstate-return state)))
      (define return-body (vector-copy (stack-body (progstate-return state))))
      (define memory (progstate-memory state))
      (define recv (progstate-recv state))
      (define comm (progstate-comm state))
      
      (define opcode-id (inst-op my-inst))
      (define opcode-name (vector-ref opcodes opcode-id))
      (define args (inst-args my-inst))
      (define const (and args (> (vector-length args) 0) (vector-ref args 0)))
      
      (define out-list (list))

      (define (snapshot)
        (set! out-list
              (cons
               (progstate a b r s t 
                          (stack data-sp data-body)
                          (stack return-sp return-body)
                          memory recv comm)
               out-list)))
      
      ;; Pushes a value to the given stack's body.
      (define-syntax-rule (push-stack! x-sp x-body value)
	(begin
	  (set! x-sp (modulo+ (add1 x-sp) 8))
	  (vector-set! x-body x-sp value)
	  ))

      ;; Pops from the given stack's body.
      (define-syntax-rule (pop-stack! x-sp x-body)
	(let ([ret-val (vector-ref x-body x-sp)])
          (vector-set! x-body x-sp #f)
	  (set! x-sp (modulo- (sub1 x-sp) 8))
	  ret-val))

      ;; Pushes to the data stack.
      (define (push! value)
	(push-stack! data-sp data-body s)
	(set! s t)
	(set! t value))
      
      ;; Pushes to the return stack.
      (define (r-push! value)
	(push-stack! return-sp return-body r)
	(set! r value))
      
            ;; Pops from the data stack.
      (define (pop!)
        (set! t s)
        (set! s (pop-stack! data-sp data-body))
        )
      
      ;; Pops from the return stack.
      (define (r-pop!)
        (set! r (pop-stack! return-sp return-body))
        )
      
      (define-syntax-rule (eq-pop! value)
        (when (and t (or (not value) (= t value)))
              (when (not value) (set! value t))
              (set! t s)
              (set! s (pop-stack! data-sp data-body))
              (snapshot)))

      ;; load inverse
      (define-syntax-rule (memeq-pop! a f)
        (let ([t-org t])
          (pop!)
          (cond
           [a
            (set! a (f a))
            (cond
             [(member a (list UP DOWN LEFT RIGHT IO))
              (when (> (get-field index comm) 0)
                    (set! comm (send comm clone)) ;; all clone-all
                    (define token (send comm push-inverse))
                    (define val (first token))
                    (define port (second token))
                    (define type (third token))
                    
                    (when (and (or (not t-org) (= t-org val))
                               (= type 0) (= a port)
                               (> (get-field index recv) 0))
                          (set! recv (send recv clone))
                          (when (send recv pop-inverse val)
                                (snapshot))))
              ]
             [t-org
              (for ([actual-addr (send memory get-addr-with-val t-org)])
                   (when (= a actual-addr)
                         (snapshot)))

              (when
               ref
               (define mem-ref (progstate-memory ref))
               (for ([actual-addr (send memory get-available-addr mem-ref)])
                    (when (= a actual-addr)
                          (set! memory (send (progstate-memory state) clone)) ;; clone before modify
                          (send memory store actual-addr t-org)
                          (snapshot))))
              
              ])
            
            ]
           
           [else
            (when
             t-org
             (for ([actual-addr (send memory get-addr-with-val t-org)])
                  (set! a actual-addr)
                  (snapshot))

             (when
              ref
              (define mem-ref (progstate-memory ref))
              (for ([actual-addr (send memory get-available-addr mem-ref)])
                   (set! a actual-addr)
                   (set! memory (send (progstate-memory state) clone)) ;; clone before modify
                   (send memory store actual-addr t-org)
                   (snapshot))))
            
            (when (> (get-field index comm) 0)
                  (set! memory (progstate-memory state)) ;; reset memory
                  (set! comm (send comm clone)) ;; clone before modify
                  (define token (send comm push-inverse))
                  (define val (first token))
                  (define port (second token))
                  (define type (third token))
                  
                  (when (and (or (not t-org) (= t-org val))
                             (= type 0)
                             (> (get-field index recv) 0))
                        (set! recv (send recv clone))
                        (when (send recv pop-inverse val)
                              (set! a port)
                              (snapshot))))]
           )))

      ;; store inverse
      (define-syntax-rule (mem-to-stack-rm a f)
        (cond
          [a
           (push! #f)
           (set! a (f a))
           (cond
             [(member a (list UP DOWN LEFT RIGHT IO))
              (when (> (get-field index comm) 0)
                    (set! comm (send comm clone)) ;; clone before modify
                    (define token (send comm push-inverse))
                    (define val (first token))
                    (define port (second token))
                    (define type (third token))
                    
                    (when (and (= type 1) (= a port))
                          (set! t val)
                          (snapshot)))]
             [else
              (for/list ([addr-val (send memory get-update-addr-val)])
                (let* ([addr (car addr-val)]
                       [val (cdr addr-val)])
                  (when (= a addr)
                    (set! memory (send (progstate-memory state) clone)) ;; clone before modify
                    (send memory del addr)
                    (set! t val)
                    (snapshot))))
              ])
           
           ]

          [else
           (push! #f)
           (for/list ([addr-val (send memory get-update-addr-val)])
             (let* ([addr (car addr-val)]
                    [val (cdr addr-val)])
               (set! memory (send (progstate-memory state) clone)) ;; clone before modify
               (send memory del addr)
               (set! a addr)
               (set! t val)
               (snapshot)))

           (when (> (get-field index comm) 0)
                 (set! memory (progstate-memory state)) ;; reset memory
                 (set! comm (send comm clone)) ;; clone before modify
                 (define token (send comm push-inverse))
                 (define val (first token))
                 (define port (second token))
                 (define type (third token))
                 
                 (when (= type 1)
                       (set! a port)
                       (set! t val)
                       (snapshot)))]))

      (define (t-t)
        (define behavior (hash-ref behaviors-bw opcode-id))
        (when (hash-has-key? behavior t)
              (define in-t-list (hash-ref behavior t))
              (define in-data (stack->vector (stack data-sp data-body)))
              (define in-return (stack->vector (stack return-sp return-body)))
              (set! out-list
                    (for/list ([in-t in-t-list])
                              (vector a b r s in-t
                                      in-data
                                      in-return
                                      memory recv comm)))))

      (define (ts-t)
        (define behavior (hash-ref behaviors-bw opcode-id))
        (when (hash-has-key? behavior t)
              (define in-list (hash-ref behavior t))
          ;; (pretty-display `(in-list ,in-list))
          (push! #f)
          (define in-data (stack->vector (stack data-sp data-body)))
          (define in-return (stack->vector (stack return-sp return-body)))
          (set! out-list
                (for/list ([in in-list])
                          (let ([in-t (first in)]
                                [in-s (second in)])
                            (vector a b r in-s in-t
                                    in-data
                                    in-return
                                    memory recv comm))))))

      (define (tsa-tsa)
        (define behavior (hash-ref behaviors-bw opcode-id))
        (define key (list t s a))
        (when (hash-has-key? behavior key)
              (define in-list (hash-ref behavior key))
              (define in-data (stack->vector (stack data-sp data-body)))
              (define in-return (stack->vector (stack return-sp return-body)))
              (set! out-list
                    (for/list ([in in-list])
                              (let ([in-t (first in)]
                                    [in-s (second in)]
                                    [in-a (third in)])
                                (vector in-a b r in-s in-t
                                        in-data
                                        in-return
                                        memory recv comm))))))

      
      (define-syntax-rule (inst-eq x) (equal? x opcode-name))

      (cond
       [(inst-eq `@p)   (eq-pop! const)]
       [(inst-eq `@+)   (memeq-pop! a sub1)]
       [(inst-eq `@b)   (memeq-pop! b identity)]
       [(inst-eq `@)    (memeq-pop! a identity)]
       
       [(inst-eq `!+)   (mem-to-stack-rm a sub1)]
       [(inst-eq `!b)   (mem-to-stack-rm b identity)]
       [(inst-eq `!)    (mem-to-stack-rm a identity)]
       
       [(inst-eq `+*)   (when (or t s a) (tsa-tsa))]
       [(member opcode-name '(2* 2/ -)) (when t (t-t))]
       [(member opcode-name '(+ and or)) (when t (ts-t))]
        
       [(inst-eq `drop) 
        (push! #f) (snapshot) ;(drop!)
        ]
       [(inst-eq `dup)  (eq-pop! s)]
       [(inst-eq `pop)  (when t (r-push! t) (pop!) (snapshot))]
       [(inst-eq `a)    (eq-pop! a)]
       [(inst-eq `nop)  (snapshot)]
       [(inst-eq `push) (when r (push! r) (r-pop!) (snapshot))]
       [(inst-eq `b!)   (when b (push! b) (set! b #f) (snapshot))]
       [(inst-eq `a!)   (when a (push! a) (set! a #f) (snapshot))]
       [(inst-eq `over)
        (let ([val (get-stack data 0)]
              [t-org t])
          (when (and t (or (not val) (= t val)))
            (set! t s)
            (pop-stack! data-sp data-body)
            (set! s t-org)
            (snapshot)))]
       [else (assert #f (format "invalid instruction ~a" inst))])

      out-list)
    ))

#|
(define machine (new GA-machine% [bitwidth 4] [config 0]))
(define simulator (new GA-simulator-racket% [machine machine]))

(define inverse (new GA-inverse% [machine machine] [simulator simulator]))
(define printer (new GA-printer% [machine machine]))
(define parser (new GA-parser%))
(define my-inst-0
  (vector-ref (send printer encode 
                    (send parser ir-from-string "+*"))
              0))

(define my-inst 
  (vector-ref (send printer encode 
                    (send parser ir-from-string "!"))
              0))

(define input-state (vector #f 10 #f 2 99
                            (vector #f #f #f #f #f #f #f #f)
                            (make-vector 8 #f)
                            (vector 1 2) (list) 
                            (list (list 3 (get-field DOWN machine) 1))))
                            

(send inverse gen-inverse-behavior my-inst-0)
(send inverse interpret-inst my-inst input-state #f)
|#
