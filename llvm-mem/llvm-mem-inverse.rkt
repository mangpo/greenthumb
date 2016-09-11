#lang racket

(require "../ops-racket.rkt" "../inst.rkt" "../inverse.rkt" "../enumerator.rkt")

(provide llvm-mem-inverse%)

(define llvm-mem-inverse%
  (class inverse%
    (super-new)
    (inherit-field machine simulator)
    (inherit lookup-bw)
    (override gen-inverse-behavior interpret-inst uid-inst-in-out)

    ;; Reduced-bit
    (define bit (get-field bitwidth machine))
    (define opcodes (get-field opcodes machine))
    (define val-range
      (for/vector ([v (arithmetic-shift 1 bit)]) (finitize v bit)))

    (define (uid-inst-in-out x)
      (define-values (i in out) (super uid-inst-in-out x))
      (values i in (car out)))

    ;; Inverse tables for all instructions.
    (define behaviors-bw (make-hash))

    ;; Generate inverse table behavior for my-inst.
    (define (gen-inverse-behavior my-inst)
      (define opcode (inst-op my-inst))
      (define opcode-name (vector-ref opcodes opcode))
      (unless (member opcode-name '(load store))
            (define args (inst-args my-inst))
            ;; For collecting which registers in my-inst are input and output.
            (define in (make-vector 5 #f))
            (define out-reg (vector-ref args 0))
            ;; Inverse table behavior
            (define behavior-bw (make-hash))
            ;; Collect information on which registers are input and output.
            (for ([arg args]
                  [type (send machine get-arg-types opcode-name)])
                 (when (equal? type `var-i) (vector-set! in arg val-range)))

            (for ([in-res (all-combination-list (vector->list in))])
                 (let ([out-state
                        (with-handlers*
                         ([exn? (lambda (e) #f)])
                         (send simulator interpret
                               (vector my-inst)
                               (vector (list->vector in-res) #f)))])
                   
                   (when 
                    out-state
                    (define in-list-filtered (filter number? in-res))
                    (define key2 (vector-ref (vector-ref out-state 0) out-reg))

                    ;; Insert into the inverse table. 
                    (hash-insert-to-list behavior-bw key2 in-list-filtered))))
            
            (define-values (key1 vars-in var-out) (uid-inst-in-out my-inst))
            ;;(pretty-display `(behavior-bw ,behavior-bw))
            (hash-set! behaviors-bw key1 behavior-bw))
      )
    
    ;; Inverse interpret my-inst using the pre-computed 'behaviors-bw'.
    ;; my-inst: instruction
    ;; state-vec: progstate in vector/list/pair format
    ;; old-liveout: liveout info
    ;; output: a list of progstates in vector/list/pair format
    (define (interpret-inst my-inst state old-liveout)
      (define opcode-name (vector-ref opcodes (inst-op my-inst)))
      (define args (inst-args my-inst))
      (define state-vars (vector-copy (vector-ref state 0)))
      (define state-mem (send (vector-ref state 1) clone-all))
      
      (cond
       [(equal? opcode-name 'load)
        (define d (vector-ref args 0))
        (define a (vector-ref args 1))
        (define addr (vector-ref state-vars (vector-ref args 1))) ;; maybe #f
        (define val (vector-ref state-vars d)) ;; not #f
        (vector-set! state-vars d #f)
        
        (cond
         ;; if addr != #f
         [addr
          (define mem-val (send state-mem lookup-update addr))
          (if (equal? mem-val val)
              (list (vector state-vars state-mem))
              (list (vector state-vars state-mem)))
          ]

         ;; addr = #f
         [else
          (for/list
           ([addr (send state-mem get-addr-with-val val)]) 
           (let ([new-state-vars (vector-copy state-vars)])
             (vector-set! new-state-vars a addr)
             (vector new-state-vars state-mem)))
          ])
        ]
       
       [(equal? opcode-name 'store)
        (define d (vector-ref args 0))
        (define a (vector-ref args 1))
        (define addr (vector-ref state-vars a)) ;; maybe #f
        (define val (vector-ref state-vars d)) ;; maybe #f

        (cond
         ;; if addr != #f
         [addr
          (define mem-val (send state-mem lookup-update addr))
          (cond
           [(and mem-val (or (equal? val #f) (equal? val mem-val)))
            (send state-mem del addr)
            (vector-set! state-vars d mem-val)
            (list (vector state-vars state-mem))]
           [else (list)])]

         ;; if addr = #f
         [else
          (cond
           ;; if val != #f
           [val
            (for/list
             ([addr (send state-mem get-update-addr-with-val val)])
             (let ([new-state-vars (vector-copy state-vars)]
                   [new-state-mem (send state-mem clone-all)])
               (send new-state-mem del addr)
               (vector-set! new-state-vars a addr)
               (vector new-state-vars new-state-mem)))
            ]

           ;; if val = #f
           [else
            (for/list
             ([addr-val (send state-mem get-update-addr-val)])
             (let* ([addr (car addr-val)]
                    [val (cdr addr-val)]
                    [new-state-vars (vector-copy state-vars)]
                    [new-state-mem (send state-mem clone-all)])
               (send new-state-mem del addr)
               (vector-set! new-state-vars a addr)
               (vector-set! new-state-vars d val)
               (vector new-state-vars new-state-mem)))
            ]
           )
          ]
         )
        ]

       [else
        (define n (vector-length state-vars))

        (define-values (key1 vars-in var-out) (uid-inst-in-out my-inst))
        
        ;; Extract values of var-out from state-vars.
        ;; This corresponds to key2 in gen-inverse-behavior.
        ;; It will be used as key2 to the same table.
        (define key2 (vector-ref state-vars var-out))
        
        ;; A vector for initializing a vector containing values of variables in the input state-vars.
        (define state-vars-base (make-vector n #f))
        (for ([i n]
              [l old-liveout])
             (when (and l (not (= i var-out)))
                   (vector-set! state-vars-base i (vector-ref state-vars i))))

        (define mapping (hash-ref behaviors-bw key1))
        (for/list ([new-state-vars (lookup-bw mapping vars-in key2 state-vars-base)])
                  (vector new-state-vars state-mem))
        ])
      )

    ))
