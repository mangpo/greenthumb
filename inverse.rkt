#lang racket

(require "inst.rkt" "machine.rkt" "enumerator.rkt" "ops-racket.rkt" "memory-racket.rkt" "special.rkt")
(provide inverse% hash-insert-to-list)

(define-syntax-rule (hash-insert-to-list table key val)
  (if (hash-has-key? table key)
      (hash-set! table key
                 (cons val (hash-ref table key)))
      (hash-set! table key (list val))))

(define inverse%
  (class object%
    (super-new)
    (init-field machine simulator)
    (public gen-inverse-behavior interpret-inst)
    ;; Reduced-bit
    (field [bit (get-field bitwidth machine)])

    ;; Inverse tables for all instructions.
    (define behaviors-bw (make-hash))

    (define type2range (make-hash))
    (let ([val-range (for/vector ([v (arithmetic-shift 1 bit)]) (finitize v bit))]
          [types (send machine get-all-progstate-types)])
      (for ([type types])
           (let-values ([(min-v max-v const) (send machine get-progstate-type-min-max-const type)])
             (hash-set! type2range type
                        (cond
                         [const const]
                         [(and min-v max-v) (list->vector (range min-v (add1 max-v)))]
                         [else val-range])))))
             
    ;; Return a list of valid abstract values given a program state type.
    (define (get-val-range type) (hash-ref type2range type))
    
    ;; Generate inverse table behavior for my-inst.
    (define (gen-inverse-behavior my-inst)
      (define ins-types (send machine get-progstate-ins-types my-inst))
      (define outs-types (send machine get-progstate-outs-types my-inst))
      (when
       (not (member (get-memory-type) (append ins-types outs-types)))
       ;; Inverse table behavior
       (define behavior-bw (make-hash))
       (define state (send machine get-state
                           (lambda (#:min [min #f] #:max [max #f] #:const [const #f]) #f)))
       (define ins-range-list
         (for/list ([type (send machine get-progstate-ins-types my-inst)])
                   (get-val-range type)))

       (for ([in-vals (all-combination-list ins-range-list)])
            (let* ([in-state (send machine update-progstate-ins my-inst in-vals state)]
                   [out-state (with-handlers*
                               ([exn? (lambda (e) #f)])
                               (send simulator interpret (vector my-inst) in-state))])
              (when out-state
                    (define out-vals (send machine get-progstate-outs-vals my-inst out-state))
                    (hash-insert-all-combinations behavior-bw out-vals in-vals))))

       (hash-set! behaviors-bw (send machine get-inst-key my-inst) behavior-bw)))

    (define (hash-insert-all-combinations table out-vals in-vals)
      ;; keep all
      (hash-insert-to-list table out-vals in-vals)
      (define n (length out-vals))
      (when (> n 1)
            ;; keep 1st
            (hash-insert-to-list table (cons (car out-vals) (for/list ([i (sub1 n)]) #f)) in-vals)
            ;; keep non 1st
            (hash-insert-to-list table (cons #f (cdr out-vals)) in-vals)
            ;; TODO: to gaurantee optimality => need to mask out all combinations
            ))
    
    
    ;; Inverse interpret my-inst using the pre-computed 'behaviors-bw'.
    ;; my-inst: instruction
    ;; state-vec: progstate in vector/list/pair format
    ;; old-liveout: liveout info
    ;; output: a list of progstates in vector/list/pair format
    (define (interpret-inst my-inst state old-liveout [ref #f])
      (define args (inst-args my-inst))
      (define ins-types (send machine get-progstate-ins-types my-inst))
      (define outs-types (send machine get-progstate-outs-types my-inst))

      (cond
       [(not (member (get-memory-type) (append ins-types outs-types)))
        (define key (send machine get-inst-key my-inst))
        (define out-vals (send machine get-progstate-outs-vals my-inst state))
        (define state-base (send machine kill-outs my-inst state)) ;; TODO: do we have to use old-liveout?

        (define mapping (hash-ref behaviors-bw key))
        (define in-vals-list (and (hash-has-key? mapping out-vals) (hash-ref mapping out-vals)))
        (and in-vals-list
             (for/list ([in-vals in-vals-list])
                       (send machine update-progstate-ins my-inst in-vals state-base)))]

       ;; load
       [(member (get-memory-type) ins-types)
        (define out-vals (send machine get-progstate-outs-vals my-inst state))
        (define out-val (car out-vals))
        (define in-vals (send machine get-progstate-ins-vals my-inst state))
        (define mem (findf (lambda (x) (is-a? x memory-racket%)) in-vals))
        (cond
         [(and out-val mem)
          (define
            ret
            (filter
             (lambda (x) x)
             (for/list ([actual-addr (send mem get-addr-with-val out-val)])
                       (send machine update-progstate-ins-load my-inst actual-addr mem state))))

          (when
           ref
           (define in-vals-ref (send machine get-progstate-ins-vals my-inst ref))
           (define mem-ref (and ref (findf (lambda (x) (is-a? x memory-racket%)) in-vals-ref)))
           (set! ret
                 (append
                  ret
                  (filter
                   (lambda (x) x)
                   (for/list ([actual-addr (send mem get-available-addr mem-ref)])
                             (let* ([new-mem (send mem clone)] ;;clone-all
                                    [new-state (send machine update-progstate-ins-load
                                                     my-inst actual-addr new-mem state)])
                               (send new-mem store actual-addr out-val)
                               new-state))))))
          
          ret
          ]
         [else #f])
        ]

       ;; store
       [(member (get-memory-type) outs-types)
        (define out-vals (send machine get-progstate-outs-vals my-inst state))
        (define mem (findf (lambda (x) (is-a? x memory-racket%)) out-vals))
        (cond
         [mem
          ;;(pretty-display `(out-vals ,state ,out-vals))
          (define addr-var-list (send mem get-update-addr-val))
          (filter
           (lambda (x) x)
           (for/list ([addr-val addr-var-list])
                     (let* ([addr (car addr-val)]
                            [val (cdr addr-val)]
                            [new-state
                             (send machine update-progstate-ins-store my-inst addr val state)])
                       (and new-state
                            (send machine update-progstate-del-mem addr new-state)))))]

         [else #f])
        ]

       [else (raise "interpret-inst-backward")] ;; TODO
       ))
    
    ))
