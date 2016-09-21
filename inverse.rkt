#lang racket

(require "inst.rkt" "machine.rkt" "enumerator.rkt" "ops-racket.rkt")
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
    (define bit (get-field bitwidth machine))
    (define val-range
      (for/vector ([v (arithmetic-shift 1 bit)]) (finitize v bit)))

    ;; Inverse tables for all instructions.
    (define behaviors-bw (make-hash))

    ;; Return a list of valid abstract values given a program state type.
    (define (get-val-range type) val-range)
    
    ;; Generate inverse table behavior for my-inst.
    (define (gen-inverse-behavior my-inst)
      (define ins-types (send machine get-progstate-ins-types my-inst))
      (define outs-types (send machine get-progstate-outs-types my-inst))
      (when
       (not (member (get-memory-type) (append ins-types outs-types)))
       ;; Inverse table behavior
       (define behavior-bw (make-hash))
       (define state (send machine get-state (lambda () #f)))
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
                    (hash-insert-to-list behavior-bw out-vals in-vals))))

       (hash-set! behaviors-bw (send machine get-inst-key my-inst) behavior-bw)))
    
    
    ;; Inverse interpret my-inst using the pre-computed 'behaviors-bw'.
    ;; my-inst: instruction
    ;; state-vec: progstate in vector/list/pair format
    ;; old-liveout: liveout info
    ;; output: a list of progstates in vector/list/pair format
    (define (interpret-inst my-inst state old-liveout)
      (define args (inst-args my-inst))
      (define ins-types (send machine get-progstate-ins-types my-inst))
      (define outs-types (send machine get-progstate-outs-types my-inst))

      (cond
       [(not (member (get-memory-type) (append ins-types outs-types)))
        (define key (send machine get-inst-key my-inst))
        (define out-vals (send machine get-progstate-outs-vals my-inst state))
        ;;(pretty-display `(key ,key ,out-vals))
        (define state-base (send machine kill-outs my-inst state)) ;; TODO: do we have to use old-liveout?

        (define mapping (hash-ref behaviors-bw key))
        (define in-vals-list (and (hash-has-key? mapping out-vals) (hash-ref mapping out-vals)))
        (and in-vals-list
             (for/list ([in-vals in-vals-list])
                       (send machine update-progstate-ins my-inst in-vals state-base)))]

       [(and (= (length outs-types) 1) (= (length ins-types) 2)
             (member (get-memory-type) ins-types)) ;; load
        (define out-val (car (send machine get-progstate-outs-vals my-inst state)))
        (define in-vals (send machine get-progstate-ins-vals my-inst state))
        (define mem #f)
        (for ([in in-vals]
              [type ins-types])
          (when (equal? type (get-memory-type))
            (set! mem in)))
        (filter
         (lambda (x) x)
         (for/list ([actual-addr (send mem get-addr-with-val out-val)])
                   (send machine update-progstate-ins-load my-inst actual-addr state)))
        ]

       [(and (= (length outs-types) 1) (equal? (get-memory-type) (car outs-types))) ;; store
        (define mem (car (send machine get-progstate-outs-vals my-inst state)))
        (define addr-var-list (send mem get-update-addr-val))
        (filter
         (lambda (x) x)
         (for/list ([addr-val addr-var-list])
                   (let* ([addr (car addr-val)]
                          [val (cdr addr-val)]
                          [new-state (send machine update-progstate-ins-store my-inst addr val state)])
                     (and new-state
                          (send machine update-progstate-del-mem addr new-state)))))
        ]

       [else (raise "interpret-inst-backward")] ;; TODO
       ))
    
    ))
