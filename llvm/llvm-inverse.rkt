#lang racket

(require "../ops-racket.rkt" "../inst.rkt" "../inverse.rkt" "../enumerator.rkt")

(provide llvm-inverse%)

(define llvm-inverse%
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

    (define (get-val-range x) val-range)

    
    ;; Generate inverse table behavior for my-inst.
    (define (gen-inverse-behavior my-inst)
      (when
       (send machine is-arithmetic-inst my-inst)
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
      (define opcode-name (vector-ref opcodes (inst-op my-inst)))
      (define args (inst-args my-inst))
      (define ins-types (send machine get-progstate-ins-types my-inst))
      (define outs-types (send machine get-progstate-outs-types my-inst))

      (cond
       [(send machine is-arithmetic-inst my-inst)
        (define key (send machine get-inst-key my-inst))
        (define out-vals (send machine get-progstate-outs-vals my-inst state))
        ;;(pretty-display `(key ,key ,out-vals))
        (define state-base (send machine kill-outs my-inst state)) ;; TODO: do we have to use old-liveout?

        (define mapping (hash-ref behaviors-bw key))
        (define in-vals-list (and (hash-has-key? mapping out-vals) (hash-ref mapping out-vals)))
        (and in-vals-list
             (for/list ([in-vals in-vals-list])
                       (send machine update-progstate-ins my-inst in-vals state-base)))]

       [(and (= (length outs-types) 1)
             (= (length ins-types) 2)
             (member `mem ins-types)) ;; load
        ;; TODO: doesn't work for ARM (e.g. str	r1, [r0, r2, asl #2] | str r3, [fp, #-12])
        (define out-val (car (send machine get-progstate-outs-vals my-inst state)))
        (define in-vals (send machine get-progstate-ins-vals my-inst state))
        (define mem #f)
        (for ([in in-vals]
              [type ins-types])
          (when (equal? type `mem)
            (set! mem in)))
        (filter
         (lambda (x) x)
         (for/list ([actual-addr (send mem get-addr-with-val out-val)])
                   (send machine update-progstate-ins-load my-inst actual-addr state)))
        ]

       [(and (= (length outs-types) 1) (equal? `mem (car outs-types))) ;; store
        (define mem (car (send machine get-progstate-outs-vals my-inst state)))
        (define addr-var-list (send mem get-update-addr-val))
        (filter
         (lambda (x) x)
         (for/list ([addr-val addr-var-list])
                   (let* ([addr (car addr-val)]
                          [val (cdr addr-val)])
                     (send machine update-progstate-ins-store my-inst addr val state))))
        ]

       [else (raise "interpret-inst-backward")] ;; TODO
       ))

    ))
