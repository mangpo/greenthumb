#lang racket

(require "../ops-racket.rkt" "../inst.rkt" "../inverse.rkt" "../enumerator.rkt"
         "arm-inst.rkt" "arm-machine.rkt")

(provide arm-inverse%)

(define arm-inverse%
  (class inverse%
    (super-new)
    ;; (inherit-field machine simulator)
    ;; (inherit lookup-bw)
    ;; (override gen-inverse-behavior interpret-inst uid-inst-in-out)

    ;; (define bit (get-field bitwidth machine))
    ;; (define opcodes (get-field opcodes machine))
    ;; (define shf-opcodes (get-field shf-opcodes machine))
    ;; (define shf-inst-reg (get-field shf-inst-reg machine))
    ;; (define reg-range-db
    ;;   (for/vector ([v (arithmetic-shift 1 bit)]) (finitize v bit)))
    ;; (define fp (send machine get-fp))

    ;; (define (uid-inst-in-out x)
    ;;   (define-values (inst-type in out) (super uid-inst-in-out x))

    ;;   (define shfop (inst-shfop x))
    ;;   (define shfarg (inst-shfarg x))
    ;;   (define inst-type-append (list shfop))
    ;;   (when (> shfop 0)
    ;;         (if (member (vector-ref shf-opcodes shfop) shf-inst-reg)
    ;;     	(set! in (append in (list shfarg)))
    ;;     	(set! inst-type-append (append inst-type-append (list shfarg)))))
    ;;   (values (append inst-type inst-type-append) in out))
    
    ;; ;; Inverse tables for all instructions.
    ;; (define behaviors-bw (make-hash))

    ;; ;; Generate inverse table behavior for my-inst.
    ;; (define (gen-inverse-behavior my-inst)
    ;;   (define opcode-name (vector-ref opcodes (inst-op my-inst)))
    ;;   ;; For collecting which registers in my-inst are input and output.
    ;;   (define in (make-vector 5 #f))
    ;;   (define out (make-vector 5 #f))
    ;;   ;; Inverse table behavior
    ;;   (define behavior-bw (make-hash))
          
    ;;   ;; Collect information on which registers are input and output.
    ;;   (define arg-types (send machine get-arg-types opcode-name))
    ;;   (define shfop (inst-shfop my-inst))
    ;;   (define shfarg (inst-shfarg my-inst))
    ;;   (when (and shfop (member (vector-ref shf-opcodes shfop) shf-inst-reg))
    ;;         (vector-set! in shfarg #t))
      
    ;;   (for ([arg (inst-args my-inst)]
    ;;         [type arg-types])
    ;;        (cond
    ;;         [(equal? type `reg-o) (vector-set! out arg #t)]
    ;;         [(equal? type `reg-i) (vector-set! in arg reg-range-db)]
    ;;         [(equal? type `reg-io)
    ;;          (vector-set! out arg #t)
    ;;          (vector-set! in arg reg-range-db)]))

    ;;   (define (inner in-res)
    ;;     (define out-state
    ;;       (with-handlers*
    ;;        ([exn? (lambda (e) #f)])
    ;;        (send simulator interpret
    ;;              (vector my-inst)
    ;;              (progstate (list->vector in-res) (vector) -1 fp))))
        
    ;;     (when 
    ;;      out-state
    ;;      (define in-list-filtered (filter number? in-res))
    ;;      (define out-list (list))
    ;;      (for ([r (progstate-regs out-state)]
    ;;            [m out])
    ;;           (when m (set! out-list (cons r out-list))))
         
    ;;      (define key (reverse out-list))
         
    ;;      ;; Insert into the inverse table. 
    ;;      (hash-insert-to-list behavior-bw key in-list-filtered)))
        
    ;;   (for ([in-res (all-combination-list (vector->list in))])
    ;;        (inner in-res))
      
    ;;   (define-values (x regs-in regs-out) (uid-inst-in-out my-inst))
    ;;   ;;(pretty-display `(behavior-bw ,behavior-bw))
    ;;   (hash-set! behaviors-bw x behavior-bw))

    ;; ;; Inverse interpret my-inst using the pre-computed 'behaviors-bw'.
    ;; ;; my-inst: instruction
    ;; ;; state-vec: progstate in vector/list/pair format
    ;; ;; old-liveout: compact format
    ;; ;; output: a list of progstates in vector/list/pair format
    ;; (define (interpret-inst my-inst state-vec old-liveout)
    ;;   ;;(pretty-display `(interpret ,state-vec ,old-liveout))
    ;;   (define opcode-name (vector-ref opcodes (inst-op my-inst)))
    ;;   (define cond-type (arm-inst-cond my-inst))

    ;;   (define regs (vector-ref state-vec 0))
    ;;   (define mem (vector-ref state-vec 1))
    ;;   (define z (vector-ref state-vec 2))
    ;;   (define fp (vector-ref state-vec 3))

    ;;   (define (exec-reg)
    ;;     (define-values (x regs-in regs-out) (uid-inst-in-out my-inst))
    ;;     (define regs-base (make-vector (vector-length regs) #f))
    ;;     (for ([i (car old-liveout)])
    ;;          (unless (member i regs-out) (vector-set! regs-base i (vector-ref regs i))))
    ;;     (define regs-out-val 
    ;;       (for/list ([r regs-out]) (vector-ref regs r)))

    ;;     (define mapping (hash-ref behaviors-bw x))
    ;;     (define ret (lookup-bw mapping regs-in regs-out-val regs-base))
    ;;     (and ret (map (lambda (x) (vector x mem z fp)) ret))
    ;;     )

    ;;   (define (exec)
    ;;     (cond
    ;;      [(equal? opcode-name `ldr#)
    ;;       (define args (inst-args my-inst))
    ;;       (define mem-index (+ fp (vector-ref args 2)))
    ;;       ;;(pretty-display `(debug ,fp ,mem-index))
    ;;       (define reg-index (vector-ref args 0))
    ;;       (define reg-val (vector-ref regs reg-index))
    ;;       (define pass #t)
    ;;       (when (and (member mem-index (cdr old-liveout))
    ;;                  (not (= (vector-ref mem mem-index) reg-val)))
    ;;         (set! pass #f))
          
    ;;       (and pass
    ;;            (let ([new-mem (vector-copy mem)]
    ;;                  [new-regs (vector-copy regs)])
    ;;              (vector-set! new-mem mem-index reg-val)
    ;;              (vector-set! new-regs reg-index #f)
    ;;              (list (vector new-regs new-mem z fp))))
    ;;       ]

    ;;      [(equal? opcode-name `str#)
    ;;       (define args (inst-args my-inst))
    ;;       (define mem-index (+ fp (vector-ref args 2)))
    ;;       (define reg-index (vector-ref args 0))
    ;;       (define mem-val (vector-ref mem mem-index))
    ;;       (define pass #t)
    ;;       (when (and (member reg-index (car old-liveout))
    ;;                  (not (= (vector-ref regs reg-index) mem-val)))
    ;;         (set! pass #f))
          
    ;;       (and pass
    ;;            (let ([new-mem (vector-copy mem)]
    ;;                  [new-regs (vector-copy regs)])
    ;;              (vector-set! new-regs reg-index mem-val)
    ;;              (vector-set! new-mem mem-index #f)
    ;;              (list (vector new-regs new-mem z fp))))]
    ;;      [else (exec-reg)]))
     
    ;;   (define (same) (list state-vec))
    ;;   ;; TODO: z != -1

    ;;   (cond
    ;;    [(member opcode-name '(tst cmp tst# cmp#))
    ;;     (list (vector (vector-copy regs) (vector-copy mem) -1 fp))]

    ;;    [(or (equal? cond-type 0) (equal? z -1))
    ;;     (exec)]

    ;;    [(equal? cond-type 1) ;; eq
    ;;     (if (equal? z 0) (exec) (same))]

    ;;    [(equal? cond-type 2) ;; ne
    ;;     (if (member z (list 1 2 3 4 5)) (exec) (same))]

    ;;    [(equal? cond-type 3) ;; ls
    ;;     (if (member z (list 0 2 5)) (exec) (same))]

    ;;    [(equal? cond-type 4) ;; hi
    ;;     (if (member z (list 3 4)) (exec) (same))]

    ;;    [(equal? cond-type 5) ;; cc
    ;;     (if (member z (list 2 5)) (exec) (same))]

    ;;    [(equal? cond-type 6) ;; cs
    ;;     (if (member z (list 0 3 4)) (exec) (same))]

    ;;    [(equal? cond-type 7) ;; lt
    ;;     (if (member z (list 2 4)) (exec) (same))]
       
    ;;    [(equal? cond-type 8) ;; ge
    ;;     (if (member z (list 0 3 5)) (exec) (same))]
       
    ;;    [else (raise (format "illegal cond-type ~a" cond-type))]
    ;;    ))
      

    ))
