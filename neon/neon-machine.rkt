#lang racket

(require "../machine.rkt" "../inst.rkt" "neon-inst.rkt")

(provide neon-machine% (all-defined-out))

;; Machine state (intput/output representaiton)
(struct progstate (dregs rregs memory))

(define-syntax-rule (build-vector n init)
  (let ([vec (make-vector n)])
    (for ([i (in-range n)])
	 (vector-set! vec i (init)))
    vec))

;; Use withing neon
;; Macro to create symbolic, random, or predefined machine state
(define-syntax default-state
  (syntax-rules (rreg dreg mem)
    ((default-state machine init)
     (progstate (build-vector (* 8 (send machine get-nregs-d)) init) 
                (build-vector (send machine get-nregs-r) init) 
                (build-vector (* 8 (send machine get-nmems)) init)))

    ((default-state machine init 
       [dreg (a b) ...] [rreg (c d) ...] [mem (e f) ...])
     (let* ([state (default-state machine init)]
            [dregs (progstate-dregs state)]
            [rregs (progstate-rregs state)]
            [memory (progstate-memory state)])
       (vector-set! dregs a b)
       ...
       (vector-set! rregs c d)
       ...
       (vector-set! memory e f)
       ...
       state))))

(define (lam-t) #t)
(define (lam-f) #f)

;; Use withing neon
;; Macros to create output state constraint
(define-syntax constraint
  (syntax-rules (all none dreg rreg mem mem-all)
    ((constraint machine all) (default-state machine lam-t))

    ((constraint machine none) (default-state machine lam-f))

    ((constraint machine [dreg d ...] [rreg r ...] [mem-all])
     (let* ([state (progstate 
                    (build-vector (* 8 (send machine get-nregs-d)) lam-f)
                    (build-vector (send machine get-nregs-r) lam-f) 
                    (build-vector (* 8 (send machine get-nmems)) lam-t))]
            [dregs (progstate-dregs state)]
            [rregs (progstate-rregs state)])
       (for ([i 8]) (vector-set! dregs (+ (* 8 d) i) #t))
       ...
       (vector-set! rregs r #t)
       ...
       state))
    ))

(define neon-machine%
  (class machine%
    (super-new)
    (inherit-field bit nop-id random-input-bit inst-id classes classes-len perline)
    (inherit print-line)
    (override set-config get-config set-config-string
              adjust-config finalize-config config-exceed-limit?
              get-state display-state
              output-constraint-string
              progstate->vector vector->progstate
	      get-arg-ranges window-size analyze-args)

    ;; Initize common fields for neon
    (set! bit 32)
    (set! random-input-bit 8)
    (set! nop-id 0)
    (set! inst-id '#(nop
                     vld1 vld2 ;vld3 vld4
                     vld1! vld2! ;vld3! vld4!
                     vst1 vst2
                     vst1! vst2!
                     vext# vtrn vzip vuzp vswp

                     vmla vmla@ vmlal vmlal@
                     vmov vmov# ;vmovl vmovn vqmovn
                     ;;vmvn vmvn#
                     vand vand# vorr vorr# vbsl
                     vadd vsub
                     vhadd vhsub
                     vshr#
                     ))
    (set! classes (vector '(vld1 vld1! vst1 vst1!)
                        '(vld2 vld2! vst2 vst2!)
                        '(vmla vadd vsub vhadd vhsub vand vorr vbsl)
                        '(vmov vtrn vzip vuzp vswp)
                        '(vmov# vand# vorr#)
                        '(vmla@ vext#))) ;; vshr#, vmlal@, vmlal
    (set! perline 8)
    (set! classes-len (vector-length classes))

    ;; Neon-only fields
    (init-field [ninsts #f] [ntypes #f])

    ;; private field
    (define nregs-d 10)
    (define nregs-r 4)
    (define nmems 4)
    
    (define dreg-range #f)
    (define qreg-range #f)
    (define rreg-range #f)
    (define const-range #f)
    (define index-range #f)

    (define/public (get-nregs-d) nregs-d)
    (define/public (get-nregs-r) nregs-r)
    (define/public (get-nmems) nmems)

    (define type-id '#(s u i))
    (set! ninsts (vector-length inst-id))
    (set! ntypes (vector-length type-id))
    
    (define (window-size) 34)

    (define (get-config)
      (list nregs-d nregs-r nmems))

    ;; TODO
    ;; info: (list nregs nmem)
    (define (set-config info)
      (set! nregs-d (first info))
      (set! nregs-r (second info))
      (set! nmems (third info))
      (set! dreg-range (list->vector (range nregs-d)))
      (set! qreg-range (list->vector (range 32 (+ 32 (quotient nregs-d 2)))))
      (set! rreg-range (list->vector (range nregs-r)))
      (set! const-range (vector 0 1))
      (set! index-range (list->vector (range 1 8)))
      )

    ;; TODO
    ;; info: (list nregs nmem)
    (define (set-config-string info)
      (format "(list ~a ~a ~a)" (first info) (second info) (third info)))

    (define (adjust-config info)
      ;; Double the memory size
      (list (first info) (second info) (* 2 (third info))))

    (define (finalize-config info)
      ;; Double the memory size
      (list (first info) (second info) (add1 (third info))))

    (define (config-exceed-limit? info)
      ;; Memory size > 1000
      (> (third info) 1000))

    ;; TODO
    ;; live-out: a list of live registers, same format is the output of (select-code) and (combine-live-out)
    ;; output: output constraint corresponding to live-out in string. When executing, the expression is evaluated to a progstate with #t and #f indicating which entries are constrainted (live).
    (define (output-constraint-string live-out)
      (cond
       [live-out
        (define live-regs-d-str (string-join (map number->string (first live-out))))
        (define live-regs-r-str (string-join (map number->string (second live-out))))
        (format "(constraint machine [dreg ~a] [rreg ~a] [mem-all])" 
                live-regs-d-str live-regs-r-str)]
       [else #f]))

    (define (get-state init extra)
      (default-state this init))

    (define (display-state s)
      (pretty-display "DREGS:")
      (print-line (progstate-dregs s))
      (pretty-display "RREGS:")
      (print-line (progstate-rregs s))
      (pretty-display "MEMORY:")
      (print-line (progstate-memory s)))

    (define (no-assumption)
      #f)

    (define (progstate->vector x)
      (vector (progstate-dregs x)
              (progstate-rregs x)
              (progstate-memory x)))
    
    (define (vector->progstate x)
      (progstate (vector-ref x 0)
                 (vector-ref x 1)
                 (vector-ref x 2)))

    (define/public (get-type-id type)
      (vector-member type type-id))

    (define/public (get-type-name id)
      (vector-ref type-id id))

    
    ;; TODO: better way to define this
    (define (get-arg-ranges opcode-name entry live-in)
      (define args (inst-args entry))
      (cond
       [(member opcode-name '(nop))
        (vector)]

       [(member opcode-name '(vld1 vld1! vld2 vld2! vst1 vst1! vst2 vst2!)) 
        (vector #f rreg-range)]

       [(member opcode-name '(vmov# vand# vorr#)) ;; TODO: different const-range for mvni
        (if (< (vector-ref args 0) nregs-d)
            (vector dreg-range const-range)
            (vector qreg-range const-range))]

       [(member opcode-name '(vmov vtrn vzip vuzp vswp))
        (if (< (vector-ref args 0) nregs-d)
            (vector dreg-range dreg-range)
            (vector qreg-range qreg-range))]

       [(member opcode-name '(vmla vand vorr vbsl vadd vsub vhadd vhsub))
        (if (< (vector-ref args 0) nregs-d)
            (vector dreg-range dreg-range dreg-range)
            (vector qreg-range qreg-range qreg-range))]
       
       [(member opcode-name '(vmla@ vext#))
        (define byte (inst-byte entry))
        (define index-range (list->vector (range (quotient 8 byte))))
        (if (< (vector-ref args 0) nregs-d)
            (vector dreg-range dreg-range dreg-range index-range)
            (vector qreg-range qreg-range qreg-range index-range))]
       
       [(member opcode-name '(vmlal))
        (vector qreg-range dreg-range dreg-range)]
       
       [(member opcode-name '(vmlal@))
        (define byte (inst-byte entry))
        (define index-range (list->vector (range (quotient 8 byte))))
        (vector qreg-range dreg-range dreg-range index-range)]

       [(member opcode-name '(vshr#))
        (if (< (vector-ref args 0) nregs-d)
            (vector dreg-range dreg-range const-range)
            (vector qreg-range qreg-range const-range))]

       ))

    (define (get-constants-inst x)
      (define opcode (send machine get-inst-name (inst-op x)))
      (define args (inst-args x))

      (define-syntax-rule (collect x ...)
        (collect-inst (list x ...)))

      (define (collect-inst fs)
        (define constants (list))
        (for ([f fs]
              [arg args])
             (when f (set! constants (cons arg constants))))
        constants)

      (cond
       [(member opcode '(vmov# vand# vorr#))
        (collect #f #t)]
       [(member opcode '(vshr#))
        (collect #f #f #t)]
       [else (list)]))

    (define (analyze-args prefix code postfix)
      (list->set (flatten (for/list ([x code]) (get-constants-inst x)))))

    
    ))
