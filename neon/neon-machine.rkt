#lang racket

(require "../machine.rkt")

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
     (progstate (build-vector (* 8 (get-field nregs-d machine)) init) 
                (build-vector (get-field nregs-r machine) init) 
                (build-vector (* 8 (get-field nmems machine)) init)))

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
                    (build-vector (* 8 (get-field nregs-d machine)) lam-f)
                    (build-vector (get-field nregs-r machine) lam-f) 
                    (build-vector (* 8 (get-field nmems machine)) lam-t))]
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
    (inherit-field bit inst-id classes classes-len perline)
    (inherit print-line)
    (override set-config get-config set-config-string
              adjust-config config-exceed-limit?
              get-state display-state
              output-constraint-string)

    ;; Initize common fields for neon
    (set! bit 32)
    (set! inst-id '#(nop
                     vld1 vld2 ;vld3 vld4
                     vld1! vld2! ;vld3! vld4!
                     vext#
                     vmla vmla@ vmlal vmlal@
                     vmov vmov# ;vmovl vmovn vqmovn
                     ;;vmvn vmvn#
                     vand vand#))
    (set! classes (vector '(vld1 vld1!)
                        '(vld2 vld2!)
                        ;; '(vmla vand)
                        ;; '(vmla# vext#)
                        ;; '(vmlal)
                        ;; '(vmlal@)
                        ;; '(vmov)
                        '(vmov# vand#)))
    (set! perline 8)
    (set! classes-len (vector-length classes))

    ;; Neon-only fields
    (init-field [nregs-d 10] [nregs-r 4] [nmems 4]
                [ninsts #f] [ntypes #f])

    (define type-id '#(s u i))
    (set! ninsts (vector-length inst-id))
    (set! ntypes (vector-length type-id))

    (define (get-config)
      (list nregs-d nregs-r nmems))

    ;; TODO
    ;; info: (list nregs nmem)
    (define (set-config info)
      (set! nregs-d (first info))
      (set! nregs-r (second info))
      (set! nmems (third info))
      )

    ;; TODO
    ;; info: (list nregs nmem)
    (define (set-config-string info)
      (format "(list ~a ~a ~a)" (first info) (second info) (third info)))

    (define (adjust-config info)
      ;; Double the memory size
      (list (first info) (second info) (* 2 (third info))))

    (define (config-exceed-limit? info)
      ;; Memory size > 1000
      (> (third info) 1000))

    ;; TODO
    ;; live-out: a list of live registers, same format is the output of (select-code) and (combine-live-out)
    ;; output: output constraint corresponding to live-out in string. When executing, the expression is evaluated to a progstate with #t and #f indicating which entries are constrainted (live).
    (define (output-constraint-string machine-var live-out)
      (define live-regs-d-str (string-join (map number->string (first live-out))))
      (define live-regs-r-str (string-join (map number->string (second live-out))))
      (format "(constraint ~a [dreg ~a] [rreg ~a] [mem-all])" 
              machine-var live-regs-d-str live-regs-r-str))

    (define (get-state init)
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

    (define/public (get-type-id type)
      (vector-member type type-id))

    (define/public (get-type-name id)
      (vector-ref type-id id))
    
    ))