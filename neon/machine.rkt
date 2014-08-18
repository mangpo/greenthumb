#lang racket

(provide bit nregs-d nregs-r
         inst-id type-id get-class-id
         default-state constraint 
         config-adjust config-exceed-limit?
         ;; get rid of the ones below
         output-constraint-string
         set-machine-config set-machine-config-string
         (all-defined-out))

(struct progstate (dregs rregs memory))

(define debug #f)
(define bit 32)
(define nregs-d 10)
(define nregs-r 4)
(define nmems 4)

;; info: (list nregs nmem)
(define (set-machine-config info)
  (set! nregs-d (first info))
  (set! nregs-r (second info))
  (set! nmems (third info)))

;; info: (list nregs nmem)
(define (set-machine-config-string info)
  (format "(set-machine-config (list ~a ~a ~a))" (first info) (second info) (third info)))

(define (config-adjust info)
  ;; Double the memory size
  (list (first info) (second info) (* 2 (third info))))

(define (config-exceed-limit? info)
  ;; Memory size > 1000
  (> (third info) 1000))

;; live-out: a list of live registers, same format is the output of (select-code) and (combine-live-out)
;; output: output constraint corresponding to live-out in string. When executing, the expression is evaluated to a progstate with #t and #f indicating which entries are constrainted (live).
(define (output-constraint-string live-out)
  (define live-regs-d-str (string-join (map number->string (first live-out))))
  (define live-regs-r-str (string-join (map number->string (second live-out))))
  (format "(constraint [dreg ~a] [rreg ~a] [mem-all])" live-regs-d-str live-regs-r-str))

(define inst-id '#(nop
                   vld1 vld2 ;vld3 vld4
                   vld1! vld2! ;vld3! vld4!
                   vext#
                   vmla vmla# vmlal vmlal#
                   vmov vmovi ;vmovl vmovn vqmovn
                   ;vmvn vmvni
                   vand vandi))

(define type-id '#(s u i))

;; TODO: move to stochastic-support
(define classes
  (vector '(vld1 vld1!)
          '(vld2 vld2!)
          ;; '(vmla vand)
          ;; '(vmla# vext#)
          ;; '(vmlal)
          ;; '(vmlal#)
          ;; '(vmov)
          '(vmovi vandi)))

(define classes-len (vector-length classes))

(define (get-class-id x)
  (define id #f)
  (for ([i (in-range classes-len)])
       (when (member x (vector-ref classes i))
             (set! id i)))
  id)

(define-syntax-rule (make-vec n init)
  (let ([vec (make-vector n)])
    (for ([i (in-range n)])
	 (vector-set! vec i init))
    vec))

(define-syntax default-state
  (syntax-rules (rreg dreg mem)
    ((default-state init)
     (progstate (make-vec (* 8 nregs-d) init) 
                (make-vec nregs-r init) 
                (make-vec (* 8 nmems) init)))

    ((default-state init [dreg (a b) ...] [rreg (c d) ...] [mem (e f) ...])
     (let* ([state (default-state init)]
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

;; Macros to create output state constraint
(define-syntax constraint
  (syntax-rules (all none dreg rreg mem mem-all)
    ((constraint all) (default-state #t))

    ((constraint none) (default-state #f))

    ((constraint [dreg d ...] [rreg r ...] [mem-all])
     (let* ([state (progstate (make-vec (* 8 nregs-d) #f) 
                              (make-vec nregs-r #f) 
                              (make-vec (* 8 nmems) #t))]
            [dregs (progstate-dregs state)]
            [rregs (progstate-rregs state)])
       (for ([i 8]) (vector-set! dregs (+ (* 8 d) i) #t))
       ...
       (vector-set! rregs r #t)
       ...
       state))
    ))

(define (display-state s)
  (pretty-display "DREGS:")
  (print-line (progstate-dregs s))
  (pretty-display "RREGS:")
  (print-line (progstate-rregs s))
  (pretty-display "MEMORY:")
  (print-line (progstate-memory s)))

(define (print-line v)
  (define count 0)
  (for ([i v])
       (when (= count 8)
	     (newline)
	     (set! count 0))
       (display i)
       (display " ")
       (set! count (add1 count))
       )
  (newline)
  )

;; Macros to create input state assumption
(define-syntax-rule (no-assumption)
  #f)