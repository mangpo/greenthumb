#lang racket

(require "../machine.rkt" "../inst.rkt" "../special.rkt" "../ops-racket.rkt")

(provide arm-machine% (all-defined-out))

;; define progstate macro
(define-syntax-rule
  (progstate regs memory z)
  (vector regs memory z))

(define-syntax-rule (progstate-regs x) (vector-ref x 0))
(define-syntax-rule (progstate-memory x) (vector-ref x 1))
(define-syntax-rule (progstate-z x) (vector-ref x 2))

(define-syntax-rule (set-progstate-regs! x v) (vector-set! x 0 v))
(define-syntax-rule (set-progstate-memory! x v) (vector-set! x 1 v))
(define-syntax-rule (set-progstate-z! x v) (vector-set! x 2 v))

(define arm-machine%
  (class machine%
    (super-new)
    (inherit-field bitwidth random-input-bits config
                   opcodes opcode-pool nop-id argtypes-info classes-info)
    
    (inherit define-instruction-class init-machine-description finalize-machine-description
             define-progstate-type define-arg-type
             update-progstate-ins kill-outs update-classes-pool get-opcode-name)
    (override display-state get-constructor
              progstate-structure update-progstate-ins-load update-progstate-ins-store)
    (field [cmp-inst #f])
    (init-field [inst-choice-name #f])

    (define (get-constructor) arm-machine%)

    (unless bitwidth (set! bitwidth 32))
    (set! random-input-bits bitwidth)
    
    ;; In ARM instructions, constant can have any value that can be produced by rotating an 8-bit value right by any even number of bits within a 32-bit word.

    (define perline 8)

    (define shf-inst-reg '(asr lsl lsr ror))
    (define shf-inst-imm '(asr# lsl# lsr# ror#))
    (define cond-opcodes '(eq ne ls hi cc cs lt ge))
    
    ;; Inform GreenThum that the 'op' field of 'inst' contains 3 categories of opcodes.
    ;; 0. base  1. conditional  2. optional shift
    (init-machine-description 3)

    ;; index 0 of inst-op = base opcode
    (define/public (get-base-opcode-id x)
      (vector-member x (vector-ref opcodes 0)))
    (define/public (get-base-opcode-name x)
      (vector-ref (vector-ref opcodes 0) x))
    
    ;; index 1 of inst-op = conditional opcode
    (define/public (get-cond-opcode-id x)
      (or (vector-member x (vector-ref opcodes 1)) -1))
    (define/public (get-cond-opcode-name x)
      (if (>= x 0) (vector-ref (vector-ref opcodes 1) x) '||))
    
    ;; index 2 of inst-op = optional shift
    (define/public (get-shf-opcode-id x)
      (or (vector-member x (vector-ref opcodes 2)) -1))
    (define/public (get-shf-opcode-name x)
      (if (>= x 0) (vector-ref (vector-ref opcodes 2) x) '||))

    ;;;;;;;;;;;;;;;;;;;;; program state ;;;;;;;;;;;;;;;;;;;;;;;;

    (define (progstate-structure)
      (progstate (for/vector ([i config]) 'reg)
                 ;; # of registers = config
                 ;; most block of code doesn't use all registers
                 ;; the smaller the number of registers, the faster the search
                 ;; printer needs to encode registers's name to numbers in [0,config)
                 (get-memory-type)
                 'z  ;; conditoinal flag
                 ))

    (define-progstate-type 'reg 
      #:get (lambda (state arg) (vector-ref (progstate-regs state) arg))
      #:set (lambda (state arg val) (vector-set! (progstate-regs state) arg val)))

    (define-progstate-type (get-memory-type)
      #:get (lambda (state) (progstate-memory state))
      #:set (lambda (state val) (set-progstate-memory! state val)))

    ;; z is an integer flag.
    ;; Instead of having N, C, Z, V flags, we use just z to capture all info we need.
    ;; z = 0 | eq
    ;; z = 1 | ne
    ;; z = 2 | x & y have the same sign. x < y
    ;; z = 3 | x & y have the same sign. x >= y
    ;; z = 4 | x < 0,  y >= 0
    ;; z = 5 | x >= 0, y < 0
    (define-progstate-type 'z
      #:get (lambda (state) (progstate-z state))
      #:set (lambda (state val) (set-progstate-z! state val))
      ;; At the beginning of the code we want to optimize, we set flag to -1.
      ;; This enforces the optimizer not to use conditional suffix before setting the flag.
      #:const -1
      ;; #:min 0 #:max 5
      )

    ;;;;;;;;;;;;;;;;;;;;; instruction classes ;;;;;;;;;;;;;;;;;;;;;;;;
    (define-arg-type 'reg (lambda (config) (range config)) #:progstate 'reg)
    (define-arg-type 'reg-sp (lambda (config) '()) #:progstate 'reg)
    (define-arg-type 'const (lambda (config) '(0 1)))
    (define-arg-type 'bit (lambda (config) `(0 1 ,(sub1 bitwidth))))
    (define-arg-type 'addr (lambda (config) '()))
    
    (define-instruction-class 'nop '(nop))
    
    ;; reg = reg op reg
    (define-instruction-class 'rrr-commute
      (list '(mul smmul) cond-opcodes)
      #:required '(#t #f)
      #:args '((reg reg reg) ()) #:ins '((1 2) (z 0)) #:outs '(0) #:commute '(1 . 2))

    (define-instruction-class 'rrr-commute-shf
      (list '(add and orr eor) cond-opcodes shf-inst-reg)
      #:required '(#t #f #f)
      #:args '((reg reg reg) () (reg)) #:ins '((1 2) (z 0) (3)) #:outs '(0) #:commute '(1 . 2))

    (define-instruction-class 'rrr-commute-shf-imm
      (list '(add and orr eor) cond-opcodes shf-inst-imm)
      #:required '(#t #f #t)
      #:args '((reg reg reg) () (bit)) #:ins '((1 2) (z 0) (3)) #:outs '(0) #:commute '(1 . 2))

    (define-instruction-class 'rrr
      (list '(asr lsl lsr ror sdiv udiv uxtah) cond-opcodes)
      #:required '(#t #f)
      #:args '((reg reg reg) ()) #:ins '((1 2) (z 0)) #:outs '(0))

    (define-instruction-class 'rrr-shf
      (list '(sub rsb bic orn) cond-opcodes shf-inst-reg)
      #:required '(#t #f #f)
      #:args '((reg reg reg) () (reg)) #:ins '((1 2) (z 0) (3)) #:outs '(0))

    (define-instruction-class 'rrr-shf-imm
      (list '(sub rsb bic orn) cond-opcodes shf-inst-imm)
      #:required '(#t #f #t)
      #:args '((reg reg reg) () (bit)) #:ins '((1 2) (z 0) (3)) #:outs '(0))

    ;; reg = reg op imm
    (define-instruction-class 'rri
      (list '(add# sub# rsb# and# orr# eor# bic# orn#) cond-opcodes)
      #:required '(#t #f)
      #:args '((reg reg const) ()) #:ins '((1 2) (z 0)) #:outs '(0))

    (define-instruction-class 'rrb
      (list '(asr# lsl# lsr# ror#) cond-opcodes)
      #:required '(#t #f)
      #:args '((reg reg bit) ()) #:ins '((1 2) (z 0)) #:outs '(0))

    ;; reg = reg
    (define-instruction-class 'rr-shf
      (list '(mov mvn) cond-opcodes shf-inst-reg)
      #:required '(#t #f #f)
      #:args '((reg reg) () (reg)) #:ins '((1) (z 0) (2)) #:outs '(0))

    (define-instruction-class 'rr-shf-imm
      (list '(mov mvn) cond-opcodes shf-inst-imm)
      #:required '(#t #f #t)
      #:args '((reg reg) () (bit)) #:ins '((1) (z 0) (2)) #:outs '(0))

    (define-instruction-class 'rr
      (list '(rev rev16 revsh rbit uxth uxtb clz) cond-opcodes)
      #:required '(#t #f)
      #:args '((reg reg) ()) #:ins '((1) (z 0)) #:outs '(0))

    ;; reg = imm
    (define-instruction-class 'ri1
      (list '(mov# mvn#) cond-opcodes)
      #:required '(#t #f)
      #:args '((reg const) ()) #:ins '((1) (z 0)) #:outs '(0))

    (define-instruction-class 'ri2
      (list '(movw# movt#) cond-opcodes)
      #:required '(#t #f)
      #:args '((reg const) ()) #:ins '((0 1) (z 0)) #:outs '(0))

    ;; reg = reg op reg op reg
    (define-instruction-class 'rrrr-commute
      (list '(mla smmla) cond-opcodes)
      #:required '(#t #f)
      #:args '((reg reg reg reg) ()) #:ins '((1 2 3) (z 0)) #:outs '(0) #:commute '(2 . 3))

    (define-instruction-class 'rrrr
      (list '(mls smmls) cond-opcodes)
      #:required '(#t #f)
      #:args '((reg reg reg reg) ()) #:ins '((1 2 3) (z 0)) #:outs '(0))

    (define-instruction-class 'ddrr
      (list '(smull umull) cond-opcodes)
      #:required '(#t #f)
      #:args '((reg reg reg reg) ()) #:ins '((2 3) (z 0)) #:outs '(0 1) #:commute '(2 . 3))

    (define-instruction-class 'rrii
      (list '(bfi sbfx ubfx) cond-opcodes)
      #:required '(#t #f)
      #:args '((reg reg bit bit) ()) #:ins '((1 2 3) (z 0)) #:outs '(1))

    (define-instruction-class 'rii
      (list '(bfc) cond-opcodes)
      #:required '(#t #f)
      #:args '((reg bit bit) ()) #:ins '((0 1 2) (z)) #:outs '(0))

    (define-instruction-class 'load#
      (list '(ldr#) cond-opcodes)
      #:required '(#t #f)
      #:args '((reg reg-sp addr) ()) #:ins `((1 2 ,(get-memory-type)) (z 0)) #:outs '(0))

    (define-instruction-class 'store#
      (list '(str#) cond-opcodes)
      #:required '(#t #f)
      #:args '((reg reg-sp addr) ()) #:ins '((0 1 2) (z)) #:outs `(,(get-memory-type)))

    (define-instruction-class 'cmp '(tst cmp)
      #:args '(reg reg) #:ins '(0 1) #:outs '(z))

    (define-instruction-class 'cmpi '(tst# cmp#)
      #:args '(reg const) #:ins '(0 1) #:outs '(z))

    (finalize-machine-description)
    
    (set! cmp-inst (map (lambda (x) (get-base-opcode-id x)) '(cmp tst cmp# tst#)))
    
    (define (print-line v)
      (define count 0)
      (for ([i v])
           (when (= count perline)
	     (newline)
	     (set! count 0))
           (display i)
           (display " ")
           (set! count (add1 count))
           )
      (newline)
      )

    ;; Pretty print progstate.
    (define (display-state s)
      (pretty-display "REGS:")
      (print-line (progstate-regs s))
      (pretty-display "MEMORY:")
      (pretty-display (progstate-memory s))
      (pretty-display (format "Z: ~a" (progstate-z s)))
      )

    ;; overridden method.
    ;; Remove code that behaves like nop.
    (define/override (clean-code code [prefix (vector)])
      ;; Filter out nop.
      (set! code (vector-filter-not
                  (lambda (x) (= (vector-ref (inst-op x) 0) (vector-ref nop-id 0)))
                  code))
      (define z-flag #f)
      (for ([x prefix])
           (let ([ops-vec (inst-op x)])
             (when (member (vector-ref ops-vec 0) cmp-inst)
        	   (set! z-flag #t))))
      (for/vector ([x code])
           (let ([ops-vec (inst-op x)])
             (when (member (vector-ref ops-vec 0) cmp-inst)
        	   (set! z-flag #t))
             (if (or z-flag (equal? (vector-ref ops-vec 1) -1))
        	 x
                 ;; If conditional flag is not set, you can remove condition-code-suffix.
        	 (inst (vector (vector-ref ops-vec 0) -1 (vector-ref ops-vec 2))
                       (inst-args x))))))

    ;; Analyze input code and remove some opcodes from instuction pool to be used during synthesis.
    (define/override (analyze-opcode prefix code postfix)
      (set! code (vector-append prefix code postfix))
      (define inst-choice '(nop 
                            add sub rsb 
                            add# sub# rsb#
                            mov mvn
                            mov# mvn#
                            asr lsl lsr ror
                            asr# lsl# lsr# ror#))
                                
      (when (code-has code '(clz
                             and orr eor bic orn
                             and# orr# eor# bic# orn#
                             ))
            (set! inst-choice (append inst-choice '(clz
                                                    and orr eor bic orn
                                                    and# orr# eor# bic# orn#
                                                    ))))
                                
      (when (code-has code '(movw# movt#))
            (set! inst-choice (append inst-choice '(movw# movt#))))

      (when (code-has code '(rev rev16 revsh rbit
        			 uxtah uxth uxtb
        			 bfc bfi
        			 sbfx ubfx
        			 ))
            (set! inst-choice (append inst-choice '(rev rev16 revsh rbit
        					    	uxtah uxth uxtb
        					    	bfc bfi
        					    	sbfx ubfx
        						))))
      (when (code-has code '(mul mla mls
                                 smull umull
                                 smmul smmla smmls))
            (set! inst-choice (append inst-choice '(mul mla mls
                                                        smull umull
                                                        smmul smmla smmls))))
      (when (code-has code '(sdiv udiv))
            (set! inst-choice (append inst-choice '(sdiv udiv))))
      (when (code-has code '(ldr#))
            (set! inst-choice (append inst-choice '(ldr#))))
      (when (code-has code '(str#))
            (set! inst-choice (append inst-choice '(str#))))
      (when (code-has code '(tst cmp tst# cmp#))
            (set! inst-choice (append inst-choice '(tst cmp tst# cmp#))))

      (define base-opcodes (vector opcodes 0))
      (set! inst-choice-name inst-choice)
      (when debug (pretty-display `(inst-choice ,inst-choice-name)))
      (set! inst-choice (map (lambda (x) (get-base-opcode-id x)) inst-choice))
      (set! opcode-pool (filter (lambda (x) (member (vector-ref x 0) inst-choice)) opcode-pool))
      (update-classes-pool)
      ;;(pretty-display (map (lambda (x) (get-opcode-name x)) opcode-pool))
      )

    ;; Helper function for 'analyze-opcode'.
    (define (code-has code inst-list)
      (for/or ([i code])
              (let ([opcode-name (get-base-opcode-name (vector-ref (inst-op i) 0))])
                (member opcode-name inst-list))))

    (define/override (reset-opcode-pool) 
      (set! opcode-pool (flatten (for/list ([info classes-info]) (instclass-opcodes info)))))

    ;; Analyze input code and update operands' ranges.
    (define/override (analyze-args prefix code postfix live-in live-out)
      ;; set e list to empty
      (define type-reg (hash-ref argtypes-info 'reg))
      (set-argtype-valid! type-reg (list))

      ;; collect from context
      (super analyze-args prefix (vector) postfix live-in live-out)
      (define context-reg-list (argtype-valid type-reg))
      (set-argtype-valid! type-reg (list))

      ;;; collect from code
      (super analyze-args (vector) code (vector) live-in live-out)
      (define reg-list (argtype-valid type-reg))
      
      (define type-reg-sp (hash-ref argtypes-info 'reg-sp))
      (define reg-sp-list (argtype-valid type-reg-sp))

      (define exclude (append reg-list context-reg-list reg-sp-list))
      
      ;; If there are too few regs, add one more.
      ;; But try to add one that does not use anywhere
      ;; (including prefix and postfix).
      (when (<= (length reg-list) 2)
            (let ([add-reg
                   (for/or ([i config])
                           (and (not (member i exclude)) i))])
              (when add-reg (set-argtype-valid! type-reg (cons add-reg reg-list)))))
      
      (for ([pair (hash->list argtypes-info)])
           (let ([name (car pair)]
                 [info (cdr pair)])
             (pretty-display `(ARM-ARG ,name ,(argtype-valid info)))))
      )

    ;; Inform about the order of argument for load instruction
    (define (update-progstate-ins-load my-inst addr mem state-base)
      (define op (vector-ref (inst-op my-inst) 0))
      (define opcode-name (get-base-opcode-name op))
      (define args (inst-args my-inst))
      (cond
       [(equal? 'ldr# opcode-name)
        (define offset (vector-ref args 2))
        (update-progstate-ins
         my-inst (list (finitize (- addr offset) bitwidth) offset mem) state-base)]
       [(equal? 'ldr opcode-name)
        (define fp (vector-ref (progstate-regs state-base) (vector-ref args 1)))
        (define offset (vector-ref (progstate-regs state-base) offset))

        (cond
         [fp
          (update-progstate-ins
           my-inst (list fp (finitize (- addr fp) bitwidth) mem) state-base)]
         [offset
          (update-progstate-ins
           my-inst (list (finitize (- addr offset) bitwidth) offset mem) state-base)]
         [else
          (for/list ([v (arithmetic-shift 1 bitwidth)])
                    (let ([vv (finitize v bitwidth)])
                      (update-progstate-ins
                       my-inst (list vv (finitize (- addr vv) bitwidth) mem) state-base)))])]
       [else (raise (format "update-progstate-ins-load: unknown instruction ~a" opcode-name))]))

    ;; Inform about the order of argument for store instruction
    (define (update-progstate-ins-store my-inst addr val state)
      ;; Put val before addr => arg 0 is val, arg 1 is address.
      (define op (vector-ref (inst-op my-inst) 0))
      (define opcode-name (get-base-opcode-name op))
      (define args (inst-args my-inst))
      (define offset (vector-ref args 2))
      (cond
       [(equal? 'str# opcode-name)
        (update-progstate-ins
         my-inst (list val (finitize (- addr offset) bitwidth) offset) state)]
       [(equal? 'str opcode-name)
        (define fp (vector-ref (progstate-regs state) (vector-ref args 1)))
        (set! offset (vector-ref (progstate-regs state) offset))

        (cond
         [fp
          (update-progstate-ins
           my-inst (list val fp (finitize (- addr fp) bitwidth)) state)]
         [offset
          (update-progstate-ins
           my-inst (list val (finitize (- addr offset) bitwidth) offset) state)]
         [else
          (for/list ([v (arithmetic-shift 1 bitwidth)])
                    (let ([vv (finitize v bitwidth)])
                      (update-progstate-ins
                       my-inst (list val vv (finitize (- addr vv) bitwidth)) state)))])]
       [else (raise (format "update-progstate-ins-store: unknown instruction ~a" opcode-name))]))
                          
    ))
