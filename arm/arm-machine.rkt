#lang racket

(require "../machine.rkt" "../inst.rkt" "arm-inst.rkt")

(provide arm-machine% (all-defined-out))

;; Program state representation.
(struct progstate (regs memory z fp))

(define-syntax-rule (build-vector n init)
  (let ([vec (make-vector n)])
    (for ([i (in-range n)])
	 (vector-set! vec i (init)))
    vec))

;; Macros to create init state for testing
(define-syntax default-state
  (syntax-rules (reg mem set-z set-fp)
    ((default-state machine init)
     (progstate (build-vector (send machine get-nregs) init) 
                (build-vector (send machine get-nmems) init)
		(init)
		(send machine get-fp)))

    ((default-state machine init [set-z vz] [set-fp vfp])
     (progstate (build-vector (send machine get-nregs) init) 
                (build-vector (send machine get-nmems) init)
		vz
		vfp))

    ((default-state machine init [reg (a b) ...] [mem (c d) ...] [set-z vz] [set-fp vfp])
     (let ([state (default-state machine init [set-z vz] [set-fp vfp])])
       (vector-set! (progstate-regs state) a b)
       ...
       (vector-set! (progstate-memory state) c d)
       ...
       state))

    ((default-state machine init [reg (a b) ...] [mem (c d) ...])
     (let ([state (default-state machine init)])
       (vector-set! (progstate-regs state) a b)
       ...
       (vector-set! (progstate-memory state) c d)
       ...
       state))

    ((default-state [reg valr] [mem valm])
     (progstate valr valm 0 (send machine get-fp)))
    ((default-state [mem valm] [reg valr])
     (progstate valr valm 0 (send machine get-fp)))))

(define (lam-t) #t)
(define (lam-f) #f)

;; Macros to create output state constraint
(define-syntax constraint
  (syntax-rules (all none reg mem mem-all)
    ((constraint machine all) (default-state machine lam-t [set-z #t] [set-fp #f]))

    ((constraint machine none) (default-state machine lam-f [set-z #f] [set-fp #f]))

    ((constraint machine [reg r ...] [mem-all])
     (let ([state (default-state machine lam-f [reg (r #t) ...] [mem] [set-z #f] [set-fp #f])])
       (struct-copy progstate state 
                    [memory (make-vector (send machine get-nmems) #t)])))

    ((constraint machine [reg r ...] [mem m ...])
     (default-state machine lam-f [reg (r #t) ...] [mem (m #t) ...] [set-z #f] [set-fp #f]))

    ((constraint machine [reg r ...] [mem m ...] [z vz])
     (default-state machine lam-f [reg (r #t) ...] [mem (m #t) ...] [set-z vz] [set-fp #f]))

    ((constraint [mem m ...] [reg r ...])
     (constraint [reg r ...] [mem m ...]))

    ((constraint [reg r ...] [mem m ...])
     (default-state lam-f [reg (r #t) ...] [mem (m #t) ...] [set-z #f] [set-fp #f]))
    ))

(define arm-machine%
  (class machine%
    (super-new)
    (inherit-field bit random-input-bit config
                   inst-id inst-pool
		   classes classes-filtered
		   nop-id)
    (inherit get-class-id filter-live state-eq?)
    (override set-config adjust-config get-memory-size
              get-state get-state-liveness display-state 
              parse-state-text
              progstate->vector vector->progstate
	      get-arg-types get-arg-ranges get-live-list
	      clean-code 
	      analyze-opcode analyze-args relaxed-state-eq?
	      update-live update-live-backward
              reset-arg-ranges
              get-constructor)
    (public get-shfarg-range get-nregs)

    (define (get-constructor) arm-machine%)

    (unless bit (set! bit 32))
    (set! random-input-bit bit)
    (set! nop-id 0)
    (set! inst-id '#(nop 
                     add sub rsb
                     add# sub# rsb#
                     and orr eor bic orn
                     and# orr# eor# bic# orn#
                     mov mvn
                     mov# mvn# movw# movt#
                     rev rev16 revsh rbit
                     asr lsl lsr ror
                     asr# lsl# lsr# ror#
                     mul mla mls
                     smull umull
                     smmul smmla smmls
                     sdiv udiv
		     uxtah uxth uxtb
                     bfc bfi
                     sbfx ubfx
                     clz
                     ;;ldr str
                     ldr# str#
                     tst cmp
                     tst# cmp#
                     ))

    ;; Instruction classes
    (set! classes 
          (vector '(add sub rsb
			and orr eor bic orn
			asr lsl lsr ror
                        mul
			sdiv udiv 
                        smmul 
                        uxtah
                        ) ;; rrr
			;; ldr str)
		  '(add# sub# rsb#
			 and# orr# eor# bic# orn#
			 asr# lsl# lsr# ror#) ;; rri
		  '(mov mvn 
			rev rev16 revsh rbit
			uxth uxtb
			clz) ;;rr
		  '(mov# mvn#) ;; ri
		  '(movw# movt#) ;; ri
		  '(mla mls smmla smmls) ;; rrrr
		  '(smull umull) ;; ddrr
		  '(bfi sbfx ubfx) ;; rrii
		  '(tst cmp) ;; rr
		  '(tst# cmp#) ;; ri
                  ;'(ldr# str#) ;; rri
		  ;'(bfc) ;; rii
                  ))

    ;; In ARM instructions, constant can have any value that can be produced by rotating an 8-bit value right by any even number of bits within a 32-bit word.

    (define perline 8)

    (init-field [branch-inst-id '#(beq bne j jal b jr jr jalr bal)]
                [shf-inst-id '#(nop lsl# asr lsl lsr ror asr# lsr# ror#)]
                [shf-inst-reg '(asr lsl lsr ror)]
                [shf-inst-imm '(asr# lsl# lsr# ror#)]
		[inst-with-shf '(add sub rsb and orr eor bic orn mov mvn)]
		[cond-inst-id '#(nop eq ne ls hi cc cs lt ge)]
		)

    (define nregs 5)
    (define nmems 1)
    (define fp 0)
 
    (define reg-range #f)
    (define reg-range-o #f)
    (define operand2-range #f)
    (define const-range #f)
    (define shf-range #f)
    (define bit-range #f)
    (define bit-range-no-0 #f)
    (define mem-range #f)

    (when config
          (set! nregs (first config))
          (set! nmems (second config))
          (set! fp (third config))
          (reset-arg-ranges))

    (define (get-nregs) nregs)
    (define/public (get-nmems) nmems)
    (define/public (get-fp) fp)
    (define/public (get-shf-inst-id x)
      (vector-member x shf-inst-id))
    (define/public (get-shf-inst-name x)
      (vector-ref shf-inst-id x))
    (define/public (get-cond-inst-id x)
      (vector-member x cond-inst-id))
    (define/public (get-cond-inst-name x)
      (vector-ref cond-inst-id x))

    ;; Set machine configuration and set valid operands' ranges accordingly.
    ;; info: a list of (# of registers, # of memory, stack pointer)
    (define (set-config info)
      (set! config info)
      (set! nregs (first info))
      (set! nmems (second info))
      (set! fp (third info))
      (reset-arg-ranges)
      )

    
    ;; Set valid operands' ranges.
    (define (reset-arg-ranges)
      (set! reg-range (list->vector (range nregs)))
      (set! reg-range-o (list->vector (range nregs)))
      (set! operand2-range (vector 0 1 (sub1 bit)))
      (set! const-range (vector 0 1))
      (set! shf-range (vector 1))
      (set! bit-range (vector 0 1))
      (set! bit-range-no-0 (vector 1))
      (set! mem-range (list->vector (for/list ([i nmems]) (- i fp)))))

    ;; Add additional valid operands' ranges.
    (define (update-arg-ranges op2 const bit reg reg-o mem only-const [vreg 0])
      ;; Not include mem-range
      (set! operand2-range 
            (list->vector 
             (set->list (set-union (list->set (vector->list operand2-range))
                                   op2 const
                                   ))))
      (set! shf-range 
            (list->vector 
	     (filter (lambda (x) (and (> x 0) (<= x 32)))
		     (set->list (set-union (list->set (vector->list shf-range))
					   op2)))))
      (set! const-range 
            (list->vector 
             (set->list (set-union (list->set (vector->list const-range))
                                   op2 const))))

      (set! bit-range 
            (list->vector 
	     (filter (lambda (x) (and (>= x 0) (<= x 32)))
                     (set->list (set-union (list->set (vector->list bit-range))
                                           op2 bit)))))
      (set! bit-range-no-0 
            (list->vector 
	     (filter (lambda (x) (and (> x 0) (<= x 32)))
                     (set->list (set-union (list->set
                                            (vector->list bit-range-no-0))
                                           op2 bit)))))

      (unless only-const
              (set! reg-range
                    (list->vector (append (set->list reg) 
                                          (range nregs (+ nregs vreg)))))
              (set! reg-range-o
                    (list->vector (append (set->list reg-o) 
                                          (range nregs (+ nregs vreg)))))
              (set! nregs (+ nregs vreg)) ;; Same if enum = 0
              
              (set! mem-range (list->vector (set->list mem))))

      (pretty-display `(reg-range ,reg-range))
      (pretty-display `(reg-range-o ,reg-range-o))
      (pretty-display `(mem-range ,mem-range))
      (pretty-display `(operand2-range ,operand2-range))
      (pretty-display `(const-range ,const-range))
      (pretty-display `(shf-range ,shf-range))
      (pretty-display `(bit-range ,bit-range))
      (pretty-display `(bit-range-no-0 ,bit-range-no-0))
      )

    ;; Double the memory size.
    (define (adjust-config info)
      (list (first info) (* 2 (second info)) (third info)))

    (define (get-memory-size info) (second info))

    ;; Convert live-out from compact format into progstate format.
    ;; live-out: a pair of (a list of live registers' ids, live memory)
    ;; output: a progstate object. #t elements indicate live.
    ;; (define/public (output-constraint live-out)
    ;;   ;; Registers are default to be dead.
    ;;   (define regs (make-vector nregs #f))
    ;;   ;; Memory is default to be live.
    ;;   (define memory (if (second live-out)
    ;;                      (make-vector nmems #t)
    ;;                      (make-vector nmems #f)))
    ;;   (for ([x (first live-out)])
    ;;        (vector-set! regs x #t))
    ;;   (progstate regs memory #f #f))

    ;; Get progstate when initialize each entry with the result of (init).
    ;; Used for generating random input states.
    (define (get-state init extra)
      (default-state this init 
        [set-z -1] ;; Set z (conditional flag) to -1 for a test input.
        [set-fp fp]))

    ;; Get progstate when initialize each entry with the result of (init).
    ;; Used for analyzing live-in information.
    (define (get-state-liveness init extra)
      (default-state this init 
        [set-z (init)] ;; Set z to (init) because the flag can be live.
        [set-fp fp]))

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
      (print-line (progstate-memory s))
      (pretty-display (format "Z: ~a" (progstate-z s)))
      )

    ;; Convert progstate to compact text.
    ;; (define (display-state-text pair)
    ;;   (define state (cdr pair))
    ;;   (define regs (progstate-regs state))
    ;;   (define memory (progstate-memory state))
    ;;   (define regs-str (string-join (map number->string (vector->list regs))))
    ;;   (define memory-str (string-join (map number->string (vector->list memory))))
    ;;   (pretty-display (format "~a,~a,~a,~a" regs-str memory-str 
    ;;     		      (progstate-z state) (progstate-fp state))))

    ;; Optional
    ;; Convert compact ext to progstate.
    (define (parse-state-text str)
      (define tokens (string-split str ","))
      (define regs-str (first tokens))
      (define memory-str (second tokens))
      (define regs (list->vector (map string->number (string-split regs-str))))
      (define memory (list->vector (map string->number (string-split memory-str))))
      (define z (string->number (third tokens)))
      (define fp (string->number (fourth tokens)))
      (cons #t (progstate regs memory z fp)))

    ;; Convert progstate to vector/list/pair object.
    (define (progstate->vector x)
      (and x (vector (progstate-regs x) (progstate-memory x) (progstate-z x) (progstate-fp x))))

    ;; Convert vector/list/pair object to progstate.
    (define (vector->progstate x)
      (and x (progstate (vector-ref x 0) (vector-ref x 1) (vector-ref x 2) (vector-ref x 3))))

    ;; Overridden method.
    ;; Remove code that behaves like nop.
    (define (clean-code code [prefix (vector)])
      ;; Filter out nop.
      (set! code (vector-filter-not (lambda (x) (= (inst-op x) nop-id)) code))
      (define z-flag #f)
      (define cond-type-nop (vector-member `nop cond-inst-id))
      (for ([x prefix])
	   (let ([op (inst-op x)])
	     (when (member (vector-ref inst-id op) '(tst cmp tst# cmp#))
		   (set! z-flag #t))))
      (for/vector ([x code])
	   (let ([op (inst-op x)]
		 [cond-type (inst-cond x)])
	     (when (member (vector-ref inst-id op) '(tst cmp tst# cmp#))
		   (set! z-flag #t))
	     (if (or z-flag (equal? cond-type cond-type-nop))
		 x
                 ;; If conditional flag is not set, you can remove condition-code-suffix.
		 (arm-inst op (inst-args x) (inst-shfop x) (inst-shfarg x) cond-type-nop)))))

    ;; Convert liveness information from progstate format to a compact format.
    ;; Use registers' liveness from state and memory' livenss from state2.
    (define (get-live-list state [state2 #f])
      (and state
           (let ([regs (progstate-regs state)]
                 [live-reg (list)]
                 [mem (if state2
                          (progstate-memory state2)
                          (progstate-memory state))]
                 [live-mem (list)]
                 )
             (for ([i (vector-length regs)]
                   [r regs])
                  (when (and r (vector-member i reg-range-o))
			(set! live-reg (cons i live-reg))))
             (for ([i (vector-length mem)]
                   [m mem])
                  (when (and m (vector-member (- i fp) mem-range))
			(set! live-mem (cons i live-mem))))
             (cons live-reg live-mem))))
    
    ;; Get live-out info.
    ;; live: live-in in compact format.
    ;; x: instruction.
    ;; output: live-out in compact format.
    (define (update-live live x)
      (define live-reg (car live))
      (define live-mem (cdr live))
      
      (define args (inst-args x))
      (define opcode-name (vector-ref inst-id (inst-op x)))
      (define (add-live ele lst)
	(if (member ele lst) 
	    ;; remove and add because we want the latest reg at the beginning.
	    (cons ele (remove ele lst))
	    (cons ele lst)))
      (and live
	   (cond
	    [(member opcode-name '(smull umull))
             (cons 
              (add-live (vector-ref args 0)
                        (add-live (vector-ref args 1) live-reg))
              live-mem)]
	    [(member opcode-name '(nop)) live]
            [(member opcode-name '(nop str str#))
             (cons
              live-reg
              (add-live (+ fp (vector-ref args 2)) live-mem))]
	    [else
             (cons
              (add-live (vector-ref args 0) live-reg)
              live-mem)])))

    ;; Get live-in info.
    ;; live: live-out in compact format.
    ;; x: instruction.
    ;; output: live-in in compact format.
    (define (update-live-backward live x)
      (define live-reg (car live))
      (define live-mem (cdr live))
      
      (define opcode-name (vector-ref inst-id (inst-op x)))
      (define args (inst-args x))
      (define args-type (get-arg-types opcode-name))
      (define shfop (inst-shfop x))
      (define shfarg (inst-shfarg x))
      (define cond-type (inst-cond x))

      (define (add-live ele lst)
	(if (member ele lst) 
	    ;; remove and add because we want the latest reg at the beginning.
	    (cons ele (remove ele lst))
	    (cons ele lst)))

      (for ([arg args]
	    [type args-type])
	   (cond
            ;; kill first
	    [(equal? type `reg-o) (when (= cond-type 0) (set! live-reg (remove arg live-reg)))]
	    [(equal? type `reg-i) (set! live-reg (add-live arg live-reg))]
	    [(equal? type `mem-o)
             (when (= cond-type 0) (set! live-mem (remove (+ fp arg) live-mem)))]
	    [(equal? type `mem-i)
             (set! live-mem (add-live (+ arg fp) live-mem))]
            ))

      (when (member (vector-ref shf-inst-id shfop) shf-inst-reg)
            (set! live-reg (add-live shfarg live-reg)))
      (cons live-reg live-mem))

    (define (get-arg-types opcode-name)
      (define class-id (get-class-id opcode-name))
      (cond
       [(equal? class-id 0) (vector `reg-o `reg-i `reg-i)]
       [(equal? class-id 1) (vector `reg-o `reg-i `op2)]
       [(equal? class-id 2) (vector `reg-o `reg-i)]
       [(equal? class-id 3) (vector `reg-o `const)]
       [(equal? class-id 4) (vector `reg-io `const)]
       [(equal? class-id 5) (vector `reg-o `reg-i `reg-i `reg-i)]
       [(equal? class-id 6) (vector `reg-o `reg-o `reg-i `reg-i)]
       [(equal? class-id 7) (vector `reg-o `reg-i `bit `bit-no-0)]
       [(equal? class-id 8) (vector `reg-i `reg-i)]
       [(equal? class-id 9) (vector `reg-i `const)]
       [(equal? opcode-name `ldr#) (vector `reg-o `fp `mem-i)]
       [(equal? opcode-name `str#) (vector `reg-i `fp `mem-o)]
       [(equal? opcode-name `bfc) (vector `reg-io `bit `bit-no-0)]
       [else (vector)]))
    
      
    ;; Get valid operands' ranges given opcode-name, live-in, live-out, and mode.
    ;; opcode-name: symbol
    ;; live-in & live-out: compact format
    ;; There are 3 modes.
    ;;  1) `basic (no restriction)
    ;;  2) `no-args = ignore reigster operands. Return `reg-o, `reg-i, and `reg-io for operand that is input register, output register, and input/output register respectively.
    (define (get-arg-ranges opcode-name entry live-in
                            #:live-out [live-out #f] #:mode [mode `basic])
      (define reg-i
	(if live-in
	    (filter-live reg-range (car live-in))
	    reg-range))

      (define reg-o
	(if live-out
	    (filter-live reg-range-o (car live-out))
	    reg-range-o))
      
      (define mem-i
	(if live-in
	    (filter-live mem-range (map (lambda (x) (- x fp)) (cdr live-in)))
	    mem-range))

      (define mem-o
	(if live-out
	    (filter-live mem-range (map (lambda (x) (- x fp)) (cdr live-out)))
	    mem-range))

      (define reg-io (list))
      (for ([i reg-i])
	   (when (vector-member i reg-o) (set! reg-io (cons i reg-io))))
      (set! reg-io (list->vector (reverse reg-io)))

      (for/vector 
       ([type (get-arg-types opcode-name)])
       (if (equal? mode `basic)
	   (cond
	    [(equal? type `reg-o)  reg-o]
	    [(equal? type `reg-i)  reg-i]
	    [(equal? type `reg-io) reg-io]
	    [(equal? type `op2)    operand2-range]
	    [(equal? type `const)  const-range]
	    [(equal? type `bit)    bit-range]
	    [(equal? type `bit-no-0) bit-range-no-0]
	    [(equal? type `mem-o)  mem-o]
	    [(equal? type `mem-i)  mem-i]
	    [(equal? type `fp)     (vector "fp")])
	   (cond
	    [(equal? type `reg-o)  `reg-o]
	    [(equal? type `reg-i)  `reg-i]
	    [(equal? type `reg-io) `reg-i]
	    [(equal? type `op2)    operand2-range]
	    [(equal? type `const)  const-range]
	    [(equal? type `bit)    bit-range]
	    [(equal? type `bit-no-0) bit-range-no-0]
	    [(equal? type `mem-o)    mem-range]
	    [(equal? type `mem-i)    mem-range]
	    [(equal? type `fp)     (vector "fp")]))
       ))

    ;; Get valid optinal-shift operand's range.
    (define (get-shfarg-range shfop-id live-in #:mode [mode `basic])
      (define shfop-name (vector-ref shf-inst-id shfop-id))
      (if (member shfop-name shf-inst-reg)
	  (if (equal? mode `no-args)
	      `reg-i
              (if live-in
                  (filter-live reg-range (car live-in))
                  reg-range))
	  shf-range))

    ;; Analyze input code and remove some opcodes from instuction pool to be used during synthesis.
    (define (analyze-opcode prefix code postfix)
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
      (set! inst-pool (map (lambda (x) (vector-member x inst-id)) inst-choice))
      (set! classes-filtered 
            (for/vector ([c classes])
                        (map (lambda (x) (vector-member x inst-id))
                             (filter (lambda (x) (member x inst-choice)) c))))
      (when debug
	    (pretty-display `(inst-choice ,inst-choice))
	    (pretty-display `(classes-filtered ,classes-filtered)))
      )

    ;; Helper function for 'analyze-opcode'.
    (define (code-has code inst-list)
      (for/or ([i code])
              (let ([opcode-name (vector-ref inst-id (inst-op i))])
                (member opcode-name inst-list))))

    ;; (define/override (reset-inst-pool)
    ;;   (define inst-choice '(
    ;;                  add sub rsb
    ;;                  add# sub# rsb#
    ;;                  and orr eor bic orn
    ;;                  and# orr# eor# bic# orn#
    ;;                  mov mvn
    ;;                  mov# mvn# movw# movt#
    ;;                  rev rev16 revsh rbit
    ;;                  asr lsl lsr ror
    ;;                  lsl# 
    ;;     	     asr# lsr# ror#
    ;;                  mul mla mls

    ;;                  smull umull
    ;;                  smmul smmla smmls

    ;;                  sdiv udiv
    ;;     	     uxtah uxth uxtb
    ;;                  bfc bfi
    ;;                  sbfx ubfx
    ;;                  clz
    ;;                  ldr str
    ;;                  ldr# str#
    ;;                  tst cmp
    ;;                  tst# cmp#
    ;;                  ))
                                
    ;;   (set! inst-pool (map (lambda (x) (vector-member x inst-id)) inst-choice)))

    ;; Analyze input code and update operands' ranges.
    (define (analyze-args prefix code postfix live-in-list live-out
                          #:only-const [only-const #f] #:vreg [vreg 0])
      (pretty-display "Analyze-args-int")
      (define reg-set (set))
      (define mem-set (set))
      (define op2-set (set))
      (define const-set (set))
      (define bit-set (set))
      (for ([x (vector-append prefix postfix)])
           (let ([ans (analyze-args-inst x)])
	     ;; (print-struct-inst x)
	     ;; (pretty-display `(const ,(set->list (first ans)) ,(set->list (second ans)) ,(set->list (third ans))))
             (set! op2-set (set-union op2-set (first ans)))
             (set! const-set (set-union const-set (second ans)))
             (set! bit-set (set-union bit-set (third ans)))
	     ))
      (for ([x code])
           (let ([ans (analyze-args-inst x)])
	     ;; (print-struct-inst x)
	     ;; (pretty-display `(const ,(set->list (first ans)) ,(set->list (second ans)) ,(set->list (third ans))))
             (set! op2-set (set-union op2-set (first ans)))
             (set! const-set (set-union const-set (second ans)))
             (set! bit-set (set-union bit-set (third ans)))
             (set! reg-set (set-union reg-set (fourth ans)))
             (set! mem-set (set-union mem-set (fifth ans)))
	     ))

      (define regs-in (list->set (car live-in-list)))

      ;; If the input code uses 2 reigsters or less, we include an extra register to be used in synthesized code.
      (when (<= (set-count reg-set) 2)
            (define reg
              (for/or ([live (progstate-regs live-out)]
                       [r (in-naturals)])
                      (and (not live) (not (set-member? reg-set r)) r)))
            (when reg (set! reg-set (set-add reg-set reg))))
                 
      (update-arg-ranges op2-set const-set bit-set
                         (set-union reg-set regs-in) reg-set
                         mem-set only-const vreg))

    (define (analyze-args-inst x)
      (define opcode (vector-ref inst-id (inst-op x)))
      (define args (inst-args x))
      (define shf (member opcode inst-with-shf))
      (define shfop (and shf (inst-shfop x) (vector-ref shf-inst-id (inst-shfop x))))
      (define shfarg (and shf (inst-shfarg x)))
      ;; (pretty-display `(shf ,shf ,shfop))

      (define reg-set 
	(if (and shfop (member shfop shf-inst-reg))
	    (set shfarg)
	    (set)))
      (define mem-set (set))
      (define const-set (set))
      (define bit-set (set))	
      (define op2-set
	(if (and shfop (member shfop shf-inst-imm))
	    (set shfarg)
	    (set)))

      (for ([arg args]
	    [type (get-arg-types opcode)])
	   (cond
	    [(member type '(reg-o reg-i reg-io)) (set! reg-set (set-add reg-set arg))]
	    [(member type '(bit bit-no-0))       (set! bit-set (set-add bit-set arg))]
	    [(equal? type `op2)   (set! op2-set (set-add op2-set arg))]
	    [(equal? type `const) (set! const-set (set-add const-set arg))]
	    [(member type '(mem-o mem-i)) (set! mem-set (set-add mem-set arg))]))
      (list op2-set const-set bit-set reg-set mem-set))

    ;; Check if state1 and state 2 are equal with relaxed condition on the register naming.
    ;; If all expected values in state1 appears in state2, then return #t.
    (define (relaxed-state-eq? state1 state2 pred [out-loc #f])
      (define regs-pred (vector-ref pred 0))
      (define regs1 (vector-ref state1 0))
      (define regs2 (vector-ref state2 0))

      (define ret
	(and
	 ;; Use normal state-eq? for everything else that is not regs.
	 (for/and ([i (range 1 (vector-length state1))])
		  (state-eq? (vector-ref state1 i) (vector-ref state2 i) (vector-ref pred i)))
	 ;; Check regs.
	 (for/and ([i regs-pred]
		   [r regs1])
		  (or (not i) (vector-member r regs2)))))

      (when 
       (and ret out-loc)
       (define special (vector-ref regs2 out-loc))
       
       (when special
	     (define okay #f)
	     (for ([i regs-pred]
		   [r regs1])
		  (when (and i (= r special)) (set! okay #t)))
	     (unless okay (set! ret #f))))
      
      ret)
                          
    ))
