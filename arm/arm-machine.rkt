#lang racket

(require "../machine.rkt" "../ast.rkt" "arm-ast.rkt")

(provide arm-machine% (all-defined-out))

(struct progstate (regs memory z fp))
(struct progstate+ progstate (extra))

(define-syntax-rule (build-vector n init)
  (let ([vec (make-vector n)])
    (for ([i (in-range n)])
	 (vector-set! vec i (init)))
    vec))

;; TODO: progstate includes Z flag

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
    (inherit-field bit random-input-bit inst-id inst-pool
		   classes classes-len classes-filtered
		   perline nop-id)
    (inherit print-line get-class-id filter-live update-live)
    (override set-config get-config set-config-string
              adjust-config finalize-config config-exceed-limit?
              get-state get-state-liveness display-state
              output-constraint-string
              display-state-text parse-state-text
              progstate->vector vector->progstate
	      get-arg-ranges get-operand-live
	      window-size clean-code analyze-opcode analyze-args)
    (public get-shfarg-range)

    (set! bit 32)
    (set! random-input-bit 32)
    (set! nop-id 0)
    (set! inst-id '#(nop 
                     add sub rsb
                     add# sub# rsb#
                     and orr eor bic orn
                     and# orr# eor# bic# orn#
                     mov mvn
                     mov# mvn# movw# movt#
                     rev rev16 revsh rbit
                     asr lsl lsr
                     asr# lsl# lsr#
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
			asr lsl lsr 
                        mul
			sdiv udiv 
                        smmul 
                        uxtah
                        ) ;; rrr
			;; ldr str)
		  '(add# sub# rsb#
			 and# orr# eor# bic# orn#
			 asr# lsl# lsr#) ;; rri
		  '(mov mvn 
			rev rev16 revsh rbit
			uxth uxtb
			clz
                        tst cmp) ;;rr
		  '(mov# mvn# movw# movt# tst# cmp#) ;; ri
		  '(mla mls smmla smmls smull umull
                        ) ;; rrrr
		  '(bfi sbfx ubfx) ;; rrii
                  '(ldr# str#) ;; rri
		  ;'(bfc) ;; rii
                  ))

;; In ARM instructions, constant can have any value that can be produced by rotating an 8-bit value right by any even number of bits within a 32-bit word.

    (set! classes-len (vector-length classes))
    (set! perline 8)

    (init-field [branch-inst-id '#(beq bne j jal b jr jr jalr bal)]
                [shf-inst-id '#(nop asr lsl lsr asr# lsl# lsr#)]
		[inst-with-shf '(add sub rsb and orr eor bic orn mov mvn)]
		[cond-inst-id '#(nop eq ne ls hi cc cs)]
		)

    (define nregs 5)
    (define nmems 1)
    (define fp 0)
 
    (define reg-range #f)
    (define operand2-range #f)
    (define const-range #f)
    (define shf-range #f)
    (define bit-range #f)
    (define bit-range-no-0 #f)
    (define mem-range #f)

    (define/public (get-nregs) nregs)
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

    (define (window-size) 100) ;;32

    (define (get-config)
      (list nregs nmems fp))

    ;; info: (list nregs nmem)
    (define (set-config info)
      (set! nregs (first info))
      (set! nmems (second info))
      (set! fp (third info))
      
      (set! reg-range (list->vector (range nregs)))
      (set! operand2-range (vector 0 1))
      ;; (list->vector
      ;;  (append (range bit) (list #x3f #xff0000 #xff00 (- #xff000000) (- #x80000000)))))

      (set! const-range (vector 0 1)) ;; 0-7, 31
      ;; (list->vector
      ;;  (append (range 17) (list (sub1 bit) 
      ;;                           #x1111 #x3333 #x5555 #xaaaa #xcccc
      ;;                           #xf0f0 #x0f0f #x3f 
      ;;   			#xffff #xaaab #x2aaa #xfff4))))
      
      (set! shf-range (vector 1))
      (set! bit-range (vector 0 1))
      (set! bit-range-no-0 (vector 1))
      (set! mem-range (list->vector (for/list ([i nmems]) (- i fp))))
      )

    (define (update-arg-ranges op2 const bit reg mem only-const)
      ;;(pretty-display `(add-constants ,l))
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
	     (set->list (set-union (list->set (vector->list bit-range))
				   bit))))
      (set! bit-range-no-0 
            (list->vector 
	     (remove* (list 0)
		      (set->list (set-union (list->set (vector->list bit-range-no-0))
					    bit)))))

      (unless only-const
              (set! reg-range (list->vector (set->list reg)))
              (set! mem-range (list->vector (set->list mem))))

      (pretty-display `(reg-range ,reg-range))
      (pretty-display `(mem-range ,mem-range))
      (pretty-display `(operand2-range ,operand2-range))
      (pretty-display `(const-range ,const-range))
      (pretty-display `(shf-range ,shf-range))
      (pretty-display `(bit-range ,bit-range))
      (pretty-display `(bit-range-no-0 ,bit-range-no-0))
      )

    ;; info: (list nregs nmem)
    (define (set-config-string info)
      (format "(list ~a ~a ~a)" 
              (first info) (second info) (third info)))

    (define (adjust-config info)
      ;; Double the memory size
      (list (first info) (* 2 (second info)) (third info)))

    (define (finalize-config info) info)

    (define (config-exceed-limit? info)
      ;; Memory size > 1000
      (> (second info) 1000))

    ;; live-out: a list of live registers' ids, same format is the output of (select-code) and (combine-live-out)
    ;; output: output constraint corresponding to live-out in string. When executing, the expression is evaluated to a progstate with #t and #f indicating which entries are constrainted (live).
    (define (output-constraint-string machine-var live-out)
      (cond
       [(first live-out)
        (define live-regs-str (string-join (map number->string (first live-out))))
        (define live-mem (second live-out))
        (if live-mem
            (format "(constraint ~a [reg ~a] [mem-all])" machine-var live-regs-str)
            (format "(constraint ~a [reg ~a] [mem])" machine-var live-regs-str))]
       [else #f]))

    ;; live-out: a list of live registers' ids
    ;; output: a progstate object. #t elements indicate live.
    (define/public (output-constraint live-out)
      ;; Registers are default to be dead.
      (define regs (make-vector nregs #f))
      ;; Memory is default to be live.
      (define memory (if (second live-out)
                         (make-vector nmems #t)
                         (make-vector nmems #f)))
      (for ([x (first live-out)])
           (vector-set! regs x #t))
      (progstate regs memory #f #f))

    (define (get-state init extra)
      (default-state this init [set-z -1] [set-fp fp]))

    (define (get-state-liveness init extra)
      (default-state this init))

    ;; Pretty print functions
    (define (display-state s)
      (pretty-display "REGS:")
      (print-line (progstate-regs s))
      (pretty-display "MEMORY:")
      (print-line (progstate-memory s))
      (pretty-display (format "Z: ~a" (progstate-z s)))
      )

    (define (no-assumption)
      #f)

    (define (display-state-text pair)
      (define state (cdr pair))
      (define regs (progstate-regs state))
      (define memory (progstate-memory state))
      (define regs-str (string-join (map number->string (vector->list regs))))
      (define memory-str (string-join (map number->string (vector->list memory))))
      (pretty-display (format "~a,~a,~a,~a" regs-str memory-str 
			      (progstate-z state) (progstate-fp state))))

    (define (parse-state-text str)
      (define tokens (string-split str ","))
      (define regs-str (first tokens))
      (define memory-str (second tokens))
      (define regs (list->vector (map string->number (string-split regs-str))))
      (define memory (list->vector (map string->number (string-split memory-str))))
      (define z (string->number (third tokens)))
      (define fp (string->number (fourth tokens)))
      (cons #t (progstate regs memory z fp)))

    (define (progstate->vector x)
      (vector (progstate-regs x) (progstate-memory x) (progstate-z x) (progstate-fp x)))

    (define (vector->progstate x)
      (progstate (vector-ref x 0) (vector-ref x 1) (vector-ref x 2) (vector-ref x 3)))

    (define (clean-code code [prefix (vector)])
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
		 (arm-inst op (inst-args x) (inst-shfop x) (inst-shfarg x) cond-type-nop)))))


    (define (get-operand-live state)
      (and state
           (let ([regs (progstate-regs state)]
                 [live (list)])
             (for ([i (vector-length regs)]
                   [r regs])
                  (when r (set! live (cons i live))))
             live)))
    
    ;; nargs, ranges
    (define (get-arg-ranges opcode-name entry live-in)
      (define-syntax-rule (reg)
        (filter-live reg-range live-in))
      (define class-id (get-class-id opcode-name))
      ;(pretty-display `(get-arg-ranges ,opcode-name ,class-id ,live-in))
      (cond
       [(equal? class-id 0) (vector reg-range (reg) (reg))]
       [(equal? class-id 1) (vector reg-range (reg) operand2-range)]
       ;[(equal? class-id 2) (vector reg-range (reg) bit-range)]
       [(equal? class-id 2) (vector reg-range (reg))]
       [(equal? class-id 3) (vector reg-range const-range)]
       [(equal? class-id 4) (vector reg-range reg-range (reg) (reg))]
       [(equal? class-id 5) (vector reg-range (reg) bit-range bit-range-no-0)]
       [(equal? class-id 6) (vector reg-range (vector "fp") mem-range)]
       [(equal? opcode-name `bfc) (vector (reg) bit-range bit-range-no-0)]
       [else (vector)]))

    (define (get-shfarg-range shfop-id live-in)
      (define shfop-name (vector-ref shf-inst-id shfop-id))
      (if (member shfop-name '(asr lsr lsl)) (filter-live reg-range live-in) shf-range))

    (define (code-has code inst-list)
      (for/or ([i code])
              (let ([opcode-name (vector-ref inst-id (inst-op i))])
                (member opcode-name inst-list))))

    (define (analyze-opcode prefix code postfix)
      (set! code (vector-append prefix code postfix))
      (define inst-choice '(nop))
      (when (code-has code '(add sub rsb 
				 add# sub# rsb#
				 and orr eor bic orn
				 and# orr# eor# bic# orn#
				 mov mvn
				 mov# mvn# movw# movt#
				 rev rev16 revsh rbit
				 asr lsl lsr
				 asr# lsl# lsr#
				 uxtah uxth uxtb
				 bfc bfi
				 sbfx ubfx
				 clz))
            (set! inst-choice (append inst-choice '(add sub rsb 
							add# 
							sub# rsb#
							and orr eor bic orn
							and# orr# eor# bic# orn#
							mov mvn
							mov# mvn# movw# movt#
							rev rev16 revsh rbit
							asr lsl lsr
							asr# lsl# lsr#
							uxtah uxth uxtb
							bfc bfi
							sbfx ubfx
							clz
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
      (when #t
	    (pretty-display `(inst-choice ,inst-choice))
	    (pretty-display `(classes-filtered ,classes-filtered)))
      )


    (define (analyze-args-inst x) ;; TODO: move this to machine.rkt
      (define opcode (vector-ref inst-id (inst-op x)))
      (define args (inst-args x))
      (define class-id (get-class-id opcode))
      (define shf (member opcode inst-with-shf))
      (define shfop (and shf (inst-shfop x) (vector-ref shf-inst-id (inst-shfop x))))
      (define shfarg (and shf (inst-shfarg x)))
      ;; (pretty-display `(shf ,shf ,shfop))

      (define-syntax-rule (collect x ...)
        (collect-main (list x ...)))

      (define (collect-main fs)
	(define reg-set (set))
	(define mem-set (set))
        (define const-set (set))
        (define bit-set (set))
	(define op2-set
	  (if (and shfop (not (equal? shfop `nop)))
	      (set shfarg)
	      (set)))

        (for ([f fs] 
              [arg args])
             (cond
              [(equal? f `reg) (set! reg-set (set-add reg-set arg))]
              [(equal? f `op2) (set! op2-set (set-add op2-set arg))]
              [(equal? f `const) (set! const-set (set-add const-set arg))]
              [(equal? f `bit) (set! bit-set (set-add bit-set arg))]
              [(equal? f `mem) (set! mem-set (set-add mem-set arg))]
	      ))
        (list op2-set const-set bit-set reg-set mem-set))

      (define reg `reg)
      (define mem `mem)
      (define bit `bit)
      (define op2 `op2) 
      (define const `const)

      (cond
       [(equal? class-id 0) (collect reg reg reg)]
       [(equal? class-id 1) (collect reg reg op2)]
       [(equal? class-id 2) (collect reg reg)]
       [(equal? class-id 3) (collect reg const)]
       [(equal? class-id 4) (collect reg reg reg reg)]
       [(equal? class-id 5) (collect reg reg bit bit)]
       [(equal? class-id 6) (collect reg #f mem)]
       [(member opcode '(bfc)) (collect reg bit bit)]
       [(equal? opcode `nop) (cons (list) (list))]
       [else (raise (format "decode-inst: undefined for ~a" opcode))]))

    (define (analyze-args prefix code postfix #:only-const [only-const #f])
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
      (update-arg-ranges op2-set const-set bit-set reg-set mem-set only-const))
                          
    ))
