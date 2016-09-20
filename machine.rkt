#lang racket

(require "inst.rkt")
(provide debug machine% get-memory-type)

(define debug #f)

(define (get-memory-type) 'mem%)

(define machine%
  (class object%
    (super-new)
    (init-field 
     ;; Required fields to be initialized when extending this class.
     [bitwidth #f]          ;; Number of bits to represnet a number
     [random-input-bits #f] ;; Number of bits to generate random inputs. Often equal to 'bit'.
     [config #f]            ;; Machine configuration such as # of regs, memory size, etc.
     [opcodes #f]           ;; A vector of opcode names.
     [nop-id #f]            ;; The index of nop in 'opcodes' vector.
     [opcode-id-to-class (make-hash)] ;; Map from opcode id to class name
     [classes-info (make-hash)]       ;; Map from class name to class info
     [argtypes-info (make-hash)]      ;; Map from arg type to arg/state type info

     ;; Fields to be set by method 'analyze-opcode'
     [opcode-pool #f]        ;; Opcodes to be considered during synthesis.
     )
    
    ;; Required methods to be implemented.
    ;; See comments at the point of method declaration in arm/arm-machine.rkt for example.
    (abstract set-config get-state clone-state update-progstate-ins-store)

    ;; Provided default methods. Can be overriden if needed.
    (public get-config window-size
	    adjust-config 
            no-assumption
            get-opcode-id get-opcode-name
            finalize-config config-exceed-limit?
            get-state-liveness display-state
	    clean-code state-eq? relaxed-state-eq?
	    update-live update-live-backward
	    analyze-opcode analyze-args 
            reset-opcode-pool 
            reset-arg-ranges
            get-arg-ranges get-arg-types
            get-constructor
            
            ;; ISA description
            define-instruction-class finalize-machine-description update-classes-pool
            define-arg-progstate-type define-progstate-type define-arg-type

            ;; For enumerative search
            get-inst-key
            get-progstate-ins-types get-progstate-outs-types
            get-progstate-ins-vals get-progstate-outs-vals
            update-progstate-ins update-progstate-ins-load update-progstate-del-mem kill-outs
            
            ;; TODO: clean-up
            get-memory-size get-live-list
            progstate->vector vector->progstate
            get-states-from-file parse-state-text
            )

    (define (get-constructor) (raise "Please implement machine:get-constructor"))

    ;; Non-context-aware window decomposition size. Set it to very high value.
    ;; Context-aware window decomposition size is set in xxx-symbolic.rkt and xxx-forwardbackward.rkt
    (define (window-size) 100)
    (define (get-config) config)
    (define (adjust-config config) config)
    (define (get-memory-size) config)
    (define (get-opcode-id opcode) (vector-member opcode opcodes))
    (define (get-opcode-name id) (vector-ref opcodes id))
    (define (no-assumption) #f)
    (define (get-state-liveness f extra) (get-state f extra))
    (define (display-state x) (pretty-display x))

    (define (finalize-config info) info)
    (define (config-exceed-limit? info)
      (> (get-memory-size info) 100))

    (define (progstate->vector x) x)
    (define (vector->progstate x) x)

    (define (parse-state-text str)
      (raise "machine:parse-state-text: override this method to read program states from file"))

    (define (get-states-from-file file)
      (define port (open-input-file file))
      (define (parse)
        (define line (read-line port))
        (if (equal? line eof)
            (list)
            (cons (parse-state-text line)
                  (parse))))
      (define ret (parse))
      (close-input-port port)
      ret)

    (define (clean-code code [prefix (vector)])
      (vector-filter-not (lambda (x) (= (inst-op x) nop-id)) code))

    (define (state-eq? state1 state2 pred)
      ;(pretty-display `(state-eq? ,state1 ,state2 ,pred))
      (cond
       [(equal? pred #t)
	(equal? state1 state2)]
       [(equal? pred #f)
	#t]
       [(number? pred)
	(for/and ([i pred]
		  [s1 state1]
		  [s2 state2])
		 (equal? s1 s2))]
       [else
	(for/and ([i pred]
		  [s1 state1]
		  [s2 state2])
		 (state-eq? s1 s2 i))]))

    (define (relaxed-state-eq? state1 state2 pred [out-loc #f])
      (state-eq? state1 state2 pred))

    ;; Deprecated
    ;; ;; range is a vector of possible arguments.
    ;; ;; live is either a list of live arguments or indicator vector.
    ;; (define (filter-live range live)
    ;;   (cond
    ;;    [(list? live)
    ;;     (vector-filter (lambda (x) (member x live)) range)]
    ;;    [(vector? live)
    ;;     (vector-filter (lambda (x) (vector-ref live x)) range)]
    ;;    [else range]))

    (define (get-live-list constraint) (progstate->vector constraint))
    
    (define (analyze-opcode prefix code postfix)
      (set! opcode-pool (range (vector-length opcodes))))

    (define (reset-opcode-pool) (void))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; instruction & arg class ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (struct instclass (opcodes pool args ins outs) #:mutable)
    (struct argtype (validfunc valid get set) #:mutable)

    (define (define-instruction-class name class-opcodes
              #:args [args '()] #:ins [ins '()] #:outs [outs '()])
      (hash-set! classes-info name (instclass class-opcodes #f (list->vector args) ins outs)))

    (define (define-arg-progstate-type name validfunc get set)
      (hash-set! argtypes-info name (argtype validfunc #f get set)))

    (define (define-progstate-type name get set)
      (hash-set! argtypes-info name (argtype #f #f get set)))

    (define (define-arg-type name validfunc)
      (hash-set! argtypes-info name (argtype f #f #f #f)))

    (define (finalize-machine-description)
      (define all-opcodes (list))
      (define classes-list (list))
      (define n 0)
      (for ([pair (hash->list classes-info)]
            [id (in-naturals)])
           (let* ([class (car pair)]
                  [info (cdr pair)]
                  [class-opcodes (instclass-opcodes info)]
                  [add-n (length class-opcodes)]
                  [pool (for/list ([i add-n]) (+ n i))]
                  )
             (set! all-opcodes (append all-opcodes class-opcodes))
             (set! classes-list (append classes-list (for/list ([i add-n]) id)))
             (set-instclass-opcodes! info pool)
             (set-instclass-pool! info pool)
             (set! n (+ n add-n))))

      (set! opcodes (list->vector all-opcodes))
      (set! opcode-id-to-class (list->vector classes-list))
      (set! nop-id (vector-member 'nop opcodes))
      )

    (define (update-classes-pool)
      (for ([class (hash-values classes-info)])
           (set-instclass-pool!
            (filter
             (lambda (opcode-id) (member opcode-id opcode-pool))
             (instclass-opcodes class)))))
    
    ;; Return types of operands given opcode-name.
    (define (get-arg-types opcode-id)
      (define class (vector-ref opcode-id-to-class opcode-id))
      (instclass-args (hash-ref classes-info class)))

    ;; Reset valid operands' ranges.
    (define (reset-arg-ranges)
      (for ([argtype-info (hash-values argtypes-info)])
           (set-argtype-valid! argtype-info ((argtype-validfunc argtype-info) config))))

    
    ;; Return valid operands' ranges given opcode-name, live-in, live-out, and mode.
    ;; opcode-name: symbol
    ;; live-in & live-out: compact format
    ;; There are 3 modes.
    ;;  1) `basic (no restriction)
    ;;  2) `no-args = ignore reigster operands. Return `var-o and `var-i for operand that is input variable and output variable respectively. This mode is only used for enumerative search.
    (define (get-arg-ranges opcode-id entry live-in
                            #:live-out [live-out #f] #:mode [mode `basic])
      
      (define class (hash-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define types (instclass-args class))
      (define ins (instclass-ins class))
      (define outs (instclass-outs class))

      ;; non-argument inputs have to be live
      (define pass #t)
      (for ([in ins])
           (unless (number? in)
                   (let ([argtype-info (hash-ref argtypes-info in)])
                     (unless ((argtype-get argtype-info) live-in)
                             (set! pass #f)))))

      (if (equal? mode `basic)
          (for/vector
           ([type types] [id (in-naturals)])
           (cond
            [(not pass) (vector)] ;; if not pass, return empty list
            [(and (member id ins) (member id outs))
             (define vals (get-arg-range-of-type type live-in))
             (get-arg-range-of-type type live-out #:vals vals)]
            [(member id ins) (get-arg-range-of-type type live-in)]
            [(member id outs) (get-arg-range-of-type type live-out)]
            [else (get-arg-range-of-type type #f)]))
          
          (for/vector
           ([type types] [id (in-naturals)])
           (cond
            [(or (member id ins) (member id outs)) type]
            [else (get-arg-range-of-type type #f)])))
      )
       

    ;; Return valid operands' range given an argument type and liveness.
    (define (get-arg-range-of-type type live #:vals [vals #f])
      (define argtype-info (hash-ref argtypes-info type))
      (unless vals (set! vals (argtype-valid argtype-info)))
      (list->vector
       (if live
           (let ([get (argtype-get argtype-info)])
             (filter (lambda (val) (get live val)) vals))
           vals))
      )
    
    ;; instruction x: e.g. add v0, v1, v2
    ;; livenss before execute inst (given live): (vector * #t #t)
    ;; liveness after execute inst (output): (vector #t #t #t)
    ;; v0 is live after executing inst, so set the first entry to #t.
    (define (update-live live my-inst)
      (define new-live (clone-state live))
      (define opcode-id (opcode my-inst))
      (define class (hash-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define types (instclass-args class))
      (define outs (instclass-outs class))

      (for ([type types] [id (in-naturals)])
           (when (member id outs)
                 (let ([argtype-info (hash-ref argtypes-info type)])
                   ((argtype-set argtype-info) new-live id #t))))

      (for ([out outs])
           (when (hash-hash-key? argtypes-info out)
                 (let ([argtype-info (hash-ref argtypes-info out)])
                   ((argtype-set argtype-info) new-live #t))))
      new-live)

    
    ;; For enumerative search
    ;; instruction x: e.g. add v0, v1, v2
    ;; liveness *after* execute inst (given live): (vector #t * *)
    ;; liveness *before* execute inst (output): (vector #f #t #t).
    ;; v1 and v2 must be live-in. v0 is not live-in.
    (define (update-live-backward live my-inst)
      (define new-live (clone-state live))
      (define opcode-id (opcode my-inst))
      (define class (hash-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define types (instclass-args class))
      (define ins (instclass-ins class))
      (define outs (instclass-outs class))

      ;; kill outs first
      (for ([type types] [id (in-naturals)])
           (when (member id outs)
                 (let ([argtype-info (hash-ref argtypes-info type)])
                   ((argtype-set argtype-info) new-live id #f))))
      (for ([out outs])
           (when (and (not (member out (list (get-memory-type))))
                      (hash-hash-key? argtypes-info out))
                 (let ([argtype-info (hash-ref argtypes-info out)])
                   ((argtype-set argtype-info) new-live #f))))
      
      ;; add live
      (for ([type types] [id (in-naturals)])
           (when (member id ins)
                 (let ([argtype-info (hash-ref argtypes-info type)])
                   ((argtype-set argtype-info) new-live id #t))))
      (for ([out outs])
           (when (hash-hash-key? argtypes-info ins)
                 (let ([argtype-info (hash-ref argtypes-info out)])
                   ((argtype-set argtype-info) new-live #t))))
      )

    ;; Analyze input code and update operands' ranges.
    (define (analyze-args prefix code postfix live-in-list live-out
                                   #:only-const [only-const #f] #:vreg [vreg 0])
      (for ([x (vector-append prefix code postfix)])
           (analyze-args-inst x)))

    (define (analyze-args-inst my-inst)
      (define opcode-id (inst-op my-inst))
      (define args (inst-args x))
      (define class (hash-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define types (instclass-args class))
      
      (for ([type types] [arg args])
           (let* ([argtype-info (hash-ref argtypes-info type)]
                  [vals (argtype-valid argtype-info)])
             (unless (member arg vals)
                     (set-argtype-valid! (cons arg vals)))))
      )
    
    ;; For building behavior-bw
    (define (get-inst-key my-inst)
      (define opcode-id (inst-op my-inst))
      (define args (inst-args x))
      (define class (hash-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define ins (instclass-ins class))
      (define outs (instclass-outs class))
      (cons
       opcode-id
       (filter (lambda (x) x)
               (for/list ([arg args] [id (in-naturals)])
                         (and (not (or (member id ins) (member id outs))) arg)))))

    (define (get-progstate-ins-types my-inst)
      (define opcode-id (inst-op my-inst))
      (define class (hash-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define ins (instclass-ins class))
      (define types (instclass-args class))

      (for/list ([in ins]) (if (number? in) (vector-ref types in) in)))

    (define (get-progstate-outs-types my-inst)
      (define opcode-id (inst-op my-inst))
      (define class (hash-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define outs (instclass-outs class))
      (define types (instclass-args class))

      (for/list ([out outs]) (if (number? out) (vector-ref types out) out)))
      
    (define (get-progstate-ins-vals my-inst state)
      (define opcode-id (inst-op my-inst))
      (define class (hash-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define ins (instclass-ins class))
      (define types (instclass-args class))

      (for/list ([in ins]
                 [type types])
                (let ([argtype-info (hash-ref argtypes-info type)])
                  (if (number? in)
                      ((argtype-get argtype-info) state type in)
                      ((argtype-get argtype-info) state type)))))
      
    (define (get-progstate-outs-vals my-inst state)
      (define opcode-id (inst-op my-inst))
      (define class (hash-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define outs (instclass-outs class))
      (define types (instclass-args class))

      (for/list ([out outs]
                 [type types])
                (let ([argtype-info (hash-ref argtypes-info type)])
                  (if (number? out)
                      ((argtype-get argtype-info) state out)
                      ((argtype-get argtype-info) state)))))

    ;; For building table & inverse interpret
    ;; Update if the entry in the state = val or the entry is #f.
    (define (update-progstate-ins my-inst vals state)
      (define new-state (clone-state state))
      (define opcode-id (inst-op my-inst))
      (define class (hash-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define ins (instclass-ins class))
      (define types (instclass-args class))
      (define pass #t)

      (for/list ([in ins] [val vals] [type types] #:break (not pass))
                (let* ([argtype-info (hash-ref argtypes-info type)]
                       [current-val 
                        (if (number? in)
                            ((argtype-get argtype-info) new-state in)
                            ((argtype-get argtype-info) new-state))])
                  (if (or (not current-val)
                          (equal? current-val val))
                      (if (number? in)
                          ((argtype-set argtype-info) new-state in val)
                          ((argtype-set argtype-info) new-state val))
                      (set! pass #f))))
      (and pass new-state))
      
    (define (update-progstate-ins-load my-inst addr state)
      (update-progstate-ins my-inst (list addr) state))

    (define (update-progstate-del-mem addr new-state)
      (let ([mem-type (hash-ref argtypes-info (get-memory-type))]
            [mem ((argtype-get mem-type) new-state)]
            [new-mem (send mem clone-all)])
        (send new-mem del addr)
        ((argtype-set mem-type) new-state new-mem)
        new-state))

    (define (kill-outs x state)
      (define new-state (clone-state state))
      (define opcode-id (inst-op my-inst))
      (define class (hash-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define outs (instclass-outs class))
      (define types (instclass-args class))

      (for/list ([out outs]
                 [type types])
                (let ([argtype-info (hash-ref argtypes-info type)])
                  (when (not (member out (list (get-memory-type))))
                        (if (number? out)
                            ((argtype-set argtype-info) state out #f)
                            ((argtype-set argtype-info) state #f)))))
      new-state)

    ))
