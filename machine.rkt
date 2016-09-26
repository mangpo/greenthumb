#lang racket

(require "inst.rkt" "memory-rosette.rkt" "queue-rosette.rkt")
(provide (all-defined-out))

(define debug #f)

(define (get-memory-type) 'mem%)
(define (get-queue-type) 'queue%)
(struct instclass (opcodes pool args ins outs commute) #:mutable)
(struct argtype (validfunc valid) #:mutable)
(struct statetype (get set min max const))

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
     [classes-info (list)]            ;; Store classes' info
     [argtypes-info (make-hash)]      ;; Map from arg type to arg type info
     [statetypes-info (make-hash)]    ;; Map from state type to state type info

     ;; Fields to be set by method 'analyze-opcode'
     [opcode-pool #f]        ;; Opcodes to be considered during synthesis.
     [program-state #f]
     )
    
    ;; Required methods to be implemented.
    ;; See comments at the point of method declaration in arm/arm-machine.rkt for example.
    (abstract set-config 
              update-progstate-ins-load update-progstate-ins-store
              progstate-structure)

    ;; Provided default methods. Can be overriden if needed.
    (public
     ;; ISA description
     define-instruction-class finalize-machine-description
     define-progstate-type define-arg-type

     ;; Search configuration
     window-size
     get-config adjust-config finalize-config config-exceed-limit?
     get-constructor
     
     ;; Search helper functions
     no-assumption clean-code 
     get-state clone-state get-state-liveness display-state state-eq? relaxed-state-eq?
     get-opcode-id get-opcode-name

     ;; For stochastic & enumerative search
     update-live update-live-backward
     reset-opcode-pool get-valid-opcode-pool update-classes-pool
     reset-arg-ranges
     analyze-opcode analyze-args 
     get-arg-ranges get-arg-types get-class-opcodes

     ;; For enumerative search
     get-inst-key
     get-progstate-ins-types get-progstate-outs-types
     get-progstate-ins-vals get-progstate-outs-vals
     get-all-progstate-types get-progstate-type-min-max-const
     update-progstate-ins update-progstate-del-mem kill-outs
     is-cannonical
     
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
      (set! opcode-pool (range (vector-length opcodes)))
      (update-classes-pool))

    (define (reset-opcode-pool) (void))

    (define (get-state init [extra #f])
      (define progstate (progstate-structure))

      (define (inner x)
        (cond
         [(equal? x (get-memory-type)) (new memory-rosette% [get-fresh-val init])]
         [(equal? x (get-queue-type)) (new queue-rosette% [get-fresh-val init])]
         [(symbol? x)
          (define info (hash-ref statetypes-info x))
          (init #:min (statetype-min info) #:max (statetype-max info) #:const (statetype-const info))]
         [(vector? x) (for/vector ([xi x]) (inner xi))]
         [(list? x) (for/list ([xi x]) (inner xi))]
         [(pair? x) (cons (inner (car x)) (inner (cdr x)))]
         [else (raise "Program state uses unknown data strucutures (beyound vector, list, and pair)")]
         ))
      (inner progstate))

    (define (clone-state state)
      (define (inner x)
        (cond
         [(or (number? x) (boolean? x)) x]
         [(vector? x) (for/vector ([xi x]) (inner xi))]
         [(list? x) (for/list ([xi x]) (inner xi))]
         [(pair? x) (cons (inner (car x)) (inner (cdr x)))]
         [else x]))
      (inner state))

    (define (get-valid-opcode-pool index n live-in)
      (flatten
       (for/list
        ([class classes-info])
        (let ([pass #t])
          ;; check that all its inputs are live
          (when
           live-in
           (for ([in (instclass-ins class)] #:break (not pass))
                (unless (number? in)
                        (let ([info (hash-ref statetypes-info in)])
                          (unless ((statetype-get info) live-in)
                                  (set! pass #f))))))
          (if pass
              (instclass-opcodes class)
              (list))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; instruction & arg class ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (define-instruction-class name class-opcodes
              #:args [args '()] #:ins [ins '()] #:outs [outs '()] #:commute [commute #f])
      (for ([arg args])
           (unless (hash-has-key? argtypes-info arg)
                   (raise (format ("Undefined argument type ~a in 'args'" arg)))))
      (for ([in ins])
           (unless (or (number? in) (hash-has-key? statetypes-info in))
                   (raise (format ("Undefined program state type ~a in 'ins'" in)))))
      (for ([out outs])
           (unless (or (number? out) (hash-has-key? statetypes-info out))
                   (raise (format ("Undefined program state type ~a in 'outs'" out)))))
      ;; filter out an entry that is not a part of program state (get & set = #f)
      (define (pred x)
        (if (number? x)
            (hash-has-key? statetypes-info (list-ref args x))
            (hash-has-key? statetypes-info x)))
      (set! ins (filter pred ins))
      (set! outs (filter pred outs))
      
      (set! classes-info
            (cons (instclass class-opcodes #f (list->vector args) ins outs commute)
                  classes-info))
      
      ;;(pretty-display (format "[DEFINE] class=~a | args=~a ins=~a outs=~a" name args ins outs))
      )

    (define (define-progstate-type name #:get [get #f] #:set [set #f]
              #:min [min #f] #:max [max #f] #:const [const #f])
      (hash-set! statetypes-info name (statetype get set min max const)))

    (define (define-arg-type name validfunc)
      (hash-set! argtypes-info name (argtype validfunc #f)))

    (define (finalize-machine-description)
      (define all-opcodes (list))
      (define classes-list (list))
      (define n 0)
      (for ([info classes-info]
            [id (in-naturals)])
           (let* ([class-opcodes (instclass-opcodes info)]
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
      (set! classes-info (list->vector classes-info))

      ;; (pretty-display `(opcodes ,opcodes))
      ;; (pretty-display `(opcode-id-to-class ,opcode-id-to-class))
      ;; (pretty-display `(nop-id ,nop-id))
      )

    (define (update-classes-pool)
      (for ([class classes-info])
           (set-instclass-pool!
            class
            (filter
             (lambda (opcode-id) (member opcode-id opcode-pool))
             (instclass-opcodes class)))))

    (define (get-class-opcodes opcode-id)
      (instclass-pool (vector-ref classes-info (vector-ref opcode-id-to-class opcode-id))))
    
    ;; Return types of operands given opcode-name.
    (define (get-arg-types opcode-id)
      (instclass-args (vector-ref classes-info (vector-ref opcode-id-to-class opcode-id))))

    ;; Reset valid operands' ranges.
    (define (reset-arg-ranges)
      (for ([pair (hash->list argtypes-info)])
           (let ([name (car pair)]
                 [argtype-info (cdr pair)])
             (when (argtype-validfunc argtype-info)
                   (set-argtype-valid! argtype-info ((argtype-validfunc argtype-info) config))))))

    
    ;; Return valid operands' ranges given opcode-name, live-in, live-out, and mode.
    ;; Return #f if the given opcode is not a valid instruction given live-in and live-out.
    ;; opcode-name: symbol
    ;; live-in & live-out: compact format
    ;; There are 3 modes.
    ;;  1) `basic (no restriction)
    ;;  2) `no-args = ignore reigster operands. Return `var-o and `var-i for operand that is input variable and output variable respectively. This mode is only used for enumerative search.
    (define (get-arg-ranges opcode-id entry live-in
                            #:live-out [live-out #f] #:mode [mode `basic])
      (define class (vector-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define types (instclass-args class))
      (define ins (instclass-ins class))
      (define outs (instclass-outs class))

      ;; non-argument inputs have to be live
      (define pass #t)
      (when live-in
            (for ([in ins])
                 (unless (number? in)
                         (let ([info (hash-ref statetypes-info in)])
                           (unless ((statetype-get info) live-in)
                                   (set! pass #f))))))

      (and pass
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
                 [else (get-arg-range-of-type type #f)]))))
      )
       

    ;; Return valid operands' range given an argument type and liveness.
    (define (get-arg-range-of-type type live #:vals [vals #f])
      (define argtype-info (hash-ref argtypes-info type))
      (unless vals (set! vals (argtype-valid argtype-info)))
      (list->vector
       (if live
           (let ([get (statetype-get (hash-ref statetypes-info type))])
             (filter (lambda (val) (get live val)) vals))
           vals))
      )

    (define (get-all-progstate-types) (hash-keys statetypes-info))

    (define (get-progstate-type-min-max-const type)
      (define info (hash-ref statetypes-info type))
      (values (statetype-min info) (statetype-max info) (statetype-const info)))
    
    ;; instruction x: e.g. add v0, v1, v2
    ;; livenss before execute inst (given live): (vector * #t #t)
    ;; liveness after execute inst (output): (vector #t #t #t)
    ;; v0 is live after executing inst, so set the first entry to #t.
    (define (update-live live my-inst)
      (define new-live (clone-state live))
      (define opcode-id (inst-op my-inst))
      (define args (inst-args my-inst))
      (define class (vector-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define types (instclass-args class))
      (define outs (instclass-outs class))

      (for ([type types] [id (in-naturals)] [arg args])
           (when (member id outs)
                 (let ([info (hash-ref statetypes-info type)])
                   ((statetype-set info) new-live arg #t))))

      (for ([out outs])
           (when (hash-has-key? statetypes-info out)
                 (let ([info (hash-ref statetypes-info out)])
                   ((statetype-set info) new-live #t))))
      new-live)

    
    ;; For enumerative search
    ;; instruction x: e.g. add v0, v1, v2
    ;; liveness *after* execute inst (given live): (vector #t * *)
    ;; liveness *before* execute inst (output): (vector #f #t #t).
    ;; v1 and v2 must be live-in. v0 is not live-in.
    (define (update-live-backward live my-inst)
      (define new-live (clone-state live))
      (define opcode-id (inst-op my-inst))
      (define args (inst-args my-inst))
      (define class (vector-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define types (instclass-args class))
      (define ins (instclass-ins class))
      (define outs (instclass-outs class))

      ;; kill outs first
      (for ([type types] [id (in-naturals)] [arg args])
           (when (member id outs)
                 (let ([info (hash-ref statetypes-info type)])
                   ((statetype-set info) new-live arg #f))))
      (for ([out outs])
           (when (and (not (member out (list (get-memory-type))))
                      (hash-has-key? statetypes-info out))
                 (let ([info (hash-ref statetypes-info out)])
                   ((statetype-set info) new-live #f))))
      
      ;; add live
      (for ([type types] [id (in-naturals)] [arg args])
           (when (member id ins)
                 (let ([info (hash-ref statetypes-info type)])
                   ((statetype-set info) new-live arg #t))))
      (for ([in ins])
           (when (hash-has-key? statetypes-info in)
                 (let ([info (hash-ref statetypes-info in)])
                   ((statetype-set info) new-live #t))))
      new-live)

    ;; Analyze input code and update operands' ranges.
    (define (analyze-args prefix code postfix live-in-list live-out)
      (for ([x (vector-append prefix code postfix)])
           (analyze-args-inst x)))

    (define (analyze-args-inst my-inst)
      (define opcode-id (inst-op my-inst))
      (define args (inst-args my-inst))
      (define class (vector-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define types (instclass-args class))
      
      (for ([type types] [arg args])
           (let* ([argtype-info (hash-ref argtypes-info type)]
                  [vals (argtype-valid argtype-info)])
             (unless (member arg vals)
                     (set-argtype-valid! argtype-info (cons arg vals)))))
      )
    
    ;; For building behavior-bw
    (define (get-inst-key my-inst)
      (define opcode-id (inst-op my-inst))
      (define args (inst-args my-inst))
      (define class (vector-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define ins (instclass-ins class))
      (define outs (instclass-outs class))
      (cons
       opcode-id
       (filter (lambda (x) x)
               (for/list ([arg args] [id (in-naturals)])
                         (and (not (or (member id ins) (member id outs))) arg)))))

    (define (get-progstate-ins-types my-inst)
      (define opcode-id (inst-op my-inst))
      (define class (vector-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define ins (instclass-ins class))
      (define types (instclass-args class))

      (for/list ([in ins]) (if (number? in) (vector-ref types in) in)))

    (define (get-progstate-outs-types my-inst)
      (define opcode-id (inst-op my-inst))
      (define class (vector-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define outs (instclass-outs class))
      (define types (instclass-args class))

      (for/list ([out outs]) (if (number? out) (vector-ref types out) out)))
      
    (define-syntax-rule (get-progstate-at state locs types args)
      (for/list
       ([loc locs])
       (if (number? loc)
           (let ([info (hash-ref statetypes-info (vector-ref types loc))])
             ((statetype-get info) state (vector-ref args loc)))
           (let ([info (hash-ref statetypes-info loc)])
             ((statetype-get info) state)))))
    
    (define (get-progstate-ins-vals my-inst state)
      (define opcode-id (inst-op my-inst))
      (define args (inst-args my-inst))
      (define class (vector-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define ins (instclass-ins class))
      (define types (instclass-args class))
      (get-progstate-at state ins types args))
      
    (define (get-progstate-outs-vals my-inst state)
      (define opcode-id (inst-op my-inst))
      (define args (inst-args my-inst))
      (define class (vector-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define outs (instclass-outs class))
      (define types (instclass-args class))
      (get-progstate-at state outs types args))

    ;; For building table & inverse interpret
    ;; Update if the entry in the state = val or the entry is #f.
    (define (update-progstate-ins my-inst vals state)
      (define new-state (clone-state state))
      (define opcode-id (inst-op my-inst))
      (define args (inst-args my-inst))
      (define class (vector-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define ins (instclass-ins class))
      (define types (instclass-args class))
      (define pass #t)

      (for/list
       ([in ins] [val vals] #:break (not pass))
       (cond
        [(number? in)
         (define info (hash-ref statetypes-info (vector-ref types in)))
         (define current-val ((statetype-get info) new-state (vector-ref args in)))
         (if (or (not current-val) (equal? current-val val))
             ((statetype-set info) new-state (vector-ref args in) val)
             (set! pass #f))]

        [else
         (define info (hash-ref statetypes-info in))
         (define current-val ((statetype-get info) new-state))
         (if (or (not current-val) (equal? current-val val))
             ((statetype-set info) new-state val)
             (set! pass #f))]))
      (and pass new-state))

    (define (update-progstate-del-mem addr new-state)
      (let* ([mem-type (hash-ref statetypes-info (get-memory-type))]
             [mem ((statetype-get mem-type) new-state)]
             [new-mem (send mem clone-all)])
        (send new-mem del addr)
        ((statetype-set mem-type) new-state new-mem)
        new-state))

    (define (kill-outs my-inst state)
      (define new-state (clone-state state))
      (define opcode-id (inst-op my-inst))
      (define args (inst-args my-inst))
      (define class (vector-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define outs (instclass-outs class))
      (define types (instclass-args class))

      (for/list
       ([out outs])
       (unless (member out (list (get-memory-type)))
               (cond
                [(number? out)
                 (let ([info (hash-ref statetypes-info (vector-ref types out))])
                   ((statetype-set info) new-state (vector-ref args out) #f))]

                [(equal? out (get-memory-type)) (void)]
                
                [else
                 (let ([info (hash-ref statetypes-info out)])
                   ((statetype-set info) new-state #f))])))
      new-state)

    ;; Return #t if args of a given opcode is cannonical.
    ;; args is cannonical if arg-a's ID <= arg-b's ID
    ;; for arg-a op arg-b, and op is commutative.
    ;; If op is not commutative, then always return #t.
    ;; arg: list of arguments' IDs
    (define (is-cannonical opcode-id args)
      (define class (vector-ref classes-info (vector-ref opcode-id-to-class opcode-id)))
      (define commute (instclass-commute class))
      (cond
       [commute
        (define arg-a (list-ref args (car commute)))
        (define arg-b (list-ref args (cdr commute)))
        (<= arg-a arg-b)]
       
       [else #t]))
      

    ))
