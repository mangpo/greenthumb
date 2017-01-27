#lang racket

(require "inst.rkt" "memory-rosette.rkt" "queue-rosette.rkt"
         "memory-racket.rkt" "queue-racket.rkt" "special.rkt")
(provide (all-defined-out))

(define debug #f)
(struct instclass (opcodes pool args ins outs commute) #:mutable)
(struct argtype (validfunc valid statetype) #:mutable)
(struct statetype (get set min max const structure))

(define machine%
  (class object%
    (super-new)
    (init-field 
     ;; Required fields to be initialized when extending this class.
     [bitwidth #f]          ;; Number of bits to represent a number
     [random-input-bits #f] ;; Number of bits to generate random inputs. Often equal to 'bit'.
     [config #f]            ;; Machine configuration such as # of regs, memory size, etc.
     [opcodes #f]           ;; A vector of opcode names.
     [nop-id #f]            ;; The index of nop in 'opcodes' vector.
     [opcode-id-to-class (make-hash)] ;; Map from opcode id to class name
     [classes-info (list)]            ;; Store classes' info
     [argtypes-info (make-hash)]      ;; Map from arg type to arg type info
     [statetypes-info (make-hash)]    ;; Map from state type to state type info
     [groups-of-opcodes #f]
     [max-number-of-args 0]
     [vector2scalar (make-hash)]

     ;; Fields to be set by method 'analyze-opcode'
     [opcode-pool #f]        ;; Opcodes to be considered during synthesis.
     )
    
    ;; Required methods to be implemented.
    ;; See comments at the point of method declaration in llvm/llvm-machine.rkt for example.
    (abstract progstate-structure)

    ;; Provided default methods. Can be overriden if needed.
    (public
     ;; ISA description
     define-instruction-class init-machine-description finalize-machine-description
     define-progstate-type define-arg-type

     ;; Search configuration
     window-size
     set-config get-config
     get-constructor
     
     ;; Search helper functions
     no-assumption clean-code 
     get-state clone-state display-state state-eq?
     get-opcode-id get-opcode-name
     progstate->vector vector->progstate
     get-states-from-file parse-state-text

     ;; For stochastic & enumerative search
     update-live update-live-backward
     reset-opcode-pool get-valid-opcode-pool update-classes-pool
     reset-arg-ranges
     analyze-opcode analyze-args 
     get-arg-ranges get-arg-types get-class-opcodes
     has-opcode-id? 

     ;; For enumerative search
     get-inst-key
     get-progstate-ins-types get-progstate-outs-types
     get-progstate-ins-vals get-progstate-outs-vals
     get-all-progstate-types get-progstate-type-min-max-const
     update-progstate-ins update-progstate-ins-load update-progstate-ins-store
     update-progstate-del-mem kill-outs
     is-cannonical
     )

    (define (get-constructor) (raise "Please implement machine:get-constructor"))

    ;; Non-context-aware window decomposition size. Set it to very high value.
    ;; Context-aware window decomposition size is set in xxx-symbolic.rkt and xxx-forwardbackward.rkt
    (define (window-size) 100)
    (define (get-config) config)
    (define (set-config info) (set! config info))

    ;; opcode: opcode name as symbol (not string)
    (define (get-opcode-id opcode)
      (if (symbol? opcode)
          (vector-member opcode opcodes)
          (for/vector ([name opcode]
                       [vec opcodes])
                      (or (vector-member name vec) -1))))
          
    (define (get-opcode-name id)
      (if (number? id)
          (vector-ref opcodes id)
          (for/vector ([i id]
                       [vec opcodes])
            (if (>= i 0) (vector-ref vec i) '||))))
    
    (define (no-assumption) #f)
    (define (display-state x) (pretty-display x))

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
      (vector-filter-not (lambda (x)
                           (if (number? (inst-op x))
                               (equal? (inst-op x) nop-id)
                               (equal? (vector-ref (inst-op x) 0) (vector-ref nop-id 0))))
                         code))

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

    ;; This function can be overriden to update opcode-pool and instclass-pool given code.
    (define (analyze-opcode prefix code postfix) (void))

    (define (reset-opcode-pool) (void))

    (define (get-state init #:concrete [concrete #t])
      (define (recursive-init structure init-min init-max init-const)
        (define (inner x)
          (cond
           [(symbol? x) (init #:min init-min #:max init-max #:const init-const)]
           [(vector? x) (for/vector ([xi x]) (inner xi))]
           [(list? x) (for/list ([xi x]) (inner xi))]
           [(pair? x) (cons (inner (car x)) (inner (cdr x)))]
           [else (raise "Program state uses unknown data strucutures (beyound vector, list, and pair)")]))
        (inner structure))
      
      (define progstate (progstate-structure))

      (define (inner x)
        (cond
         [(equal? x (get-memory-type))
          (new (if concrete memory-racket% memory-rosette%) [get-fresh-val init])]
         [(equal? x (get-queue-in-type))
          (new (if concrete queue-in-racket% queue-in-rosette%) [get-fresh-val init])]
         [(equal? x (get-queue-out-type))
          (new (if concrete queue-out-racket% queue-out-rosette%) [get-fresh-val init])]
         [(symbol? x)
          (define info (hash-ref statetypes-info x))
          (recursive-init (statetype-structure info)
                          (statetype-min info)
                          (statetype-max info)
                          (statetype-const info))]
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
      (cond
       [live-in
        (flatten
         (for/list
          ([class classes-info])
          (let ([pass #t])
            ;; check that all its inputs are live
            (for ([in (instclass-ins class)] #:break (not pass))
                 (unless (number? in)
                         (let ([info (hash-ref statetypes-info in)])
                           (unless ((statetype-get info) live-in)
                                   (set! pass #f)))))
            (if pass
                (instclass-pool class)
                (list)))))]
       
       [else opcode-pool]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; instruction & arg class ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Inform GreenThumb how many opcodes there are in one instruction.
    (define (init-machine-description opcode-types)
      (set! groups-of-opcodes opcode-types)
      (if (= opcode-types 1)
          (set! opcodes (list))
          (set! opcodes (make-vector opcode-types (list)))))
    
    ;; name: name of this class of instructions
    ;; class-opcodes:
    ;;   if groups-of-opcodes = 1, class-opcodes is a list of opcodes
    ;;   if groups-of-opcodes = n > 1, class-opcodes is a list of <= n sublists of opcodes.
    ;;      where sublist i corresponds to opcodes in group i.
    ;;      (see ARM for an example)
    (define (define-instruction-class name class-opcodes
              #:scalar [scalar #f] #:vector-width [vector-width #f]
              #:args [args '()] #:ins [ins '()] #:outs [outs '()] #:commute [commute #f]
              #:required [required (list)])
      (unless opcodes
              (raise "Call 'init-machine-description' before defining instruction classes"))

      ;;(pretty-display `(class-opcodes ,class-opcodes))
      (for ([arg (flatten args)])
           (unless (hash-has-key? argtypes-info arg)
                   (raise (format "Undefined argument type ~a in 'args'" arg))))
      (for ([in (flatten ins)])
           (unless (or (number? in) (hash-has-key? statetypes-info in))
                   (raise (format "Undefined program state type ~a in 'ins'" in))))
      (for ([out (flatten outs)])
           (unless (or (number? out) (hash-has-key? statetypes-info out))
                   (raise (format "Undefined program state type ~a in 'outs'" out))))
      ;; filter out an entry that is not a part of program state (get & set = #f)
      (cond
       ;; if opcodes is a list of lists
       [(> groups-of-opcodes 1)
        (when scalar
              (raise "define-instruction-class does not support defining vector instructions with opcode groups > 1."))
        (when (symbol? (car class-opcodes))
              (set! class-opcodes (list class-opcodes))
              (set! args (list args))
              (set! ins (list ins))
              (set! required (list #t)))

        (when (> (length class-opcodes) groups-of-opcodes)
              (raise (format "The number of groups of opcodes provided at define-instruction-class '~a' is more than '~a', which is defined at init-machine-description." (length class-opcodes) groups-of-opcodes)))
        (unless (= (length class-opcodes) (length required))
                (raise (format "At define-instruction-class '~a', there are ~a groups of opcodes, but only ~a groups are specified with required or optional." name (length class-opcodes) (length required))))
        (unless (= (length class-opcodes) (length args))
                (raise (format "At define-instruction-class '~a', there are ~a groups of opcodes, but ~a groups of arguments are given." name (length class-opcodes) (length args))))
        (unless (= (length class-opcodes) (length ins))
                (raise (format "At define-instruction-class '~a', there are ~a groups of opcodes, but ~a groups of inputs are given." name (length class-opcodes) (length ins))))

        (define all-args (flatten args))
        (set! ins (for/list ([in ins]) (filter-statetype in all-args)))
        (set! outs (filter-statetype outs (car args)))
        ;; collect opcodes
        (for ([group class-opcodes]
              [id (in-naturals)])
             (for ([opcode group])
                  (unless (member opcode (vector-ref opcodes id))
                          (vector-set! opcodes id (cons opcode (vector-ref opcodes id))))))
        ;; insert instruction classes
        (for ([group (all-opcodes-groups class-opcodes args ins required)])
             (check-max-number-of-args (second group))
             (set! classes-info
                   (cons (instclass (first group) #f
                                    (list->vector (second group))
                                    (third group) outs commute)
                         classes-info)))
        ]

       [else
        (unless (symbol? (car class-opcodes))
                (raise (format "The number of groups of opcodes provided at define-instruction-class '~a' is more than '1', which is defined at init-machine-description." (length class-opcodes))))
        (check-max-number-of-args args)
        
        (set! ins (filter-statetype ins args))
        (set! outs (filter-statetype outs args))
        ;; collect opcodes
        (for ([opcode class-opcodes])
             (unless (member opcode opcodes)
                     (set! opcodes (cons opcode opcodes))))
        ;; insert instruction class
        (set! classes-info
              (cons (instclass class-opcodes #f (list->vector args) ins outs commute)
                    classes-info))

        ;; map vector opcode to scalar opcode
        (when scalar
              (unless
               (= (length class-opcodes) (length scalar))
               (raise "Number of vector opcodes is not equal to number of corresponding scalar opcodes."))
              (for ([v class-opcodes]
                    [s scalar])
                   (hash-set! vector2scalar v (cons s vector-width))))
        ])
      
      ;;(pretty-display (format "[DEFINE] class=~a | args=~a ins=~a outs=~a" name args ins outs))
      )

    (define (check-max-number-of-args args)
        (when (> (length args) max-number-of-args) (set! max-number-of-args (length args))))

    (define (filter-statetype locs args)
      (define (pred x)
        (if (number? x)
            (hash-has-key? statetypes-info
                           (argtype-statetype (hash-ref argtypes-info (list-ref args x))))
            (hash-has-key? statetypes-info x)))
      (filter pred locs))

    ;; Given an instruction class with multiple types of opcodes (both required and optional).
    ;; Generate all instruction classes with required opcode types.
    ;; TODO: when ins/outs refer to args using numbers***
    (define (all-opcodes-groups opcodes-groups args-groups ins-groups required)
      (define ret (list))
      
      ;; (define (adjust ins-groups args-groups)
      ;;   (define offset 0)
      ;;   (for/list ([ins ins-groups]
      ;;              [args args-groups])
      ;;             (let ([ret
      ;;                    (for/list ([in ins])
      ;;                              (if (number? in) (+ in offset) in))])
      ;;               (set! offset (+ offset (length args)))
      ;;               ret)))

      (define (adjust-ins ins offset)
        (for/list ([l ins])
                  (for/list ([i l])
                            (if (number? i) (- i offset) i))))
      
      (define (recurse opcodes-final args-final ins-final
                       opcodes-groups args-groups ins-groups required)
        (cond
         [(empty? opcodes-groups)
          (set! ret
                (cons
                 (list (reverse opcodes-final)
                       (flatten (reverse args-final)) (flatten (reverse ins-final)))
                 ret))]
         [(empty? (car opcodes-groups))
          (recurse (cons (list '||) opcodes-final)
                   (cons (list) args-final)
                   (cons (list) ins-final)
                   (cdr opcodes-groups) (cdr args-groups) (cdr ins-groups) (cdr required))]
          
         [else
          ;; If this opcode type is not required, try excluding it.
          (unless (car required)
                  (recurse (cons (list '||) opcodes-final)
                           (cons (list) args-final)
                           (cons (list) ins-final)
                           (cdr opcodes-groups) (cdr args-groups)
                           (adjust-ins (cdr ins-groups) (length (car args-groups)))
                           (cdr required)))

          ;; Include this opcode type.
          (recurse (cons (car opcodes-groups) opcodes-final)
                   (cons (car args-groups) args-final)
                   (cons (car ins-groups) ins-final)
                   (cdr opcodes-groups) (cdr args-groups) (cdr ins-groups) (cdr required))]))
      (define extra (- groups-of-opcodes (length opcodes-groups)))
      (recurse (list) (list) (list)
               (append opcodes-groups (make-list extra (list)))
               (append args-groups (make-list extra (list)))
               (append ins-groups (make-list extra (list)))
               (append required (make-list extra #t)))
      ret)

    ;; Given an instruction class with (one or more) required opcode types.
    ;; Enumerate all possible combinations, and converting opcode name to opcode id in the process.
    (define (all-opcodes-combinations opcodes-groups)
      (when debug (pretty-display `(groups ,opcodes-groups)))
      (define ret (list))
      (define (recurse final work)
        (cond
         [(empty? work) (set! ret (cons (list->vector final) ret))]
         [else
          (define remain (cdr work))
          (for ([op (car work)]) (recurse (cons op final) remain))]))
      
      (if (list? (car opcodes-groups))
          ;; multiple opcode types, enumerate.
          (begin
            (unless (= (length opcodes-groups) groups-of-opcodes)
                    (raise (format "all-opcodes-combinations: number of generated opcode groups is ~a, not equal to ~a, which is defined at init-machine-description." (length opcodes-groups) groups-of-opcodes)))
            (recurse (list)
                     (reverse
                      (for/list ([group opcodes-groups]
                                 [ops opcodes])
                                ;; if opcode doens't exist, use -1 (default nop)
                                (map (lambda (x) (or (vector-member x ops) -1)) group))))
            ret)
          ;; one opcode type, don't have to enumerate.
          (map (lambda (x) (vector-member x opcodes)) opcodes-groups)))

    (define (define-progstate-type name #:get [get #f] #:set [set #f]
              #:min [min #f] #:max [max #f] #:const [const #f]
              #:structure [st 'x])
      (hash-set! statetypes-info name (statetype get set min max const st)))

    (define (define-arg-type name validfunc #:progstate [state name])
      (hash-set! argtypes-info name (argtype validfunc #f state)))

    (define (finalize-machine-description)

      ;; set nop-id and convert opcodes into vector format
      (cond
       [(> groups-of-opcodes 1)
        ;; (for ([group1 opcodes] [id1 (in-naturals)])
        ;;      (for ([group2 opcodes] [id2 (in-naturals)])
        ;;           (when (< id1 id2)
        ;;                 (let ([common (set-intersect (list->set group1) (list->set group2))])
        ;;                   (unless (set-empty? common)
        ;;                           (raise (format "~a cannot be in both opcode groups ~a and ~a." id1 id2)))))))  
        
        
        (set! opcodes (for/vector ([group opcodes]) (list->vector (reverse group))))
        (set! nop-id (vector-member 'nop (vector-ref opcodes 0)))
        ]
       [else
        (set! opcodes (list->vector (reverse opcodes)))
        (set! nop-id (vector-member 'nop opcodes))])

      ;; Need to convert opcodes into vector format before this because
      ;; all-opcodes-combinations requires opcodes to be in vector format.
      (set! classes-info (reverse classes-info))
      (for ([info classes-info]
            [id (in-naturals)])
           ;; (pretty-display `(class ,(instclass-opcodes info)))
           ;; (pretty-display `(ins ,(instclass-ins info)))
           (let* ([class-opcodes (all-opcodes-combinations (instclass-opcodes info))])
             (set-instclass-opcodes! info class-opcodes)
             (set-instclass-pool! info class-opcodes)
             (for ([ops-vec class-opcodes])
                  (hash-set! opcode-id-to-class ops-vec id))))
      
      ;; convert classes-info into vector format
      (set! opcode-pool (flatten (for/list ([info classes-info]) (instclass-pool info))))
      (set! classes-info (list->vector classes-info))

      (define nop-ops-vec #f)
      (when (> groups-of-opcodes 1)
            (for ([ops-vec (hash-keys opcode-id-to-class)])
                 (when (= nop-id (vector-ref ops-vec 0))
                       (when nop-ops-vec (raise "'nop' cannot be in multiple instruction classes."))
                       (set! nop-ops-vec ops-vec)))
            (set! nop-id nop-ops-vec))

      ;; convert to id domain
      (define new-vector2scalar (make-hash))
      (for ([pair (hash->list vector2scalar)])
           (hash-set! new-vector2scalar (get-opcode-id (car pair))
                        (cons
                         (get-opcode-id (cadr pair))
                         (cddr pair))))
      (set! vector2scalar new-vector2scalar)

      (when debug
            (pretty-display `(opcodes ,opcodes))
            ;;(pretty-display `(opcode-id-to-class ,opcode-id-to-class))
            (pretty-display `(nop-id ,nop-id)))
      
      ;; (for ([info classes-info]
      ;;       [id (in-naturals)])
      ;;      (pretty-display `(class ,id ,(instclass-args info) ,(instclass-ins info))))
      )

    (define (update-classes-pool)
      (for ([class classes-info])
           (set-instclass-pool!
            class
            (filter
             (lambda (opcode-id) (member opcode-id opcode-pool))
             (instclass-opcodes class)))))

    (define (get-class-opcodes opcode-id)
      (instclass-pool (vector-ref classes-info (hash-ref opcode-id-to-class opcode-id))))

    (define (has-opcode-id? opcode-id)
      (hash-has-key? opcode-id-to-class opcode-id))
    
    ;; Return types of operands given opcode-name.
    (define (get-arg-types opcode-id)
      (instclass-args (vector-ref classes-info (hash-ref opcode-id-to-class opcode-id))))

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
    ;; live-in & live-out: vector/list/pair format
    ;; There are 3 modes.
    ;;  1) `basic (no restriction)
    ;;  2) `no-args = ignore reigster operands. Return `var-o and `var-i for operand that is input variable and output variable respectively. This mode is only used for enumerative search.
    (define (get-arg-ranges opcode-id entry live-in
                            #:live-out [live-out #f] #:mode [mode `basic])
      (define class (vector-ref classes-info (hash-ref opcode-id-to-class opcode-id)))
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
                  (get-arg-range-of-type type live-out #:vals (vector->list vals))]
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
      ;;(pretty-display `(vals ,type ,vals))
      (list->vector
       (if live
           (let ([get (statetype-get (hash-ref statetypes-info
                                               (argtype-statetype (hash-ref argtypes-info type))))])
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
      (cond
       [live
        (define new-live (clone-state live))
        (define opcode-id (inst-op my-inst))
        (define args (inst-args my-inst))
        (define class (vector-ref classes-info (hash-ref opcode-id-to-class opcode-id)))
        (define types (instclass-args class))
        (define outs (instclass-outs class))

        (when args
              (for ([type types] [id (in-naturals)] [arg args])
                   (when (member id outs)
                         (let ([info (hash-ref statetypes-info
                                               (argtype-statetype (hash-ref argtypes-info type)))])
                           ((statetype-set info) new-live arg #t)))))

        (for ([out outs])
             (when (hash-has-key? statetypes-info out)
                   (let ([info (hash-ref statetypes-info out)])
                     ((statetype-set info) new-live #t))))
        new-live]

       [else live])
      )

    
    ;; For enumerative search
    ;; instruction x: e.g. add v0, v1, v2
    ;; liveness *after* execute inst (given live): (vector #t * *)
    ;; liveness *before* execute inst (output): (vector #f #t #t).
    ;; v1 and v2 must be live-in. v0 is not live-in.
    (define (update-live-backward live my-inst)
      (cond
       [live
        (define new-live (clone-state live))
        (define opcode-id (inst-op my-inst))
        (define args (inst-args my-inst))
        (define class (vector-ref classes-info (hash-ref opcode-id-to-class opcode-id)))
        (define types (instclass-args class))
        (define ins (instclass-ins class))
        (define outs (instclass-outs class))

        (when args
              ;; kill outs first
              (for ([type types] [id (in-naturals)] [arg args])
                   (when (member id outs)
                         (let ([info (hash-ref statetypes-info
                                               (argtype-statetype (hash-ref argtypes-info type)))])
                           ((statetype-set info) new-live arg #f)))))
        (for ([out outs])
             (when (and (not (special-type? out))
                        (hash-has-key? statetypes-info out))
                   (let ([info (hash-ref statetypes-info out)])
                     ((statetype-set info) new-live #f))))
        (when args
              ;; add live
              (for ([type types] [id (in-naturals)] [arg args])
                   (when (member id ins)
                         (let ([info (hash-ref statetypes-info
                                               (argtype-statetype (hash-ref argtypes-info type)))])
                           ((statetype-set info) new-live arg #t)))))
        (for ([in ins])
             (when (hash-has-key? statetypes-info in)
                   (let ([info (hash-ref statetypes-info in)])
                     ((statetype-set info) new-live #t))))
        new-live]
       [else live]))

    ;; Analyze input code and update operands' ranges.
    (define (analyze-args prefix code postfix live-in-list live-out)
      (for ([x (vector-append prefix code postfix)])
           (analyze-args-inst x))

      (when debug
            (pretty-display `(analyze-args))
            (for ([pair (hash->list argtypes-info)])
                 (let ([name (car pair)]
                       [info (cdr pair)])
                   (pretty-display `(ARG ,name ,(argtype-valid info))))))
      )

    (define (analyze-args-inst my-inst)
      (define opcode-id (inst-op my-inst))
      (define args (inst-args my-inst))

      (when
       args
       (define class (vector-ref classes-info (hash-ref opcode-id-to-class opcode-id)))
       (define types (instclass-args class))
       
       (for ([type types] [arg args])
            (let* ([argtype-info (hash-ref argtypes-info type)]
                   [vals (argtype-valid argtype-info)])
              (unless (member arg vals)
                      (set-argtype-valid! argtype-info (cons arg vals))))))
      )
    
    ;; For building behavior-bw
    (define (get-inst-key my-inst)
      (define opcode-id (inst-op my-inst))
      (define args (inst-args my-inst))
      (define class (vector-ref classes-info (hash-ref opcode-id-to-class opcode-id)))
      (define ins (instclass-ins class))
      (define outs (instclass-outs class))
      (cons
       opcode-id
       (filter (lambda (x) x)
               (for/list ([arg args] [id (in-naturals)])
                         (and (not (or (member id ins) (member id outs))) arg)))))

    (define (get-progstate-ins-types my-inst)
      (define opcode-id (inst-op my-inst))
      (define class (vector-ref classes-info (hash-ref opcode-id-to-class opcode-id)))
      (define ins (instclass-ins class))
      (define types (instclass-args class))

      (for/list ([in ins]) (if (number? in) (vector-ref types in) in)))

    (define/public (get-progstate-ins-outs opcode-id)
      (define class (vector-ref classes-info (hash-ref opcode-id-to-class opcode-id)))
      (values (instclass-ins class) (instclass-outs class)))

    (define (get-progstate-outs-types my-inst)
      (define opcode-id (inst-op my-inst))
      (define class (vector-ref classes-info (hash-ref opcode-id-to-class opcode-id)))
      (define outs (instclass-outs class))
      (define types (instclass-args class))

      (for/list ([out outs]) (if (number? out) (vector-ref types out) out)))
      
    (define-syntax-rule (get-progstate-at state locs types args)
      (for/list
       ([loc locs])
       (if (number? loc)
           (let ([info (hash-ref statetypes-info
                                 (argtype-statetype (hash-ref argtypes-info (vector-ref types loc))))])
             ((statetype-get info) state (vector-ref args loc)))
           (let ([info (hash-ref statetypes-info loc)])
             ((statetype-get info) state)))))
    
    (define (get-progstate-ins-vals my-inst state)
      (define opcode-id (inst-op my-inst))
      (define args (inst-args my-inst))
      (define class (vector-ref classes-info (hash-ref opcode-id-to-class opcode-id)))
      (define ins (instclass-ins class))
      (define types (instclass-args class))
      (get-progstate-at state ins types args))
      
    (define (get-progstate-outs-vals my-inst state)
      (define opcode-id (inst-op my-inst))
      (define args (inst-args my-inst))
      (define class (vector-ref classes-info (hash-ref opcode-id-to-class opcode-id)))
      (define outs (instclass-outs class))
      (define types (instclass-args class))
      (get-progstate-at state outs types args))

    ;; For building inverse behavior table & inverse interpret
    ;; Update sate if the entry in the state = val or the entry is #f.
    (define (update-progstate-ins my-inst vals state)
      (define new-state (clone-state state))
      (define opcode-id (inst-op my-inst))
      (define args (inst-args my-inst))
      (define class (vector-ref classes-info (hash-ref opcode-id-to-class opcode-id)))
      (define ins (instclass-ins class))
      (define types (instclass-args class))
      (define pass #t)

      (for/list
       ([in ins] [val vals] #:break (not pass))
       (cond
        [(number? in)
         (define info (hash-ref statetypes-info
                                (argtype-statetype (hash-ref argtypes-info (vector-ref types in)))))
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
             [new-mem (send mem clone)]) ;;clone-all
        (send new-mem del addr)
        ((statetype-set mem-type) new-state new-mem)
        new-state))

    (define (kill-outs my-inst state)
      (define new-state (clone-state state))
      (define opcode-id (inst-op my-inst))
      (define args (inst-args my-inst))
      (define class (vector-ref classes-info (hash-ref opcode-id-to-class opcode-id)))
      (define outs (instclass-outs class))
      (define types (instclass-args class))

      (for/list
       ([out outs])
       (cond
        [(number? out)
         (let ([info (hash-ref statetypes-info
                               (argtype-statetype (hash-ref argtypes-info (vector-ref types out))))])
           ((statetype-set info) new-state (vector-ref args out) #f))]

        [(special-type? out) (void)]
        
        [else
         (let ([info (hash-ref statetypes-info out)])
           ((statetype-set info) new-state #f))]))
      new-state)

    (define (update-progstate-ins-load my-inst addr mem state)
      (raise "update-progstate-ins-load: unimplemented. Need to extend this function."))

    (define (update-progstate-ins-store my-inst addr val state)
      (raise "update-progstate-ins-store: unimplemented. Need to extend this function."))

    ;; Return #t if args of a given opcode is cannonical.
    ;; args is cannonical if arg-a's ID <= arg-b's ID
    ;; for arg-a op arg-b, and op is commutative.
    ;; If op is not commutative, then always return #t.
    ;; arg: list of arguments' IDs
    (define (is-cannonical opcode-id args)
      (define class (vector-ref classes-info (hash-ref opcode-id-to-class opcode-id)))
      (define commute (instclass-commute class))
      (cond
       [commute
        (define arg-a (list-ref args (car commute)))
        (define arg-b (list-ref args (cdr commute)))
        (<= arg-a arg-b)]
       
       [else #t]))
      

    ))
