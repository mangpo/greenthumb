#lang racket

(require "../printer.rkt" 
         "../inst.rkt")

(provide llvm-printer%)

(define llvm-printer%
  (class printer%
    (super-new)
    (inherit-field machine)
    (inherit encode decode)
    (override encode-inst decode-inst print-syntax-inst print-encode-info
              ;; Required method for cooperative search
              config-from-string-ir output-constraint-string)

    ;; Print in LLVM IR format.
    ;; x: instruction
    (define (print-syntax-inst x [indent ""])
      (define op (inst-op x))
      (define args (inst-args x))
      (cond
       [(equal? op "nop") (void)]
       [(equal? op "store")
        (pretty-display
         (format "~a i32 ~a, i32* ~a"
                 op (vector-ref args 0) (vector-ref args 1)))]
       [(equal? op "load")
        (pretty-display
         (format "~a = ~a i32, i32* ~a"
                 (vector-ref args 0) op (vector-ref args 1)))]
       
       [else
        (define matches (regexp-match* #rx"_v[0-9]+" op))
        (cond
         [(empty? matches)
          (display (format "~a = ~a i32 ~a" 
                           (vector-ref args 0)
                           op
                           (vector-ref args 1)))
          (for ([i (range 2 (vector-length args))])
               (display (format ", ~a" (vector-ref args i))))
          (newline)]

         [(= (length matches) 1)
          (display (format "~a = ~a <~a x i32> ~a" 
                           (vector-ref args 0)
                           (extract-op op) (substring (car matches) 2)
                           (format-arg (vector-ref args 1) "i32")))
          (for ([i (range 2 (vector-length args))])
               (display (format ", ~a" (format-arg (vector-ref args i) "i32"))))
          (newline)
          ]

         [(= (length matches) (sub1 (vector-length args)))
          (define args-string
            (for/list ([i (range 1 (vector-length args))]
                       [m matches])
                      (format "<~a x i32> ~a"
                              (substring m 2)
                              (format-arg (vector-ref args i) "i32"))))
          (pretty-display (format "~a = ~a ~a" 
                                  (vector-ref args 0)
                                  (extract-op op) (string-join args-string ", ")))
          ]

         [else (raise "print-syntax-inst: numbers of types and args mismatch ~a." x)]

         )]
       ))

    (define (extract-op op)
      (define pos (caar (regexp-match-positions #rx"_v[0-9]+" op)))
      (substring op 0 pos))

    (define (format-arg arg type)
      (cond
       [(string? arg) arg]
       [(vector? arg)
        (define l (for/list ([c arg]) (format "~a ~a" type c)))
        (format "<~a>" (string-join l ", "))]
       [else (raise "print-syntax-inst: unknown arg format ~a." arg)]))

    (define name2num (make-hash))
    (define num2name (make-vector 100))
    (define name2num-vec4 (make-hash))
    (define num2name-vec4 (make-vector 100))

    (define (get-var-id arg name2num num2name)
      (if (hash-has-key? name2num arg)
          (hash-ref name2num arg)
          ;; If not in the table, give it a fresh number
          ;; and update the table.
          (let ([id (hash-count name2num)])
            (hash-set! name2num arg id)
            (vector-set! num2name id arg)
            id)))

    (define (print-encode-info)
      (pretty-display (format "Encode var info  (name->num): ~a" name2num))
      (pretty-display (format "Encode vec4 info (name->num): ~a" name2num-vec4))
      )

    (define-syntax-rule (char1=% x) (and (string? x) (equal? (substring x 0 1) "%")))
				  
    ;; Convert an instruction x from string-IR to encoded-IR format.
    (define (encode-inst x)
      (cond
       [(equal? (inst-op x) "nop") (inst (get-field nop-id machine) (vector))]
       [(equal? (inst-op x) #f) x]
       [else
        (define args (inst-args x))
        ;; First input argument.
        (define first-in (vector-ref args 1))
        ;; Last input argument.
        (define last-in (vector-ref args (sub1 (vector-length args))))
        (define op
          (string->symbol
           (cond
            [(and (char1=% first-in) (char1=% last-in))
             (inst-op x)]
            
            ;; Append # to opcode to indicate that last argument is a constant.
            [(char1=% first-in)
             (string-append (inst-op x) "#")]

            ;; Prepend _ to opcode to indicate that first argument is a constant.
            [(char1=% last-in)
             (string-append "_" (inst-op x))]
            
            [else
             (raise "Not support %out = op <imm>, <imm>")])))
        (define op-id (send machine get-opcode-id op))
        (define new-args
          (for/vector ([arg args]
                       [type (send machine get-arg-types op-id)])
                      (cond
                       [(equal? type 'var)
                        (get-var-id arg name2num num2name)]
                       [(equal? type 'vec4)
                        (get-var-id arg name2num-vec4 num2name-vec4)]
                       [(equal? type 'const-vec4)
                        (vector-map string->number arg)]
                       [else 
                        (string->number arg)])))
        
        (inst op-id new-args)]))

    (define (fresh-name num2name prefix)
      (define numbers
	(filter 
	 number?
	 (map (lambda (x) (and (string? x) (string->number (substring x 2))))
	      (vector->list num2name))))
      (format "%~a~a" prefix (add1 (foldl max 0 numbers))))
    
    (define (num->name x num2name prefix)
      (define name (vector-ref num2name x))
      (when (equal? name 0)
            (set! name (fresh-name num2name prefix))
            (vector-set! num2name x name))
      name)
    
    ;; Convert an instruction x from encoded-IR to string-IR format.
    (define (decode-inst x)
      (define op (symbol->string (send machine get-opcode-name (inst-op x))))
      (cond
       [(equal? op "nop") (inst op (vector))]
       [else
        (define args (inst-args x))
        (when (member op (list "load" "store"))
              (set! args (vector-copy args 0 2)))

        (define types (send machine get-arg-types (inst-op x)))

        (define new-args
          (for/vector ([arg args] [type types])
                      (cond
                       [(equal? type 'var) (num->name arg num2name "x")]
                       [(equal? type 'vec4) (num->name arg num2name-vec4 "v")]
                       [(equal? type 'const-vec4) (vector-map number->string arg)]
                       [else (number->string arg)])))

        (cond
         [(regexp-match #rx"#" op)
          (set! op (substring op 0 (sub1 (string-length op))))
          ]
         [(equal? "_" (substring op 0 1))
          (set! op (substring op 1))
          ])

        (inst op new-args)]))

    ;; Convert liveness infomation to the same format as program state.
    ;; x: (vector <list of variables> <list of vector variables> <memory: #t or #f>)
    ;; output: liveness in program state format
    (define/public (encode-live x)
      (define config (send machine get-config))
      (define live (make-vector (car config) #f))
      (define live-vec4 (make-vector (cdr config) #f))
      (for ([v (vector-ref x 0)])
           (vector-set! live (hash-ref name2num (if (symbol? v) (symbol->string v) v)) #t))
      (for ([v (vector-ref x 1)])
           (vector-set! live-vec4 (hash-ref name2num-vec4 (if (symbol? v) (symbol->string v) v)) #t))
      (vector live live-vec4 (vector-ref x 2)))

    ;;;;;;;;;;;;;;;;;;;;; For cooperative search ;;;;;;;;;;;;;;;;;;
    
    ;; Return program state config from a given program in string-IR format.
    ;; program: string IR format
    ;; output: program state config, an input to machine:set-config
    (define (config-from-string-ir program)
      (encode program)
      (cons (max 3 (hash-count name2num)) ;; need at least 3 to generate inverse inst behaviors
            (hash-count name2num-vec4)))
    
    ;; Convert live-out (the output from parser::info-from-file) into string. 
    (define (output-constraint-string live-out) 
      (format "(send printer encode-live '~a)" live-out))

    ;; Compress space by reusing variables.
    (define/override (compress-state-space program live-out)
      (define code (encode program))
      
      (define all-names (set))
      (define all-names-vec4 (set))
      (for ([v (vector-ref live-out 0)])
           (set! all-names (set-add all-names
                                    (hash-ref name2num (if (symbol? v) (symbol->string v) v)))))
      (for ([v (vector-ref live-out 1)])
           (set! all-names-vec4 (set-add all-names-vec4
                                         (hash-ref name2num-vec4 (if (symbol? v) (symbol->string v) v)))))
      
      (define avail-names (list))
      (define avail-names-vec4 (list))
      (define no-rename (list))

      (define-syntax-rule (add-to x l)
        (unless (member x l) (set! l (cons x l))))

      (define-syntax-rule (add-to-set x l)
        (set! l (set-add l x)))

      (define (exclude? arg)
        (member arg no-rename))
      
      ;; Rename variable from old to new up to instruction at index.
      (define (rename-var old new index this-type)
        (define def #f)
        (for ([i (reverse (range index))] #:break def)
             (let* ([my-inst (vector-ref code i)]
                    [args (inst-args my-inst)]
                    [types (send machine get-arg-types (inst-op my-inst))])
               (for ([arg args]
                     [type types]
                     [arg-i (in-naturals)] #:break def)
                    (when (and (equal? arg old) (equal? type this-type))
                          (vector-set! args arg-i new)
                          (when (= arg-i 0) (set! def #t)))))))

      ;; Check if arg is an input variable to the program.
      (define (is-input? arg index)
        (define def #f)
        (for ([i index] #:break def)
             (let* ([my-inst (vector-ref code i)]
                    [args (inst-args my-inst)])
               (when (equal? arg (vector-ref args 0))
                     (set! def #t))))
        (not def))
      
      (for ([index (reverse (range (vector-length code)))])
           (let* ([my-inst (vector-ref code index)]
                  [op (inst-op my-inst)])
             (when
              op
              (let* ([args (inst-args my-inst)]
                     [types (send machine get-arg-types op)])
                (let-values ([(ins outs) (send machine get-progstate-ins-outs op)])
                  ;; add outs to available names and all names
                  (for ([out outs])
                       (when (number? out)
                             (let ([arg (vector-ref args out)]
                                   [type (vector-ref types out)])
                               (cond
                                [(equal? type 'var)
                                 (add-to-set arg all-names)
                                 (add-to arg avail-names)]
                                [(equal? type 'vec4)
                                 (add-to-set arg all-names-vec4)
                                 (add-to arg avail-names-vec4)]))))
                  
                  ;; remove outs from available names
                  (for ([in ins])
                       (when (number? in)
                             (let ([arg (vector-ref args in)]
                                   [type (vector-ref types in)])
                               (cond
                                [(equal? type 'var)
                                 (set! avail-names (remove arg avail-names))]
                                [(equal? type 'vec4)
                                 (set! avail-names-vec4 (remove arg avail-names-vec4))]))))

                  (define-syntax-rule (try-rename arg type all-names avail-names)
                    (unless (set-member? all-names arg)
                            (if (empty? avail-names)
                                (add-to-set arg all-names)
                                (begin
                                  (rename-var arg (car avail-names) (add1 index) type)
                                  (set! avail-names (cdr avail-names))))))

                  ;; rename intermediates
                  (for ([arg args]
                        [type types]
                        [i (in-naturals)])
                       (when (and (member i ins)
                                  (not (is-input? arg index))
                                  (not (exclude? arg)))
                             (cond
                              [(equal? type 'var)
                               (try-rename arg type all-names avail-names)]
                              [(equal? type 'vec4)
                               (try-rename arg type all-names-vec4 avail-names-vec4)])))
                  )))))

      (define new-program (decode code))
      (set! name2num (make-hash))
      (set! num2name (make-vector 100))
      (set! name2num-vec4 (make-hash))
      (set! num2name-vec4 (make-vector 100))
      (values new-program
              live-out
              #f 
              (config-from-string-ir new-program)))

    
    ))
