#lang racket

(require "ast.rkt")
(provide debug machine%)

(define debug #f)

(define machine%
  (class object%
    (super-new)
    (init-field 
     ;; Required fields to be initialized when extending this class.
     [bit #f]              ;; Number of bits to represnet a number
     [random-input-bit #f] ;; Number of bits to generate random inputs. Often equal to 'bit'.
     [config #f]           ;; Machine configuration such as # of regs, memory size, etc.
     [inst-id #f]          ;; A vector of opcode names.
     [nop-id #f]           ;; The index of nop in 'inst-id' vector.
     [classes #f]          ;; A vector of lists of opcodes. Each list groups opcodes with the same operands' types together.
     [classes-len #f]      ;; Number of classes.

     ;; Fields to be set by method 'analyze-opcode'
     [inst-pool #f]        ;; Opcodes to be considered during synthesis.
     [classes-filtered #f] ;; 'classes' that is filtered in only opcodes in 'inst-pool'.
     )

    ;; Required methods to be implemented.
    ;; See comments at the point of method declaration in arm/arm-machine.rkt for example.
    (abstract set-config
              get-state display-state display-state-text parse-state-text
              adjust-config get-memory-size
              output-constraint-string 
              progstate->vector vector->progstate
	      get-arg-ranges reset-arg-ranges window-size)

    ;; Provided default methods. Can be overriden if needed.
    (public set-config-string get-config
            get-class-id no-assumption
            get-inst-id get-inst-name
            finalize-config config-exceed-limit?
            output-assume-string get-state-liveness
            get-states-from-file 
	    clean-code state-eq? relaxed-state-eq?
	    update-live update-live-backward filter-live get-live-list
	    analyze-opcode analyze-args 
            reset-inst-pool)

    (define (get-config) config)

    (define (set-config-string x)
      (cond
       [(number? x) (number->string x)]
       [(or (list? x) (vector? x))
        (string-join
         (append (list "(list")
                 (for/list ([i x]) (set-config-string i))
                 (list ")")))]
       [(pair? x)
        (format "(cons ~a ~a)"
                (set-config-string (car x))
                (set-config-string (cdr x)))]
       [else
        (raise (format "machine:set-config: unimplemented for ~a" x))]))

    (define (get-inst-id opcode) (vector-member opcode inst-id))
    (define (get-inst-name id) (vector-ref inst-id id))
    (define (no-assumption) #f)
    (define (get-state-liveness f extra) (get-state f extra))

    (define (finalize-config info) info)
    (define (config-exceed-limit? info)
      (> (get-memory-size info) 100))

    ;; x: name in form of symbol
    (define (get-class-id x)
      (define id #f)
      (for ([i (in-range classes-len)])
           (when (member x (vector-ref classes i))
                 (set! id i)))
      id)

    (define (output-assume-string machine-var x) x)

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

    (define (update-live live x) live)
    (define (update-live-backward live x) live)

    (define (filter-live range live)
      (if live
          (vector-filter (lambda (x) (member x live)) range)
          range))

    (define (get-live-list constraint) (progstate->vector constraint))
    
    (define (analyze-opcode prefix code postfix)
      (set! inst-pool (range (vector-length inst-id)))
      (set! classes-filtered 
            (for/vector ([c classes])
                        (map (lambda (x) (vector-member x inst-id)) c)))
      #t
      )

    (define (reset-inst-pool)
      (set! inst-pool (range (vector-length inst-id))))

    (define (analyze-args prefix code postfix #:only-const [x #f])
      (void))

    ))
