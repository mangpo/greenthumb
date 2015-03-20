#lang racket

(require "ast.rkt")
(provide debug machine%)

(define debug #f)

(define machine%
  (class object%
    (super-new)
    (init-field [bit #f] [random-input-bit #f] [inst-id #f] [inst-pool #f] 
		[classes #f] [classes-len #f] [classes-filtered #f]
		[perline 8] [nop-id #f])
    (abstract set-config get-config set-config-string
              get-state display-state
              adjust-config finalize-config config-exceed-limit?
              output-constraint-string
              progstate->vector vector->progstate
	      get-arg-ranges 
	      window-size)
    (public get-class-id print-line no-assumption
            get-inst-id get-inst-name
            output-assume-string get-state-liveness
            display-state-text parse-state-text get-states-from-file syntax-equal?
	    clean-code state-eq? relaxed-state-eq?
	    update-live filter-live get-operand-live
	    analyze-opcode analyze-args)

    (define (get-inst-id opcode)
      (vector-member opcode inst-id))

    (define (get-inst-name id)
      (vector-ref inst-id id))

    (define (no-assumption) #f)

    (define (get-state-liveness f extra) (get-state f extra))

    ;; x: name in form of symbol
    (define (get-class-id x)
      (define id #f)
      (for ([i (in-range classes-len)])
           (when (member x (vector-ref classes i))
                 (set! id i)))
      id)

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

    (define (output-assume-string machine-var x)
      x)

    (define (display-state-text x)
      (raise "display-state-text: unimplemented"))

    (define (parse-state-text str)
      (raise "parse-state-text: unimplemented"))

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
    
    (define (syntax-equal? code1 code2)
      (for/and ([x1 code1]
                [x2 code2])
               (and (equal? (inst-op x1) (inst-op x2))
                    (equal? (inst-args x1) (inst-args x2)))))

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

    (define (relaxed-state-eq? state1 state2 pred)
      (state-eq? state1 state2 pred))

    (define (update-live live x)
      (define (add-live ele lst)
        (if (member ele lst) lst (cons ele lst)))
      (and live
           (cond
            [(= (vector-length (inst-args x)) 0) live]
            [else
             (let ([def (vector-ref (inst-args x) 0)])
               (if (number? def)
                   (add-live def live)
                   (foldl add-live live def)))])))

    (define (filter-live range live)
      (if live
          (vector-filter (lambda (x) (member x live)) range)
          range))

    (define (get-operand-live constraint) #f)
    
    (define (analyze-opcode prefix code postfix)
      (set! inst-pool (range (vector-length inst-id)))
      (set! classes-filtered 
            (for/vector ([c classes])
                        (map (lambda (x) (vector-member x inst-id)) c))))

    (define (analyze-args prefix code postfix)
      (void))

    ))
