#lang s-exp rosette

(require "ast.rkt" "machine.rkt" "decomposer.rkt")
(require rosette/solver/kodkod/kodkod)

(provide symbolic%)

(define symbolic%
  (class decomposer%
    (super-new)
    (inherit-field machine printer 
                   simulator validator
                   stat)
    (init-field [pure-symbolic #t]
                [bit (get-field bit machine)])
    (override synthesize-window)
    (public synthesize-from-sketch evaluate-inst sym-op sym-arg)
    (abstract gen-sym-inst)

    ;; Create symbolic opcode using Rosette symbolic variable.
    (define (sym-op)
      (define-symbolic* op number?)
      (assert (and (>= op 0) (< op ninsts)))
      op)
    
    ;; Create symbolic operand using Rosette symbolic variable.
    (define (sym-arg)
      (define-symbolic* arg number?)
      arg)
    
    (define ninsts (vector-length (get-field inst-id machine)))

    (define (interpret-spec spec start-state assumption)
      (send validator assume start-state assumption)
      ;;(pretty-display "interpret spec")
      (define res (send simulator interpret spec start-state))
      ;;(pretty-display "done interpret spec")
      res
      )
    
    (define (sym-input)
      (define-symbolic* input number?)
      input
      )

    ;; Superoptimize program
    ;; >>> INPUTS >>>
    ;; spec: input program specification
    ;; sketch: skeleton of the output program
    ;; constraint: constraint on the output state
    ;; cost: upperbound (exclusive) of the cost of the output program, #f is no upperbound
    ;; assume: input assumption
    (define (synthesize-window spec sketch prefix postfix constraint extra 
			       cost time-limit
			       #:hard-prefix [hard-prefix (vector)] 
			       #:hard-postfix [hard-postfix (vector)]
			       #:assume [assumption (send machine no-assumption)])
      ;;(send machine analyze-opcode prefix spec postfix)
      (send machine reset-inst-pool)
      (set! sketch (for/vector ([x sketch]) (if (inst-op x) x (gen-sym-inst))))
      (if pure-symbolic 
          (synthesize-from-sketch 
           (vector-append prefix spec postfix) 
	   (vector-append prefix sketch postfix)
	   constraint extra cost time-limit
            #:hard-prefix hard-prefix #:hard-postfix hard-postfix 
            #:assume assumption)
          (synthesize-window-mix 
           spec sketch prefix postfix constraint extra cost time-limit
            #:hard-prefix hard-prefix #:hard-postfix hard-postfix 
            #:assume assumption)))

    ;; Randomly guess opcodes, then uses symbolic search to solve the rest.
    ;; Caution: this function mutates sketch at symbolic instructions
    (define (synthesize-window-mix spec sketch prefix postfix constraint extra 
                                   [cost #f] [time-limit 3600]
                                   #:hard-prefix [hard-prefix (vector)] 
                                   #:hard-postfix [hard-postfix (vector)]
                                   #:assume [assumption (send machine no-assumption)])

      ;; TODO: Break when best correct program is updated.
      (define (loop)
        (partial-random-sketch sketch)
        (send printer print-struct sketch)

        (with-handlers* 
         ([exn:fail? 
           (lambda (e)
             (if  (or (regexp-match #rx"synthesize: synthesis failed" (exn-message e))
                      (regexp-match #rx"assert: cost" (exn-message e)))
                  (loop)
                  (raise e)))])
         (synthesize-from-sketch 
          (vector-append prefix spec postfix)
	  (vector-append prefix sketch postfix)
	  constraint extra cost time-limit
          #:hard-prefix hard-prefix #:hard-postfix hard-postfix 
          #:assume assumption)
         ))

      (loop))

    (define (partial-random-sketch sketch)
      (for ([i sketch])
           (set-inst-op! i (random ninsts))))


    ;; Query kodkod or SMT solver to find a candidate.
    (define (synthesize-from-sketch spec sketch constraint extra 
				    [cost #f]
				    [time-limit 3600]
                                    #:hard-prefix [hard-prefix (vector)] 
                                    #:hard-postfix [hard-postfix (vector)]
				    #:assume [assumption (send machine no-assumption)])
      (send (current-solver) shutdown)
      (current-solver (new kodkod%))
      (pretty-display "SUPERPOTIMIZE:")
      (pretty-display (format "solver = ~a" (current-solver)))
      (when debug
            (send printer print-struct hard-prefix)
	    (newline)
            (send printer print-struct spec)
	    (newline)
            (send printer print-struct hard-postfix)
	    (newline)
	    )

      (current-bitwidth bit)
      

      (define start-state (send machine get-state sym-input extra))
      (define spec-state #f)
      (define sketch-state #f)
      (define spec-cost #f)
      (define sketch-cost #f)
      ;; (pretty-display "========= start state")
      ;; (send machine display-state start-state)
      
      (define (interpret-spec!)
        (when debug (pretty-display "========== interpret spec"))
        (set! spec-state 
              (interpret-spec (vector-append hard-prefix spec hard-postfix)
                              start-state assumption)))
      
      (define (compare-spec-sketch)
        (when debug (pretty-display "=========== interpret sketch"))
        (set! sketch-state 
              (send simulator interpret (vector-append hard-prefix sketch hard-postfix)
                    start-state spec-state))
        (when debug (pretty-display "check output"))
        ;; (set! spec-cost (send simulator performance-cost spec))
        (set! sketch-cost (send simulator performance-cost sketch))
	(pretty-display `(cost-compare ,cost ,sketch-cost))
        (when cost (assert (< sketch-cost cost) "cost"))
        (send validator assert-state-eq spec-state sketch-state constraint)
        (when debug (pretty-display "=========== done compare-spec-sketch"))
        )
      
      ;; Collect input variables and contruct their init values.
      (define-values (sym-vars inputs)
        (send validator generate-inputs-inner 2 spec start-state assumption))

      (when debug
            (pretty-display "Test calculate performance-cost with symbolic instructions...")
            (send simulator performance-cost sketch)
            (pretty-display "Test simulate with symbolic instructions...")
            (send simulator interpret sketch start-state)
            (pretty-display "Passed!"))
      
      (define model 
        (timeout
         time-limit
         (synthesize 
          #:forall sym-vars
          #:init (drop inputs 1)
          #:assume (interpret-spec!)
          #:guarantee (compare-spec-sketch))
         )
        )
      
      (when debug (pretty-display ">>> done synthesize"))
      (define final-program (evaluate-program sketch model))
      (when debug (pretty-display ">>> done evaluate program"))
      (define final-cost (evaluate sketch-cost model))
      
      (pretty-display ">>> superoptimize-output")
      (set! final-program (send machine clean-code final-program hard-prefix))
      ;;(send printer print-struct final-program)
      (send printer print-syntax (send printer decode final-program)) (newline)
      (pretty-display (format "limit cost = ~a" cost))
      (pretty-display (format "new cost = ~a" final-cost))
      (pretty-display "=====================================")
      (clear-asserts)
      (clear-terms!)

      ;; Print to file
      (send stat update-best-correct final-program final-cost)

      (values final-program final-cost)
      )
    
    ;; Evaluate a symbolic instruction to a concrete instruction according to a given model.
    (define (evaluate-inst x model)
      (inst (evaluate (inst-op x) model)
            (evaluate (inst-args x) model)))
    
    (define (evaluate-program code model)
      (traverse code inst? (lambda (x) (evaluate-inst x model))))

    ))
