#lang racket

(require "ast.rkt" "stat.rkt" "machine.rkt")

(provide stochastic%)

(define stochastic%
  (class object%
    (super-new)
    (public superoptimize inst-copy-with-op inst-copy-with-args
            get-mutations mutate filter-live
            mutate-opcode mutate-operand
            mutate-operand-specific mutate-other
            random-instruction print-mutation-info
	    random-args-from-op
            get-operand-live update-live)
    (abstract correctness-cost get-arg-ranges)
              
;;;;;;;;;;;;;;;;;;;;; Parameters ;;;;;;;;;;;;;;;;;;;
    (init-field machine printer syn-mode
                [solver #f]
                [simulator #f]
                [stat (new stat% [printer printer])]
                [input-file #f]
                [w-error 9999]
                [beta 1]
                [nop-mass 0.8]
                [ntests 16]
                [mutate-dist 
                 #hash((opcode . 1) (operand . 1) (swap . 1) (instruction . 1))]
                [live-in #f])
    
    (define nop-id (send machine get-inst-id `nop))
    (define inst-id (get-field inst-id machine))
    (define classes (get-field classes machine))
  
    (define (print-mutation-info)
      (for ([op inst-id])
           (pretty-display `(opcode ,op ,(get-mutations op)))))
  
;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;
    (define (inst-copy-with-op x op) (struct-copy inst x [op op]))
    (define (inst-copy-with-args x args) (struct-copy inst x [args args]))

    (define (superoptimize spec constraint this-live-in
                           name time-limit size [extra-info #f]
                           #:assume [assumption (send machine no-assumption)]
                           #:input-file [input-file #f]
                           #:start-prog [start #f])
      (set! live-in (get-operand-live this-live-in))
      ;; Generate testcases
      (when debug 
            (pretty-display ">>> Phase 0: print mutation info")
            (print-mutation-info)
            ;;(raise "done")
            (pretty-display ">>> Phase 1: generate input states")
            (if input-file
                (pretty-display ">>> Inputs from file")
                (pretty-display ">>> Auto generate"))
            )
      (define inputs 
        (if input-file
            (map cdr (send machine get-states-from-file input-file))
            (send solver generate-input-states ntests spec assumption extra-info)))

      (when debug
            (for ([i inputs])
                 (send machine display-state i))
            (pretty-display ">>> Phase 2: generate output states"))
      (define outputs (map (lambda (x) (send simulator interpret spec x #:dep #t)) inputs))
      (when debug
            (for ([i outputs])
                 (send machine display-state i))
            )

      ;; MCMC sampling
      (define-syntax-rule (get-sketch) 
        (random-insts (if size size (vector-length spec))))
      (set-field! best-correct-program stat spec)
      (set-field! best-correct-cost stat (send simulator performance-cost spec))
      (set-field! name stat name)
      (mcmc-main spec 
                 (cond
                  [start start]
                  [syn-mode (get-sketch)]
                  [else spec])
                 inputs outputs constraint assumption time-limit extra-info)
      )

    (define (remove-nops code)
      (list->vector 
       (filter (lambda (x) (not (equal? (inst-op x) nop-id)))
               (vector->list code))))

    (define (get-operand-live constraint) #f)

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
          
    (define (random-insts n)
      (define my-live-in live-in)
      (for/vector ([i n]) 
        (let ([x (random-instruction my-live-in)])
          (pretty-display `(x ,x))
          (set! my-live-in (update-live my-live-in x)) ;; TODO
          x)))

    (define (filter-live range live)
      (if live
          (vector-filter (lambda (x) (member x live)) range)
          range))

    ;; Generic across architectures
    (define (mutate-swap index entry p)
      (define new-p (vector-copy p))
      (define index2 (random-from-list-ex (range (vector-length p)) index))
      (when debug
            (pretty-display " >> mutate swap")
            (pretty-display (format " --> swap = ~a" index2)))
      (vector-set! new-p index (vector-ref new-p index2))
      (vector-set! new-p index2 entry)
      (send stat inc-propose `swap)
      new-p)

    ;; Generic across architectures
    (define (mutate-opcode index entry p)
      (define opcode-id (inst-op entry))
      (define opcode-name (vector-ref inst-id opcode-id))
      (define class-id (send machine get-class-id opcode-name))
      (define class (and class-id (vector-ref classes class-id)))
      (when debug
            (pretty-display (format " >> mutate opcode"))
            (pretty-display (format " --> org = ~a ~a" opcode-name opcode-id))
            (pretty-display (format " --> class = ~a" class)))
      (cond
       [class
        (set! class (remove* (list #f) (map (lambda (x) (send machine get-inst-id x)) class)))
        (define new-opcode-id (random-from-list-ex class opcode-id))
        (define new-p (vector-copy p))
        (when debug
              (pretty-display (format " --> new = ~a ~a" (send machine get-inst-name new-opcode-id) new-opcode-id)))
        (vector-set! new-p index (inst-copy-with-op entry new-opcode-id))
        (send stat inc-propose `opcode)
        new-p]

       [else (mutate p)]))

    (define (mutate-operand index entry p)
      (define opcode-id (inst-op entry))
      (define opcode-name (vector-ref inst-id opcode-id))
      (define args (vector-copy (inst-args entry)))
      (define my-live-in live-in)
      (for ([i index])
           (set! my-live-in (update-live my-live-in (vector-ref p i))))
      (define ranges (get-arg-ranges opcode-name entry my-live-in))
      (cond
       [(> (vector-length ranges) 0)
        (define args (vector-copy (inst-args entry)))
        (define okay-indexes (list))
        (for ([range ranges]
              [i (vector-length ranges)])
             (when (> (vector-length range) 1) (set! okay-indexes (cons i okay-indexes))))
        (define change (random-from-list okay-indexes))
        (define valid-vals (vector-ref ranges change))
        (define new-val 
          (if (vector? valid-vals)
              (random-from-vec-ex valid-vals (vector-ref args change))
              (mutate-operand-specific opcode-name args change live-in)))
        
        (define new-p (vector-copy p))
        (when debug
              (pretty-display (format " --> org = ~a ~a" opcode-name args))
              (pretty-display (format " --> new = [~a]->~a)" change new-val)))
        (vector-set! args change new-val)
        (vector-set! new-p index (inst-copy-with-args entry args))
        (send stat inc-propose `operand)
        new-p]

       [else (mutate p)]))

    (define (mutate-instruction index entry p)
      (define new-p (vector-copy p))
      (define opcode-id (inst-op entry))
      (define opcode-name (vector-ref inst-id opcode-id))
      (define new-opcode-id
        (if (and (not (equal? opcode-id nop-id)) (< (random) nop-mass))
            (begin (send stat inc-propose `nop) 
                   nop-id)
            (begin (send stat inc-propose `inst) 
                   (random-from-list-ex (range (vector-length inst-id)) opcode-id))))
      (when debug
            (pretty-display (format " >> mutate instruction ~a" (vector-ref inst-id new-opcode-id))))
      (define my-live-in live-in)
      (for ([i index])
           (set! my-live-in (update-live my-live-in (vector-ref p i))))
      (define new-entry (random-instruction my-live-in new-opcode-id))
      (vector-set! new-p index new-entry)
      new-p)
    
    (define (random-instruction live-in [opcode-id (random (vector-length inst-id))])
      (define opcode-name (vector-ref inst-id opcode-id))
      (define args (random-args-from-op opcode-name live-in))
      (inst opcode-id args))
    
    (define (random-args-from-op opcode-name live-in)
      (define ranges (get-arg-ranges opcode-name #f live-in))
      (when debug (pretty-display (format " --> ranges ~a" ranges)))
      (for/vector ([i (vector-length ranges)])
                (random-from-vec (vector-ref ranges i))))
    
    (define (mutate-operand-specific opcode-name args index)
      (raise "mutate-operand-specific: unimplemented"))

    (define (mutate-other index entry p stat type)
      (raise "mutate-other: unimplemented"))

    (define (get-mutations opcode-name)
      (define mutations '(instruction swap))
      (unless (equal? opcode-name `nop)
              (set! mutations (cons `opcode mutations))
              (set! mutations (cons `operand mutations)))
      mutations)

    (define (get-mutate-type opcode-name)
      (define mutations (get-mutations opcode-name))
      
      ;; TODO: redundant
      (when debug (pretty-display `(mutations ,mutations)))
      (define sum 0)
      (define prop (map (lambda (x) 
                          (let ([v (hash-ref mutate-dist x)])
                            (set! sum (+ sum v))
                            sum))
                        mutations))
      (set! prop (map (lambda (x) (exact->inexact (/ x sum))) prop))
      (define rand (random))
      (define (loop name-list prop-list)
        (if (<= rand (car prop-list))
            (car name-list)
            (loop (cdr name-list) (cdr prop-list))))
      (loop mutations prop))

    (define (mutate p)
      (define vec-len (vector-length p))
      (define index (random vec-len))
      (define entry (vector-ref p index))
      (define opcode-id (inst-op entry))
      (define opcode-name (vector-ref inst-id opcode-id))
      (define type (get-mutate-type opcode-name))
      (when debug 
            (pretty-display 
             (format " >> mutate = ~a, index = ~a" type index)))

      (cond
       [(equal? type `opcode)      (mutate-opcode index entry p)]
       [(equal? type `operand)     (mutate-operand index entry p)]
       [(equal? type `instruction) (mutate-instruction index entry p)]
       [(equal? type `swap)        (mutate-swap index entry p)]
       [else                       (mutate-other index entry p type)]))
    

    (define (mcmc-main target init inputs outputs constraint assumption time-limit extra-info)
      (pretty-display ">>> start MCMC sampling")
      (pretty-display ">>> Phase 3: stochastic search")
      (pretty-display "start-program:")
      (send printer print-struct init)
      (define syn-mode #t)

      (define (cost-one-input program input output)
        (with-handlers* 
         ([exn:break? (lambda (e) (raise e))]
          [exn? (lambda (e) 
                  (when debug 
                        (pretty-display "Error!")
                        (pretty-display (exn-message e)))
                  #f)]
          )
         (let* ([t1 (current-milliseconds)]
                [program-out (send simulator interpret program input)]
                [t2 (current-milliseconds)]
                [ret (correctness-cost output program-out constraint)]
                [t3 (current-milliseconds)]
                )
           (send stat simulate (- t2 t1))
           (send stat check (- t3 t2))
           ret
           )))
      
      (define (cost-all-inputs program okay-cost)
        ;; (define correct 0)
        (define change-mode #f)
        ;; (for ([input inputs]
        ;;       [output outputs])
        ;;      (when correct
        ;;            (set! correct (+ correct (cost-one-input program input output)))
        ;;            (when (> correct okay-cost)
        ;;                  (set! correct #f))))

        (define (loop correct inputs outputs)
          (when debug (pretty-display `(correct ,correct)))
          (cond
           [(> correct okay-cost) #f]
           [(empty? inputs) correct]
           [else
            (let ([cost (cost-one-input program (car inputs) (car outputs))])
              (if cost 
                  (loop (+ correct cost) (cdr inputs) (cdr outputs))
                  w-error))]
           ))

        (define correct
          (and (send simulator is-valid? program)
               (loop 0 inputs outputs)))
        (when debug (pretty-display `(final-correct ,correct)))

        (define ce #f)
        (when (and (number? correct) (= correct 0))
              (send stat inc-validate)
              (define t1 (current-milliseconds))
              (set! ce (send solver counterexample target program constraint extra-info
                             #:assume assumption))
              (if ce 
                  (begin
                    (set! correct 1)
                    (set! inputs (cons ce inputs))
                    (set! outputs (cons (send simulator interpret target ce #:dep #t) 
                                        outputs))
                    (pretty-display (format "Add counterexample. Total = ~a." (length inputs)))
                    (send machine display-state ce)
                    )
                  (begin
                    (send stat inc-correct)
                    (when syn-mode (set! change-mode #t) (set! syn-mode #f))
                                        ;(send stat inc-correct)
                    ))
              
              (define t2 (current-milliseconds))
              (send stat validate (- t2 t1))
              )
        
        (if (number? correct)
            (let ([total-cost 
                   (if syn-mode correct (+ (send simulator performance-cost program) correct))])
              (when debug (pretty-display `(total-cost ,total-cost)))
              (when (< total-cost (get-field best-cost stat))
                    (send stat update-best program total-cost)
                    )
              (when (and (= correct 0) (= total-cost (get-field best-correct-cost stat)))
                    (send stat update-best-correct-program program))
              (when (and (= correct 0) (< total-cost (get-field best-correct-cost stat)))
                    (pretty-display "NEW! best-correct-program")
                    (pretty-display "program-eq? --> true")
                    (pretty-display "target:")
                    (send printer print-struct target)
                    (pretty-display "candidate:")
                    (send printer print-struct program)
                    (send stat update-best-correct program total-cost)
                    )
              (if (or (<= total-cost okay-cost) change-mode) 
                  ;; return (correctness-cost . correct)
                  (cons total-cost (= correct 0))
                  (cons #f #f)))
            (cons #f #f))
        )

      (define (accept-cost current-cost)
        (- current-cost (/ (log (random)) beta)))

      ;; Main loop
      (define (iter current current-cost)
        (when debug (pretty-display ">>> iter >>>"))
        (send stat inc-iter current-cost)
        (define t1 (current-milliseconds))
        (define proposal (mutate current))
        (when debug (pretty-display ">>> done mutate >>>"))
        (define t2 (current-milliseconds))
        (send stat mutate (- t2 t1))
        (when debug
              (pretty-display (format "================ Current (syn=~a) =================" syn-mode))
              (send printer print-struct current)
              ;; (define cost (cost-all-inputs current (arithmetic-shift 1 32)))
              ;; (pretty-display (format "actual cost: ~a" cost))
              (pretty-display (format "================ Propose (syn=~a) =================" syn-mode))
              (send printer print-struct proposal)
              )
        (define n-inputs (length inputs))
        (define okay-cost (accept-cost current-cost))
        (when debug (pretty-display ">>> start simulate"))
        (define cost-correct (cost-all-inputs proposal okay-cost))
        (when debug (pretty-display ">>> finish simulate"))
        (define proposal-cost (car cost-correct))
        (when debug
              (pretty-display (format "current cost: ~a" current-cost))
              (pretty-display (format "okay cost: ~a" okay-cost))
              (pretty-display (format "proposal cost: ~a" proposal-cost)))

        (if proposal-cost
            (begin
              (when debug
                    (pretty-display "================ ACCEPT! =================")
                    (print-struct proposal))
              (send stat inc-accept)
              (when (> proposal-cost current-cost) 
                    (send stat inc-accept-higher))
              ;; Adjust cost due to new counterexample
              (when (> (length inputs) n-inputs)
                    (when debug (display (format "Adjust proposal cost from ~a " proposal-cost)))
		    (define more-cost (cost-one-input proposal (car inputs) (car outputs)))
		    (if more-cost
			(set! proposal-cost (sub1 (+ proposal-cost more-cost)))
			(set! proposal-cost w-error))
                    (when debug (pretty-display (format "to ~a." proposal-cost)))
                    )
              (iter (if (cdr cost-correct) 
                        (remove-nops proposal) 
                        proposal) 
                    proposal-cost))
            (begin
              ;; Adjust cost due to new counterexample
              (when (> (length inputs) n-inputs)
                    (when debug (display (format "Adjust current cost from ~a " current-cost)))
		    (define more-cost (cost-one-input current (car inputs) (car outputs)))
		    (if more-cost
			(set! current-cost (+ current-cost more-cost))
			(set! current-cost w-error))
                    (when debug (pretty-display (format "to ~a." current-cost)))
                    )
              (iter current current-cost))
            ))


      (with-handlers 
       ([exn:break? (lambda (e) (send stat print-stat-to-file))])
       
       (timeout time-limit
                (iter init 
                      ;; cost-all-inputs can return #f if program is invalid
                      (or (car (cost-all-inputs init w-error))
                          w-error)
                      ))
       )
      )
    ))
