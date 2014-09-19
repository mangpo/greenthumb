#lang racket

(require "ast.rkt")
(provide generate-inputs generate-outputs-steps calculate-cost)

(define nsteps 100)
(define repeat 10)

(define (get-states-from-file machine file)
  (send machine get-states-from-file file))

(define (generate-inputs code extra-info dir
                         machine printer solver)
  
  (pretty-display ">>> Phase 1: generate input states")
  (define inputs (send solver generate-input-states 16 code #f extra-info))
  (system (format "rm -r ~a" dir))
  (system (format "mkdir ~a" dir))

  (with-output-to-file (format "~a/program" dir)
    (thunk
     (send printer print-syntax (send printer decode code))))

  (with-output-to-file (format "~a/inputs" dir)
    (thunk
     (for ([input inputs])
       (send machine display-state-text (cons #t input))))))

(define (generate-outputs-steps code dir subdir
                                machine simulator stochastic)

  (define inputs (get-states-from-file machine (format "~a/inputs" dir)))
  (system (format "rm -r ~a/~a" dir subdir))
  (system (format "mkdir ~a/~a" dir subdir))

  (define (interpret code input)
    (with-handlers*
     ([exn:state? (lambda (e)
                    (cons #f (exn:state-state e)))])
     (cons #t (send simulator interpret code input))))
  
  (define (print-state-step code step count)
    (define outputs (map (lambda (x) (interpret code (cdr x))) inputs))
    (with-output-to-file (format "~a/~a/~a_~a" dir subdir step count)
      (thunk
       (for ([output outputs])
         (send machine display-state-text output)))))
    
  (define (iter code step count)
    (define new-code (send stochastic mutate code))
    (print-state-step new-code step count)
    (when (< step nsteps)
          (iter new-code (add1 step) count)))

  (pretty-display ">>> Phase 2: generate output states")
  (print-state-step code 0 0)
  (for ([i (in-range repeat)])
       (iter code 1 i)))

(define (calculate-cost dir subdir live-out
                        machine stochastic)

  (define constraint (send machine output-constraint live-out))
  (define ref-states (get-states-from-file machine (format "~a/~a/~a_~a" dir subdir 0 0)))
  
  (define (compare states)
    (min
     10000
     (for/sum ([state1 ref-states]
               [state2 states])
              (if (car state2)
                  (send stochastic correctness-cost (cdr state1) (cdr state2) constraint)
                  10000))))

  (with-output-to-file (format "~a/~a/cost-v2.csv" dir subdir)
    (thunk
     (for* ([step (range 1 nsteps)]
            [count repeat])
           (let ([states (get-states-from-file 
                          machine 
                          (format "~a/~a/~a_~a" dir subdir step count))])
             (pretty-display (format "~a,~a" step (compare states))))))))
  
  