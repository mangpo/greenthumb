#lang racket

(require "inst.rkt")
(provide generate-inputs generate-outputs-steps calculate-cost
         generate-tree calculate-cost2)

(define nsteps 3)
(define repeat 100)

(define (get-states-from-file machine file)
  (send machine get-states-from-file file))

(define (generate-inputs code extra-info dir
                         machine printer solver #:assume [assume #f])
  
  (pretty-display ">>> Phase 1: generate input states")
  (define inputs (send solver generate-input-states 16 code assume extra-info))
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
                                machine printer simulator stochastic)
  (define (interpret code input)
    (with-handlers*
     ([exn:state? (lambda (e)
                    (cons #f (exn:state-state e)))])
     (cons #t (send simulator interpret code input))))

  (define inputs (get-states-from-file machine (format "~a/inputs" dir)))
  (define correct-outputs (map (lambda (x) (interpret code (cdr x))) inputs))
  (system (format "rm -r ~a/~a" dir subdir))
  (system (format "mkdir ~a/~a" dir subdir))
  
  (define (print-state-step code outputs step count)
    (with-output-to-file #:exists 'append (format "~a/~a/program_~a" dir subdir count)
      (thunk (send printer print-syntax (send printer decode code))
             (newline)))
    (with-output-to-file (format "~a/~a/outputs_~a_~a" dir subdir step count)
      (thunk
       (for ([output outputs])
         (send machine display-state-text output)))))

  (define (get-new-code code [round 0])
    (define new-code (send stochastic mutate code))
    (define outputs (map (lambda (x) (interpret new-code (cdr x))) inputs))
    ;; (pretty-display `(get-new-code ,(andmap car outputs)))
    ;; (send printer print-syntax (send printer decode new-code)) (newline)
    (if (or (andmap car outputs) (> round 10))
        (values new-code outputs)
        (get-new-code code (add1 round))))
    
  (define (iter code step count)
    ;; (pretty-display `(iter ,count ,step))
    (define-values (new-code outputs) (get-new-code code))
    (print-state-step new-code outputs step count)
    (when (< step nsteps)
          (iter new-code (add1 step) count)))

  (send printer print-syntax (send printer decode code)) (newline)
  (pretty-display ">>> Phase 2: generate output states")
  (print-state-step code correct-outputs 0 0)
  (for ([i (in-range 1 (add1 repeat))])
       (iter code 1 i)))

(define (calculate-cost dir name live-out
                        machine stochastic)

  (define constraint (send machine output-constraint live-out))
  (define ref-states (get-states-from-file machine (format "~a/outputs_~a_~a" dir 0 0)))
  
  (define (compare states)
    (min
     10000
     (for/sum ([state1 ref-states]
               [state2 states])
              (if (car state2)
                  (send stochastic correctness-cost (cdr state1) (cdr state2) constraint)
                  10000))))

  (system (format "mkdir ~a/~a" dir name))
  (for ([count (in-range 1 (add1 repeat))])
       (with-output-to-file (format "~a/~a/cost-~a.csv" dir name count)
         (thunk
          (for* ([step (range 1 (add1 nsteps))])
                (let ([states (get-states-from-file 
                               machine 
                               (format "~a/outputs_~a_~a" dir step count))])
                  (pretty-display (format "~a,~a" step (compare states)))))))))

(define (calculate-cost2 dir name live-out n
                         machine stochastic)

  (define constraint (send machine output-constraint live-out))
  (define ref-states (get-states-from-file machine (format "~a/outputs_0" dir)))
  
  (define (compare states)
    (min
     10000
     (for/sum ([state1 ref-states]
               [state2 states])
              (if (car state2)
                  (send stochastic correctness-cost (cdr state1) (cdr state2) constraint)
                  10000))))

  (with-output-to-file (format "~a/costs-~a" dir name)
    (thunk
     (for* ([id n])
           (let ([states (get-states-from-file  machine (format "~a/outputs_~a" dir id))])
             (pretty-display (compare states)))))))
  

(define (generate-tree code dir subdir degree n
                       machine printer simulator stochastic)
  (define (interpret code input)
    (with-handlers*
     ([exn:state? (lambda (e)
                    (cons #f (exn:state-state e)))])
     (cons #t (send simulator interpret code input))))

  (system (format "rm -r ~a/~a" dir subdir))
  (system (format "mkdir ~a/~a" dir subdir))
  (define inputs (get-states-from-file machine (format "~a/inputs" dir)))
  (define correct-outputs (map (lambda (x) (interpret code (cdr x))) inputs))
  
  (define (print-state code outputs id)
    (with-output-to-file #:exists 'append (format "~a/~a/programs" dir subdir)
      (thunk (send printer print-syntax (send printer decode code))
             (newline)))
    (with-output-to-file (format "~a/~a/outputs_~a" dir subdir id)
      (thunk
       (for ([output outputs])
         (send machine display-state-text output)))))

  (define (get-new-code code id [round 0])
    (define new-code (send stochastic mutate code))
    (define outputs (map (lambda (x) (interpret new-code (cdr x))) inputs))
    ;; (pretty-display `(get-new-code ,(andmap car outputs)))
    ;; (send printer print-syntax (send printer decode new-code)) (newline)
    (if (or (andmap car outputs) (> round 10))
        (begin
          (print-state new-code outputs id)
          new-code)
        (get-new-code code id (add1 round))))
  
  (define (loop code-list id-list count)
    (define my-code (car code-list))
    (define my-id (car id-list))
    (define new-codes (for/list ([i degree]) (get-new-code my-code (+ count i))))
    (define new-ids (for/list ([i degree]) (+ count i)))
    (with-output-to-file #:exists 'append (format "~a/~a/tree" dir subdir)
      (thunk 
       (for ([i degree]) (pretty-display (format "~a ~a" my-id (+ count i))))))
    (when (< count n)
          (loop (append (cdr code-list) new-codes)
                (append (cdr id-list) new-ids)
                (+ count degree))))

  (print-state code correct-outputs 0)
  (loop (list code) (list 0) 1))
  
