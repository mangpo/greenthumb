#lang racket

(require "stat.rkt")
(provide optimize)

(define (optimize code nregs nmems live-regs live-memory synthesize 
                  #:bit [bit 32] #:dir [dir "output"] #:cores [cores 12])

  (define path (format "~a/driver" dir))
  (system (format "mkdir ~a" dir))
  (system (format "rm ~a*" path))
  
  (define (create-file id)
    (define (req file)
      (format "(file \"/bard/wilma/pphothil/superopt/modular-optimizer3/~a\")" file))
    (define require-files 
      (string-join 
       (map req 
            '(ast.rkt stochastic.rkt 
                      vpe/parser.rkt vpe/state.rkt vpe/print.rkt 
                      vpe/solver-support.rkt))))
    (define live-regs-str (string-join (map number->string live-regs)))
    (define live-memory-str (string-join (map number->string live-memory)))
    (with-output-to-file #:exists 'truncate (format "~a-~a.rkt" path id)
      (thunk
       (pretty-display (format "#lang racket"))
       (pretty-display (format "(require ~a)" require-files))
       (pretty-display (format "(set-bit! ~a)" bit))
       (pretty-display (format "(set-nregs! ~a)" nregs))
       (pretty-display (format "(set-nmems! ~a)" nmems))
       (pretty-display (format "(define code (ast-from-string \"~a\"))" code))
       (pretty-display (format "(define encoded-code (encode code #f))"))
       (pretty-display (format "(stochastic-optimize encoded-code ~a (constraint ~a [reg ~a] [mem ~a]) #:synthesize ~a #:name \"~a-~a\")" 
                               nmems nmems live-regs-str live-memory-str synthesize path id))
       ;;(pretty-display "(dump-memory-stats)"
       )))

  (define (run-file id)
    (define out-port (open-output-file (format "~a-~a.log" path id) #:exists 'truncate))
    (define-values (sp o i e) 
      (subprocess out-port #f out-port (find-executable-path "racket") (format "~a-~a.rkt" path id)))
    sp)

  (define (wait)
    ;;(pretty-display "wait")
    (sleep 10)
    (define check-file 
      (with-output-to-string (thunk (system (format "ls ~a*.stat | wc -l" path)))))
    (define n 
      (string->number (substring check-file 0 (- (string-length check-file) 1))))
    ;;(pretty-display `(n ,check-file ,n ,cores ,(= n cores)))
    (pretty-display (format "There are currently ~a stats." n))
    (unless (= n cores)
            (wait)))

  (define (update-stats)
    (unless (andmap (lambda (sp) (not (equal? (subprocess-status sp) 'running))) processes)
        (get-stats)
        (sleep 10)
        (update-stats)))

  (define (get-stats)
    (define stats
      (for/list ([id cores])
                (create-stat-from-file (format "~a-~a.stat" path id))))
    (with-handlers* 
     ([exn? (lambda (e) (pretty-display "Error: print stat"))])
     (let ([output-id (print-stat-all stats)])
       (pretty-display (format "output-id: ~a" output-id)))
     )
    )

  (define processes
    (for/list ([id cores])
              (create-file id)
              (run-file id)))
  
  (with-handlers* 
   ([exn:break? (lambda (e)
                  (for ([sp processes])
                       (when (equal? (subprocess-status sp) 'running)
                             (subprocess-kill sp #f)))
                  (sleep 5)
                  )])
   (wait)
   (update-stats)
   )
  
  (get-stats))


                       
