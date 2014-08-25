#lang racket

(require "stat.rkt")

(provide parallel%)

(define parallel%
  (class object%
    (super-new)
    (init-field meta parser machine printer compress solver)
    (public optimize)
    
    (define (optimize-inner code-org live-out-org synthesize dir cores time-limit size)
      (define path (format "~a/driver" dir))
      (system (format "mkdir ~a" dir))
      (system (format "rm ~a*" path))
      
      (pretty-display ">>> select code:")
      (send printer print-syntax code-org)
      (pretty-display (format ">>> live-out-org: ~a" live-out-org))

      ;; Use the fewest number of registers possible.
      (define-values (code live-out map-back machine-info) 
        (send compress compress-reg-space code-org live-out-org))

      ;; machine-info from compress-reg-space is only accurate for reg but not memory. This will adjust the rest of the machine info.
      (set! machine-info (send solver proper-machine-config code machine-info))
      (pretty-display ">>> compressed-code:")
      (send printer print-syntax code)
      (pretty-display (format ">>> machine-info: ~a" machine-info))
      (pretty-display (format ">>> live-out: ~a" live-out))

      (define (create-file id)
        (define (req file)
          (format "(file \"/bard/wilma/pphothil/superopt/modular-optimizer3/~a\")" (send meta required-module file)))
        (define required-files
          (string-join (map req '(parser machine printer stochastic))))
        (with-output-to-file #:exists 'truncate (format "~a-~a.rkt" path id)
          (thunk
           (pretty-display (format "#lang racket"))
           (pretty-display (format "(require ~a)" required-files))
           (pretty-display (format "(define machine (new ~a))" (send meta get-class-name "machine")))
           (pretty-display (format "(send machine set-config ~a)"
                                   (send machine set-config-string machine-info)))
           (pretty-display (format "(define printer (new ~a [machine machine]))" (send meta get-class-name "printer")))
           ;; TODO: can substitute with solver
           (pretty-display (format "(define stochastic (new ~a [machine machine] [printer printer]))" (send meta get-class-name "stochastic")))
           (pretty-display (format "(define parser (new ~a))" (send meta get-class-name "parser")))
           (pretty-display "(define code (send parser ast-from-string \"")
           (send printer print-syntax code)
           (pretty-display "\"))")
           (pretty-display (format "(define encoded-code (send printer encode code))"))
           (pretty-display (format "(send stochastic superoptimize encoded-code ~a ~a \"~a-~a\" ~a ~a)" 
                                   (send machine output-constraint-string "machine" live-out)
                                   synthesize path id time-limit size))
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
                    (create-stat-from-file (format "~a-~a.stat" path id) printer)))
        (with-handlers* 
         ([exn? (lambda (e) (pretty-display "Error: print stat"))])
         (let ([output-id (print-stat-all stats printer)])
           (pretty-display (format "output-id: ~a" output-id))
           output-id
           )
         )
        )
      
      (define processes
        (for/list ([id cores])
                  (create-file id)
                  (run-file id)))
      
      (with-handlers* 
       ([exn:break? 
         (lambda (e)
           (for ([sp processes])
                (when (equal? (subprocess-status sp) 'running)
                      (subprocess-kill sp #f)))
           (sleep 5)
           )])
       (wait)
       (update-stats)
       )
      
      (define id (get-stats))
      (define output-code (send parser ast-from-file (format "~a-~a.best" path id)))
      (if output-code
          (let ([decompressed-code (send compress decompress-reg-space output-code map-back)])
            (send printer print-syntax decompressed-code)
            decompressed-code)
          code-org)
      )

    (define (optimize-filter code-org live-out synthesize dir cores time-limit size)
      (define-values (pass start stop extra-live-out) (send compress select-code code-org))
      (pretty-display `(select-code ,pass ,start ,stop ,extra-live-out))
      (if pass
          (let ([middle-output
                 (optimize-inner 
                  pass
                  (send compress combine-live-out live-out extra-live-out)
                  synthesize dir cores time-limit size)])
            (send compress combine-code code-org middle-output start stop))
          code-org)
      )

    ;; Inputs
    ;; input: can be in 3 different types.
    ;;   1) parsed AST, 2) .s file, 3) LLVM IR file
    ;; live-out: live-out info in custom format--- for vpe, a list of live registers
    ;;   synthesize: #t = synthesize mode, #f = optimize mode
    (define (optimize code-org live-out synthesize 
                      #:need-filter [need-filter #f]
                      #:dir [dir "output"] 
                      #:cores [cores 12]
                      #:time-limit [time-limit 3600]
                      #:size [size #f])
      (pretty-display `(optimize))

      (if (> (vector-length code-org) 2)
          (if need-filter
              (optimize-filter code-org live-out synthesize dir cores time-limit size)
              (optimize-inner code-org live-out synthesize dir cores time-limit size))
          code-org))

    ))