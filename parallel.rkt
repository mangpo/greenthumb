#lang racket

(require "path.rkt" "stat.rkt")

(provide parallel%)

(define parallel%
  (class object%
    (super-new)
    (init-field meta parser machine printer compress solver search-type mode)
    ;; search = `solver, `stoch, `hybrid
    ;; mode = `linear, `binary, `syn, `opt
    (public optimize)
    
    (define (optimize-inner code-org live-out-org live-in-org rootdir cores time-limit size 
                            assume extra-info input-file start-prog)
      (pretty-display (format "SEACH TYPE: ~a" search-type))
      ;;(define path (format "~a/driver" dir))
      (system (format "rm -r ~a" rootdir))
      (system (format "mkdir ~a" rootdir))
      
      (pretty-display ">>> select code:")
      (send printer print-syntax code-org)
      (pretty-display (format ">>> live-in-org: ~a" live-in-org))
      (pretty-display (format ">>> live-out-org: ~a" live-out-org))

      ;; Use the fewest number of registers possible.
      (define-values (code live-out live-in map-back machine-info) 
        (send compress compress-reg-space code-org live-out-org live-in-org))
      (pretty-display (format ">>> machine-info: ~a" machine-info))
      (pretty-display `(map-back ,map-back))

      (pretty-display ">>> compressed-code:")
      (send printer print-syntax code)
      ;; machine-info from compress-reg-space is only accurate for reg but not memory. This will adjust the rest of the machine info.
      (set! machine-info (send solver proper-machine-config code machine-info extra-info))
      (pretty-display (format ">>> machine-info: ~a" machine-info))
      (pretty-display (format ">>> live-in: ~a" live-in))
      (pretty-display (format ">>> live-out: ~a" live-out))

      (define dir-id 0)
      (define (optimize-partial prefix from to)
        (pretty-display (format "OPTIMIZE-PARTIAL from = ~a, to = ~a" from to))
	(define dir (format "~a/~a" rootdir dir-id))
	(system (format "mkdir ~a" dir))
	(define path (format "~a/driver" dir))
	(set! dir-id (add1 dir-id))
        
        (define (create-file id stochastic? mode)
          (define (req file)
            (format "(file \"~a/~a\")" srcpath (send meta required-module file)))
          (define required-files
            (string-join (map req '(parser machine printer stochastic solver))))
          (with-output-to-file 
              #:exists 'truncate (format "~a-~a.rkt" path id)
              (thunk
               (pretty-display (format "#lang racket"))
               (pretty-display (format "(require ~a)" required-files))
               (pretty-display (format "(define machine (new ~a))" (send meta get-class-name "machine")))
               ;; Very important to set-config before perform stochastic search.
               (pretty-display (format "(send machine set-config ~a)"
                                       (send machine set-config-string machine-info)))
               (pretty-display (format "(define printer (new ~a [machine machine]))" (send meta get-class-name "printer")))
               (pretty-display (format "(define parser (new ~a))" (send meta get-class-name "parser")))

               (if stochastic?
                   (pretty-display 
                    (format "(define search (new ~a [machine machine] [printer printer] [parser parser] [syn-mode ~a]))" 
                            (send meta get-class-name "stochastic") 
                            (equal? mode `syn)))
                   (pretty-display 
                    (format "(define search (new ~a [machine machine] [printer printer] [parser parser] [syn-mode `~a]))" 
                            (send meta get-class-name "solver") 
                            mode)))

               (pretty-display "(define prefix (send parser ast-from-string \"")
               (send printer print-syntax prefix)
               (pretty-display "\"))")
               (pretty-display "(define code (send parser ast-from-string \"")
               (send printer print-syntax (vector-copy code from to))
               (pretty-display "\"))")
               (pretty-display "(define postfix (send parser ast-from-string \"")
               (send printer print-syntax (vector-copy code to (vector-length code)))
               (pretty-display "\"))")
               (pretty-display (format "(define encoded-prefix (send printer encode prefix))"))
               (pretty-display (format "(define encoded-code (send printer encode code))"))
               (pretty-display (format "(define encoded-postfix (send printer encode postfix))"))
               (when start-prog
                     (pretty-display "(define start-code (send parser ast-from-string \"")
                     (send printer print-syntax start-prog)
                     (pretty-display "\"))")
                     (pretty-display (format "(define encoded-start-code (send printer encode start-code))"))
                     )
               (pretty-display 
                (format "(send search superoptimize encoded-code ~a ~a \"~a-~a\" ~a ~a ~a #:assume ~a #:input-file ~a #:start-prog ~a #:prefix encoded-prefix #:postfix encoded-postfix)" 
                        (send machine output-constraint-string "machine" live-out)
                        (send machine output-constraint-string "machine" live-in)
                        path id time-limit size extra-info
                        (send machine output-assume-string "machine" assume)
                        (if input-file (string-append "\"" input-file "\"") #f)
                        (if start-prog "encoded-start-code" #f)
                        ))
               
               ;;(pretty-display "(dump-memory-stats)"
               )))
        
        (define (run-file id)
          (define out-port (open-output-file (format "~a-~a.log" path id) #:exists 'truncate))
          (define-values (sp o i e) 
            (subprocess out-port #f out-port (find-executable-path "racket") (format "~a-~a.rkt" path id)))
          sp)

        (define (kill-all)
          (for ([sp (append processes-stoch processes-solver)])
               (when (equal? (subprocess-status sp) 'running)
                     (subprocess-kill sp #f))))
	
        (define (get-stats)
          (define stats
            (for/list ([id cores])
                      (let ([name (format "~a-~a.stat" path id)])
                        (and (file-exists? name)+
                             (create-stat-from-file name printer)))))=
          (with-handlers* 
           ([exn? (lambda (e) (pretty-display "Error: print stat"))])
           (when (> cores-stoch 0)
                 (print-stat-all (filter identity (take stats cores-stoch)) printer)))

          (define-values (cost len time id) (get-best-info dir))
          (when cost
                (pretty-display "=============== SUMMARY ===============")
                (pretty-display (format "cost:\t~a" cost))
                (pretty-display (format "len:\t~a" len))
                (pretty-display (format "time:\t~a" time))
                (pretty-display id)))
        
        (define cores-solver 
          (cond
           [(equal? search-type `stoch) 0]
           [(equal? search-type `solver) cores]
           [(equal? search-type `hybrid) 3]))
        (define cores-stoch (- cores cores-solver))

        ;; STEP 1: create file & run
        (define-syntax-rule (create-and-run id mode stoch?)
          (begin
            (create-file id stoch? mode)
            (run-file id)))

        (define processes-stoch
          (if (equal? search-type `hybrid)
              (let ([n 2])
                (append (for/list ([id n]) 
                                  (create-and-run id `opt #t))
                        (for/list ([id (- cores-stoch n)]) 
                                  (create-and-run (+ n id) `syn #t))))
              (for/list ([id cores-stoch]) (create-and-run id mode #t))))

        (define processes-solver
          (if (equal? search-type `hybrid)
              (list (create-and-run (+ cores-stoch 0) `partial1 #f)
                    (create-and-run (+ cores-stoch 1) `partial2 #f)
                    (create-and-run (+ cores-stoch 2) `partial3 #f))
              (for/list ([id cores-solver])
                        (create-and-run (+ cores-stoch id) mode #f))))

        (define (result)
          (define (update-stats)
            (sleep 10)
            (when (and (or (empty? processes-stoch)
                           (ormap (lambda (sp) (equal? (subprocess-status sp) 'running)) processes-stoch))
                       (or (empty? processes-solver)
                           (andmap (lambda (sp) (equal? (subprocess-status sp) 'running)) processes-solver)))
                  (get-stats)
                  (update-stats)))

          (with-handlers* 
           ([exn:break? (lambda (e) (kill-all) (sleep 5))])
           (update-stats)
           (kill-all)))
	
        ;; STEP 2: wait until timeout or optimal program is found.
        (result)

        ;; STEP 3: get best output & print
        (get-stats)
        (define output-code (send parser ast-from-file (format "~a/best.s" dir)))
        (if output-code output-code (vector-copy code from to)))

      (define code-len (vector-length code))
      (define window-size (send machine window-size))
      (define rounds (ceiling (/ code-len window-size)))
      (define size (ceiling (/ code-len rounds)))
      (define output-code (vector))
      (define mid-positions (make-vector rounds))
      (for ([round rounds])
           (let ([new-code (optimize-partial output-code (* round size) 
                                             (min (* (add1 round) size) code-len))])
             (vector-set! mid-positions round 
                          (+ (vector-length output-code) 
                             (quotient (vector-length new-code) 2)))
             (set! output-code (vector-append output-code new-code))))
      
      (when (> rounds 1)
            (pretty-display `(mid-positions mid-positions))
            (define small-size (floor (* (/ 6 10) window-size)))
            (define gap1 (- (vector-ref mid-positions 1) (vector-ref mid-positions 0)))
            (when (< gap1 small-size)
                  (vector-set! 
                   mid-positions 0
                   (max 0 (- (vector-ref mid-positions 1) small-size))))
            (define gap2 (- (vector-ref mid-positions (- rounds 1)) 
                            (vector-ref mid-positions (- rounds 2))))
            (when (< gap2 small-size)
                  (vector-set! 
                   mid-positions (- rounds 1)
                   (min (vector-length output-code)
                        (+ (vector-ref mid-positions (- rounds 2)) small-size))))
            (pretty-display `(mid-positions mid-positions))

            (set! code output-code)
            (set! output-code (vector-copy code 0 (vector-ref mid-positions 0)))
            (newline)
            (pretty-display (format ">>> another round"))
            (send printer print-syntax code)
            (for ([round (sub1 rounds)])
                 (let ([new-code 
                        (optimize-partial output-code
                                          (vector-ref mid-positions round)
                                          (vector-ref mid-positions (add1 round)))])
                   (set! output-code (vector-append output-code new-code))))
            (set! output-code (vector-append 
			       output-code 
			       (vector-copy code (vector-ref mid-positions (sub1 rounds)))))
	    )

      (let ([decompressed-code (send compress decompress-reg-space output-code map-back)])
        (send printer print-syntax decompressed-code)
        decompressed-code)
      )

    (define (optimize-filter code-org live-out live-in dir cores time-limit size 
                             assume extra-info input-file start-prog)
      (define-values (pass start stop extra-live-out) (send compress select-code code-org))
      (pretty-display `(select-code ,pass ,start ,stop ,extra-live-out))
      (if pass
          (let ([middle-output
                 (optimize-inner 
                  pass
                  (send compress combine-live-out live-out extra-live-out)
                  live-in dir cores time-limit size extra-info input-file start-prog)])
            (send compress combine-code code-org middle-output start stop))
          code-org)
      )

    ;; Inputs
    ;; input: can be in 3 different types.
    ;;   1) parsed AST, 2) .s file, 3) LLVM IR file
    ;; live-out: live-out info in custom format--- for vpe, a list of live registers
    ;;   synthesize: #t = synthesize mode, #f = optimize mode
    (define (optimize code-org live-out live-in
                      #:assume [assume #f]
                      #:extra-info [extra-info #f]
                      #:need-filter [need-filter #f]
                      #:dir [dir "output"] 
                      #:cores [cores 8]
                      #:time-limit [time-limit 3600]
                      #:size [size #f]
                      #:input-file [input-file #f]
                      #:start-prog [start-prog #f])
      (when (and (equal? search-type `hybrid) (< cores 8))
	    (raise "Cannot run hybrid search when # of cores < 12"))

      (if (> (vector-length code-org) 0)
          (if need-filter
              (optimize-filter code-org live-out live-in dir cores time-limit size assume extra-info input-file start-prog)
              (optimize-inner code-org live-out live-in dir cores time-limit size assume extra-info input-file start-prog))
          code-org))

    ))
