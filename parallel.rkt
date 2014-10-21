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
    
    (define (optimize-inner code-org live-out-org dir cores time-limit size 
                            assume extra-info input-file start-prog)
      (pretty-display (format "SEACH TYPE: ~a" search-type))
      (define path (format "~a/driver" dir))
      (system (format "mkdir ~a" dir))
      (system (format "rm ~a/*" dir))
      
      (pretty-display ">>> select code:")
      (send printer print-syntax code-org)
      (pretty-display (format ">>> live-out-org: ~a" live-out-org))

      ;; Use the fewest number of registers possible.
      (define-values (code live-out map-back machine-info) 
        (send compress compress-reg-space code-org live-out-org))
      (pretty-display (format ">>> machine-info: ~a" machine-info))

      (pretty-display ">>> compressed-code:")
      (send printer print-syntax code)
      ;; machine-info from compress-reg-space is only accurate for reg but not memory. This will adjust the rest of the machine info.
      (set! machine-info (send solver proper-machine-config code machine-info extra-info))
      (pretty-display (format ">>> machine-info: ~a" machine-info))
      (pretty-display (format ">>> live-out: ~a" live-out))

      (define (create-file id stochastic? mode)
        (define (req file)
          (format "(file \"~a/~a\")" srcpath (send meta required-module file)))
        (define required-files
          (string-join (map req '(parser machine printer stochastic solver))))
        (with-output-to-file #:exists 'truncate (format "~a-~a.rkt" path id)
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
	       (pretty-display (format "(define search (new ~a [machine machine] [printer printer] [parser parser] [syn-mode ~a]))" 
                                       (send meta get-class-name "stochastic") 
                                       (equal? mode `syn)))
	       (pretty-display (format "(define search (new ~a [machine machine] [printer printer] [parser parser] [syn-mode `~a]))" 
                                       (send meta get-class-name "solver") 
                                       mode)))

           (pretty-display "(define code (send parser ast-from-string \"")
           (send printer print-syntax code)
           (pretty-display "\"))")
           (pretty-display (format "(define encoded-code (send printer encode code))"))
           (when start-prog
                 (pretty-display "(define start-code (send parser ast-from-string \"")
                 (send printer print-syntax start-prog)
                 (pretty-display "\"))")
                 (pretty-display (format "(define encoded-start-code (send printer encode start-code))"))
                 )
	   (pretty-display (format "(send search ~a encoded-code ~a \"~a-~a\" ~a ~a ~a #:assume ~a #:input-file ~a #:start-prog ~a)" 
                                   "superoptimize"
				   (send machine output-constraint-string "machine" live-out)
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
		      (and (file-exists? name)
			   (create-stat-from-file name printer)))))
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
	 [(equal? search-type `hybrid) 1]))
      (define cores-stoch (- cores cores-solver))

      ;; (define types 
      ;; 	(for/vector ([id cores]) 
      ;; 		    (if (or (equal? search-type `stoch) (equal? search-type `hybrid))
      ;; 			(cons #t mode) (cons #f mode))))
      ;; (when (equal? search-type `hybrid)
      ;; 	    (vector-set! types (sub1 cores) (cons `solver `hybrid)))
      ;; STEP 1: create file & run
      (define processes-stoch
        (for/list ([id cores-stoch])
                  (create-file id #t mode)
                  (run-file id)))

      (define processes-solver
	(for/list ([id cores-solver])
		  (create-file (+ cores-stoch id) #f (if (equal? search-type `hybrid) `partial mode))
		  (run-file (+ cores-stoch id))))

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
      (if output-code
	  (let ([decompressed-code (send compress decompress-reg-space output-code map-back)])
	    (send printer print-syntax decompressed-code)
	    decompressed-code)
	  code-org)
      )

    (define (optimize-filter code-org live-out  dir cores time-limit size 
                             assume extra-info input-file start-prog)
      (define-values (pass start stop extra-live-out) (send compress select-code code-org))
      (pretty-display `(select-code ,pass ,start ,stop ,extra-live-out))
      (if pass
          (let ([middle-output
                 (optimize-inner 
                  pass
                  (send compress combine-live-out live-out extra-live-out)
                   dir cores time-limit size extra-info input-file start-prog)])
            (send compress combine-code code-org middle-output start stop))
          code-org)
      )

    ;; Inputs
    ;; input: can be in 3 different types.
    ;;   1) parsed AST, 2) .s file, 3) LLVM IR file
    ;; live-out: live-out info in custom format--- for vpe, a list of live registers
    ;;   synthesize: #t = synthesize mode, #f = optimize mode
    (define (optimize code-org live-out 
                      #:assume [assume #f]
                      #:extra-info [extra-info #f]
                      #:need-filter [need-filter #f]
                      #:dir [dir "output"] 
                      #:cores [cores 12]
                      #:time-limit [time-limit 3600]
                      #:size [size #f]
                      #:input-file [input-file #f]
                      #:start-prog [start-prog #f])
      (pretty-display `(optimize))

      (if (> (vector-length code-org) 0)
          (if need-filter
              (optimize-filter code-org live-out  dir cores time-limit size assume extra-info input-file start-prog)
              (optimize-inner code-org live-out  dir cores time-limit size assume extra-info input-file start-prog))
          code-org))

    ))
