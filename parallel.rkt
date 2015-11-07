#lang racket

(require "path.rkt" "stat.rkt")

(provide parallel%)

(define (get-free-mem)
  (string->number
   (list-ref
    (string-split
     (with-output-to-string (thunk (system "free | head -2 | tail -1"))))
    3)))


(define parallel%
  (class object%
    (super-new)
    (init-field isa parser machine printer compress validator search-type mode
                [window #f])
    ;; search = `solver, `stoch, `hybrid
    ;; mode = `linear, `binary, `syn, `opt
    (public optimize)

    (define (required-module x) (format "~a/~a-~a.rkt" isa isa x))
    (define (get-class-name x) (format "~a-~a%" isa x))
    
    ;; Optimize code
    (define (optimize-inner code-org live-out-org live-in-org rootdir cores time-limit prog-size 
                            assume extra-info input-file start-prog)
      (pretty-display (format "SEACH TYPE: ~a size=~a" search-type prog-size))
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
      (set! machine-info (send validator proper-machine-config code machine-info extra-info))
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
        
        (define (create-file id search-type mode)
          (define (req file)
            (format "(file \"~a/~a\")" srcpath (required-module file)))
          (define required-files
            (string-join
             (map req '(parser machine printer
                               stochastic symbolic forwardbackward))))
          (with-output-to-file 
              #:exists 'truncate (format "~a-~a.rkt" path id)
              (thunk
               (pretty-display (format "#lang racket"))
               (pretty-display (format "(require ~a)" required-files))
               (pretty-display (format "(define machine (new ~a [config ~a]))"
                                       (get-class-name "machine")
                                       (send machine set-config-string machine-info)))
               (pretty-display (format "(define printer (new ~a [machine machine]))" (get-class-name "printer")))
               (pretty-display (format "(define parser (new ~a))" (get-class-name "parser")))


               (cond
                [(equal? search-type `stoch)
                 (pretty-display 
                  (format "(define search (new ~a [machine machine] [printer printer] [parser parser] [syn-mode ~a]))" 
                          (get-class-name "stochastic") 
                          (equal? mode `syn)))]
                [(equal? search-type `solver)
                 (pretty-display 
                  (format "(define search (new ~a [machine machine] [printer printer] [parser parser] [syn-mode `~a]))" 
                          (get-class-name "symbolic") 
                          mode))]
                [(equal? search-type `enum)
                 (pretty-display 
                  (format "(define search (new ~a [machine machine] [printer printer] [parser parser] [syn-mode `~a]))" 
                          (get-class-name "forwardbackward") 
                          mode))])

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
                        path id time-limit prog-size extra-info
                        (send machine output-assume-string "machine" assume)
                        (if input-file (string-append "\"" input-file "\"") #f)
                        (if start-prog "encoded-start-code" #f)
                        ))
               
               ;;(pretty-display "(dump-memory-stats)"
               )))
        
        (define (run-file id)
          (define out-port 
	    (open-output-file (format "~a-~a.log" path id) #:exists 'truncate))
          (define-values (sp o i e) 
            (subprocess out-port #f out-port (find-executable-path "racket") (format "~a-~a.rkt" path id)))
          sp)

        (define (kill-all)
          (for ([sp (append processes-stoch processes-solver processes-enum)]
		[id cores])
               (when (equal? (subprocess-status sp) 'running)
                     (subprocess-kill sp #f))))
	
        (define t (current-seconds))
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
          (pretty-display (format "current-time:\t~a" (- (current-seconds) t)))
          (when cost
                (pretty-display "=============== SUMMARY ===============")
                (pretty-display (format "cost:\t~a" cost))
                (pretty-display (format "len:\t~a" len))
                (pretty-display (format "time:\t~a" time))
                (pretty-display id)))

        
        (define cores-stoch
          (cond
           [(equal? search-type `stoch) cores]
           [(equal? search-type `hybrid) 3] ;;(floor (* (/ 2 6) cores))]
           [else 0]))
        (define cores-enum
          (cond
           [(equal? search-type `enum) cores]
           [(equal? search-type `hybrid) (floor (* (/ 3 6) cores))]
           [else 0]
           ))
        (define cores-solver 
          (cond
           [(equal? search-type `solver) cores]
           [(equal? search-type `hybrid) (- cores cores-stoch cores-enum)]
           [else 0]
           ))

        (pretty-display `(stoch-enum-sym ,cores-stoch ,cores-enum ,cores-solver))

        ;; STEP 1: create file & run
        (define-syntax-rule (create-and-run id mode search-type)
          (begin
            (create-file id search-type mode)
            (run-file id)
            ))

        (define processes-stoch
          (if (equal? search-type `hybrid)
              ;; (let ([n 3])
              ;;   (append (for/list ([id n]) 
              ;;                     (create-and-run id `opt `stoch))
              ;;           (for/list ([id (- cores-stoch n)]) 
              ;;                     (create-and-run (+ n id) `syn `stoch))))
              (for/list ([id cores-stoch]) (create-and-run id `opt `stoch))
              (for/list ([id cores-stoch]) (create-and-run id mode `stoch))))

        (define processes-solver
          (cond
           [(or (equal? search-type `hybrid)
                (and (equal? search-type `solver) (equal? mode `partial)))
            (define n1 (if (equal? search-type `solver) 1 0))
            (define n2 (floor (* (/ 8 16) cores-solver)))
            (define n3 0);(floor (* (/ 4 16) cores-solver)))
            (define n4 (if (< (+ n1 n2 n3) cores-solver) (ceiling (* (/ 2 16) cores-solver)) 0))
            (define n5 (if (< (+ n1 n2 n3 n4) cores-solver) (ceiling (* (/ 1 16) cores-solver)) 0))
            (set! n3 (- cores-solver n1 n2 n3 n4 n5))
            (pretty-display `(sym ,n1 ,n2 ,n3 ,n4 ,n5))
            (append (for/list ([i n1]) (create-and-run (+ cores-stoch i) `linear `solver))
                    (for/list ([i n2]) (create-and-run (+ cores-stoch n1 i) `partial1 `solver))
                    (for/list ([i n3]) (create-and-run (+ cores-stoch n1 n2 i) `partial2 `solver))
                    (for/list ([i n4]) (create-and-run (+ cores-stoch n1 n2 n3 i) `partial3 `solver))
                    (for/list ([i n5]) (create-and-run (+ cores-stoch n1 n2 n3 n4 i) `partial4 `solver))
                    )
            ]

           [else
            (for/list ([id cores-solver]) (create-and-run id mode `solver))]))

        (define processes-enum
          (cond
           [(or (equal? search-type `hybrid)
                (and (equal? search-type `enum) (equal? mode `partial)))
            (define n1 1)
            (define n2 (floor (* (/ 8 16) cores-enum)))
            (define n3 0) ;;(floor (* (/ 4 16) cores-enum)))
            (define n4 (if (< (+ n1 n2 n3) cores-enum) (ceiling (* (/ 2 16) cores-enum)) 0))
            (define n5 (if (< (+ n1 n2 n3 n4) cores-enum) (ceiling (* (/ 1 16) cores-enum)) 0))
            (set! n3 (- cores-enum n1 n2 n3 n4 n5))
            (pretty-display `(enum ,n1 ,n2 ,n3 ,n4 ,n5))
            (append (for/list ([i n1]) (create-and-run (+ cores-stoch cores-solver i) `linear `enum))
                    (for/list ([i n2]) (create-and-run (+ cores-stoch cores-solver n1 i) `partial1 `enum))
                    (for/list ([i n3]) (create-and-run (+ cores-stoch cores-solver n1 n2 i) `partial2 `enum))
                    (for/list ([i n4]) (create-and-run (+ cores-stoch cores-solver n1 n2 n3 i) `partial3 `enum))
                    (for/list ([i n5]) (create-and-run (+ cores-stoch cores-solver n1 n2 n3 n4 i) `partial4 `enum))
                    )
            ]
           
           [else
            (for/list ([id cores-enum]) (create-and-run id mode `enum))]))
        
        (define (result)
	  (define limit (if (string? time-limit) 
			    (string->number time-limit) 
			    time-limit))
          (define (update-stats)
            (sleep 10)
            (when (and (< (- (current-seconds) t) limit) (> (get-free-mem) 500000))
                  (for ([id (length processes-stoch)]
                        [sp processes-stoch])
                       (unless (equal? (subprocess-status sp) 'running)
                               (pretty-display (format "driver-~a is dead." id))))
                  (for ([id (length processes-solver)]
                        [sp processes-solver])
                       (unless (equal? (subprocess-status sp) 'running)
                               (pretty-display (format "driver-~a is dead." (+ cores-stoch id)))))
                  (for ([id (length processes-enum)]
                        [sp processes-enum])
                       (unless (equal? (subprocess-status sp) 'running)
                               (pretty-display (format "driver-~a is dead." (+ cores-stoch cores-solver id)))))
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
	(if (file-exists? (format "~a/best.s" dir))
	    (send parser ast-from-file (format "~a/best.s" dir))
	    (vector-copy code from to)))

      (define code-len (vector-length code))
      (define window-size (if window window (send machine window-size)))
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
            (pretty-display `(mid-positions ,mid-positions))
            (define small-size window-size);(floor (* (/ 6 10) window-size)))
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
            (pretty-display `(mid-positions ,mid-positions))

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

      (system "pkill -u mangpo java")
      (system "pkill -u mangpo z3")
      (let ([decompressed-code (send compress decompress-reg-space output-code map-back)])
        (send printer print-syntax decompressed-code)
        decompressed-code)
      )

    (define (optimize code-org live-out live-in
                      #:assume [assume #f]
                      #:extra-info [extra-info #f]
                      #:dir [dir "output"] 
                      #:cores [cores 8]
                      #:time-limit [time-limit 3600]
                      #:size [size #f]
                      #:input-file [input-file #f]
                      #:start-prog [start-prog #f])

      (if (> (vector-length code-org) 0)
          (optimize-inner code-org live-out live-in dir cores time-limit size assume extra-info input-file start-prog)
          code-org))

    ))
