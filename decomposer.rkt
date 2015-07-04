#lang racket

(require  "ast.rkt" "machine.rkt" "printer.rkt" "stat.rkt")

(provide decomposer%)
(struct exn:restart exn (program))

(define decomposer%
  (class object%
    (super-new)
    (init-field machine printer parser [simulator #f] [validator #f] [syn-mode `linear]
		[stat (new stat% [printer printer])])
    (abstract len-limit window-size synthesize-window)
    (public superoptimize
            superoptimize-linear superoptimize-binary)

    (define (superoptimize spec constraint live-in name time-limit size [extra #f]
                           #:prefix [prefix (vector)] #:postfix [postfix (vector)]
			   #:assume [assumption (send machine no-assumption)]
			   #:input-file [input-file #f]
			   #:start-prog [start #f])
      (send stat set-name name)
      (set-field! best-correct-cost stat (send simulator performance-cost spec))
      (timeout
       time-limit
       (cond
	[(equal? syn-mode `binary) 
	 (superoptimize-binary spec constraint time-limit size extra
                               #:hard-prefix prefix #:hard-postfix postfix
			       #:assume assumption)]

	[(equal? syn-mode `linear) 
	 (superoptimize-linear spec constraint time-limit size extra
                               #:hard-prefix prefix #:hard-postfix postfix
			       #:assume assumption)]

	;; [(equal? syn-mode `partial1)
	;;  (superoptimize-partial-pattern spec constraint 90 size extra ;; no div 60
        ;;                                 #:hard-prefix prefix #:hard-postfix postfix
        ;;                                 #:assume assumption)]

	;; [(equal? syn-mode `partial2)
	;;  (superoptimize-partial-pattern-slow spec constraint 800 size extra
        ;;                                      #:hard-prefix prefix #:hard-postfix postfix
        ;;                                      #:assume assumption)]

	;; [(equal? syn-mode `partial3)
	;;  (superoptimize-partial-random spec constraint 90 1 size extra
        ;;                                 #:hard-prefix prefix #:hard-postfix postfix
        ;;                                 #:assume assumption)]

	;; [(equal? syn-mode `partial4)
	;;  (superoptimize-partial-random spec constraint 240 1 size extra ;; no div 100
        ;;                                 #:hard-prefix prefix #:hard-postfix postfix
        ;;                                 #:assume assumption)]

	[(equal? syn-mode `partial1)
	 (superoptimize-partial-random spec constraint 60 (/ 1 2) size extra
                                       #:hard-prefix prefix #:hard-postfix postfix
                                       #:assume assumption)]

	[(equal? syn-mode `partial2)
	 (superoptimize-partial-random spec constraint 60 1 size extra
                                       #:hard-prefix prefix #:hard-postfix postfix
                                       #:assume assumption)]

	[(equal? syn-mode `partial3)
	 (superoptimize-partial-random spec constraint 90 (/ 3 2) size extra
                                       #:hard-prefix prefix #:hard-postfix postfix
                                       #:assume assumption)]

	[(equal? syn-mode `partial4)
	 (superoptimize-partial-random spec constraint 90 2 size extra
                                       #:hard-prefix prefix #:hard-postfix postfix
                                       #:assume assumption)]
        )
       )
      )


    ;; Optimize the cost using binary search on the number of holes.
    ;; spec: non-encoded block
    (define (superoptimize-binary spec constraint time-limit size [extra #f]
				  #:lower-bound [lower-bound 0]
                                  #:assume [assumption (send machine no-assumption)]
                                  #:prefix [prefix (vector)] #:postfix [postfix (vector)]
                                  #:hard-prefix [hard-prefix (vector)] 
                                  #:hard-postfix [hard-postfix (vector)]
                                  )
      (pretty-display (format ">> superoptimize-binary"))
      (when (> (vector-length prefix) 0)
            (display "[")
            (send printer print-syntax (send printer decode prefix))
            (display "] "))
      (send printer print-syntax (send printer decode spec))
      (when (> (vector-length postfix) 0)
            (display " [")
            (send printer print-syntax (send printer decode postfix))
            (display "]"))
      (newline)
      (define prefix-len (vector-length prefix))
      (define postfix-len (vector-length postfix))

      (define final-program #f)
      (define final-len (if size size (vector-length spec)))
      (define final-cost #f)
      (define (inner begin end cost [middle (quotient (+ begin end) 2)])
	(newline)
        (pretty-display `(binary-search ,begin ,end ,middle ,cost))
        (define sketch (send validator sym-insts middle))
        
        (define-values (out-program out-cost)
          (with-handlers* 
           ([exn:fail? 
             (lambda (e) 
               (pretty-display "catch error")
               ;; (if (or (regexp-match #rx"synthesize: synthesis failed" (exn-message e))
               ;;         (regexp-match #rx"assert: cost" (exn-message e)))
               ;;     (values #f cost)
               ;;     (begin
               ;;      (pretty-display (exn-message e))
               ;;      (raise e)))
               (when (and (not (regexp-match #rx"synthesize: synthesis failed" (exn-message e)))
                          (not (regexp-match #rx"assert: cost" (exn-message e))))
                     (pretty-display (exn-message e)))
               (values #f cost)
               )])
	   ;; TODO: args: spec sketch prefix postfix
           (synthesize-window spec sketch prefix postfix 
			      constraint extra cost time-limit
			      #:hard-prefix hard-prefix #:hard-postfix hard-postfix
			      #:assume assumption)))

        (when out-program 
              (set! final-program 
                    (vector-copy out-program 
                                 prefix-len 
                                 (- (vector-length out-program) postfix-len)))
              (set! final-len middle)
              (set! final-cost out-cost))

        (if out-program
            (inner begin middle out-cost)
            (when (< middle end) (inner (add1 middle) end cost))))
      
      (with-handlers 
       ([exn:break? (lambda (e) (unless final-program (set! final-program "timeout")))])
       (inner (max 1 lower-bound) final-len 
              (send simulator performance-cost (vector-append prefix spec postfix))
              (max lower-bound (quotient (+ 1 final-len) 2))))

      ;; Try len + 2
      ;; (unless (equal? final-program "timeout")
      ;;         (with-handlers 
      ;;          ([exn:break? (lambda (e) (void))])
      ;;          (inner (+ final-len 2) (+ final-len 2) final-cost)))
      

      (pretty-display "after inner")
      final-program)

    (define (superoptimize-linear spec constraint time-limit size [extra #f]
			   #:assume [assumption (send machine no-assumption)]
                           #:prefix [prefix (vector)] #:postfix [postfix (vector)]
                           #:hard-prefix [hard-prefix (vector)] #:hard-postfix [hard-postfix (vector)]
                           )
      (newline)
      (pretty-display (format ">> superoptimize-linear size = ~a" size))
      (when (> (vector-length prefix) 0)
            (display "[")
            (send printer print-syntax (send printer decode prefix))
            (display "] "))
      (send printer print-syntax (send printer decode spec))
      (when (> (vector-length postfix) 0)
            (display " [")
            (send printer print-syntax (send printer decode postfix))
            (display "]"))
      (newline)
      (define prefix-len (vector-length prefix))
      (define postfix-len (vector-length postfix))
      (define sketch (send validator sym-insts (if size (min size (vector-length spec)) 
				    (vector-length spec))))
      (define final-program #f) ;; not including prefix & poster
      (define t #f)
      (define (inner cost)
	(newline)
        (pretty-display `(linear-inner ,(vector-length sketch) ,cost))
        (set! t (current-seconds))
	(define-values (out-program out-cost) 
	  (synthesize-window spec sketch prefix postfix
			     constraint extra cost time-limit
			     #:hard-prefix hard-prefix #:hard-postfix hard-postfix
			     #:assume assumption))
        (pretty-display `(time ,(- (current-seconds) t)))

	(set! final-program (vector-copy out-program 
                                         prefix-len 
                                         (- (vector-length out-program) postfix-len)))
	(set! sketch (vector-take sketch (vector-length final-program)))
	(inner out-cost))
      
      (with-handlers* 
       ([exn:fail? 
	 (lambda (e) 
	   (clean-up)
           (pretty-display "FAIL!")
           (pretty-display `(time ,(- (current-seconds) t)))
           (pretty-display (exn-message e))
	   (if (or (regexp-match #rx"synthesize: synthesis failed" (exn-message e))
                   (regexp-match #rx"assert: cost" (exn-message e))
		   (regexp-match #rx"assert: progstate-cost" (exn-message e)))
	       final-program
	       (raise e)))]
	[exn:break? (lambda (e) 
		      (clean-up)
                      (pretty-display "TIMEOUT!")
		      (if final-program
			   final-program
			  "timeout"))])
       (inner (send simulator performance-cost (vector-append prefix spec postfix)))))

    ;; TODO: timeout = 60 => 150
    (define (superoptimize-partial-pattern 
             spec constraint time-limit size [extra #f]
             #:hard-prefix [hard-prefix (vector)]
             #:hard-postfix [hard-postfix (vector)]
             #:assume [assumption (send machine no-assumption)])

      (define (inner)
	(newline)
	(pretty-display "Phase 1: fixed window")
        (define program1
          (fixed-window hard-prefix hard-postfix spec constraint 60 extra assumption 
                        (window-size) (len-limit)))
        (check-global spec #f)
	;;(define program1 spec)

	(define (loop timeout w)
	  (newline)
	  (pretty-display (format "Phase 2: sliding window, timeout = ~a, window-size = ~a" 
				  timeout w))
	  (define program2
	    (sliding-window hard-prefix hard-postfix program1 
                            constraint timeout extra assumption w))
	  (check-global spec program2)
	  (loop (* 2 timeout) (floor (* (/ 5 4) w))))
	(loop time-limit (window-size))
        )
        
      (with-handlers*
       ([exn:restart?
         (lambda (e)
	   (superoptimize-partial-pattern 
            (exn:restart-program e)
            constraint time-limit size extra 
            #:hard-prefix hard-prefix #:hard-postfix hard-postfix
            #:assume assumption))])
       (inner))
      )

    (define (superoptimize-partial-pattern-slow
             spec constraint time-limit size [extra #f]
             #:hard-prefix [hard-prefix (vector)]
             #:hard-postfix [hard-postfix (vector)]
             #:assume [assumption (send machine no-assumption)])
      (pretty-display "superoptimize-partial-pattern-slow")
      (define (loop timeout w)
        (newline)
        (pretty-display (format "Phase: sliding window, timeout = ~a, window-size = ~a" 
                                timeout w))
        (define program
          (sliding-window hard-prefix hard-postfix spec
                          constraint timeout extra assumption w 
			  #:restart #t #:lower-bound (add1 (len-limit))))
        (check-global spec program)
        (loop (* 2 timeout) (max (add1 w) (floor (* (/ 5 4) w)))))
        
      (with-handlers*
       ([exn:restart?
         (lambda (e)
	   (superoptimize-partial-pattern-slow
            (exn:restart-program e)
            constraint time-limit size extra 
            #:hard-prefix hard-prefix #:hard-postfix hard-postfix
            #:assume assumption))])
       (loop time-limit (max (add1 (window-size)) (floor (* (/ 5 4) (window-size))))))
      )
    
    (define (superoptimize-partial-random 
             spec constraint time-limit scale size [extra #f]
             #:hard-prefix [hard-prefix (vector)]
             #:hard-postfix [hard-postfix (vector)]
             #:assume [assumption (send machine no-assumption)])
      (define (inner w timeout choices)
        (define from (list-ref choices (random (length choices))))
        (pretty-display (format ">> superoptimize-partial-random pos = ~a, timeout = ~a" from timeout))
        ;;(pretty-display `(choices ,choices))
        (define prefix (vector-copy spec 0 from))
        (define after-prefix (vector-copy spec from))
        (define-values (new-seq pos)
          (sliding-window-at hard-prefix hard-postfix 
                             prefix after-prefix
                             constraint timeout extra assumption w))
        (define output 
          (if new-seq
              (vector-append prefix new-seq (vector-copy after-prefix pos))
              spec))

        (check-global spec output)

        (define new-choices (remove from choices))
        (if (empty? new-choices)
            (inner (floor (* (/ 5 4) w)) 
		   (* 2 timeout) (range (max 1 (sub1 (vector-length spec)))))
            (inner w timeout new-choices))
        )

      (with-handlers*
       ([exn:restart?
         (lambda (e)
           (superoptimize-partial-random 
            (exn:restart-program e)
            constraint time-limit scale size extra 
            #:hard-prefix hard-prefix #:hard-postfix hard-postfix
            #:assume assumption))])
       (inner (floor (* scale (window-size))) time-limit
              (range (sub1 (vector-length spec))))))

    (define (check-global input-prog quick-restart)
      (define-values (cost len time id) (send stat get-best-info-stat))
      (pretty-display `(check-global ,cost ,len ,id))
      (define old-cost (send simulator performance-cost input-prog))
      (define best-cost (if cost cost (get-field best-correct-cost stat)))

      (when (< best-cost old-cost)
        (when (< best-cost (get-field best-correct-cost stat))
          (pretty-display "Steal program from other."))
        (when (or quick-restart (< best-cost (get-field best-correct-cost stat)))
          (define best-program 
            (if cost 
                (send printer encode
                      (send parser ast-from-file 
                            (format "~a/best.s" (get-field dir stat))))
                quick-restart))
          (pretty-display "restart!!!!!")
          (raise (exn:restart "restart" (current-continuation-marks) best-program)))))
    
    (define (fixed-window hard-prefix hard-postfix spec constraint time-limit extra assume
                          window size-limit)
      (define len (vector-length spec))
      (define output (vector))
      (define steps (quotient len window))
      (for ([i steps])
           (let* ([start (* i window)]
                  [end (* (add1 i) window)]
                  [seq (vector-copy spec start end)]
                  [new-seq
                   (superoptimize-linear 
                    seq constraint time-limit size-limit extra #:assume assume
                    #:hard-prefix hard-prefix #:hard-postfix hard-postfix
                    #:prefix output
                    #:postfix (vector-copy spec end len))])
             (if (or (equal? new-seq #f) (equal? new-seq "timeout"))
                 (set! output (vector-append output seq))
                 (set! output (vector-append output new-seq)))))
      (set! output (vector-append output (vector-copy spec (* steps window) len)))
      (when (> len (* steps window))
            (let* ([out-len (vector-length output)]
                   [seq (vector-copy output (max 0 (- out-len window)) out-len)]
                   [prefix (vector-copy output 0 (max 0 (- out-len window)))]
                   [new-seq
                    (superoptimize-linear 
                     seq constraint time-limit size-limit extra #:assume assume
                     #:hard-prefix hard-prefix #:hard-postfix hard-postfix
                     #:prefix prefix)])
             (if (or (equal? new-seq #f) (equal? new-seq "timeout"))
                 (set! output (vector-append prefix seq))
                 (set! output (vector-append prefix new-seq)))))
      ;; (print-syntax (decode output))
      output)

    (define (sliding-window-at hard-prefix hard-postfix prefix code 
                               constraint time-limit extra assume window
			       #:lower-bound [lower-bound 0]
                               #:restart [restart #f])
      (define spec (vector-append prefix code))
      (define len-code (vector-length code))
      (define (inner pos-to)
        (define out-program
          (superoptimize-binary 
           (vector-take code pos-to) constraint time-limit #f extra #:assume assume
           #:hard-prefix hard-prefix #:hard-postfix hard-postfix
           #:prefix prefix
           #:postfix (vector-drop code pos-to)
	   #:lower-bound lower-bound))
        (when restart (check-global spec #f))
        (cond
         [(equal? out-program "timeout")
          (if (> pos-to (max 2 lower-bound)) 
              (begin
                (pretty-display "timeout => shrink")
                (inner (sub1 pos-to)))
              (values #f #f))]
         [(equal? out-program #f)
          (values #f pos-to)]
         [else
          (values out-program pos-to)]))
                  
      (inner (min len-code window)))

    (define (sliding-window hard-prefix hard-postfix spec 
			    constraint time-limit extra assume window 
			    #:restart [restart #f]
			    #:lower-bound [lower-bound 0])
      (define output (vector))
      (define (loop code)
        (when (> (vector-length code) 0)
          (define-values 
            (out-program next-pos)
            (sliding-window-at hard-prefix hard-postfix output code 
			       constraint time-limit extra assume window
                               #:restart restart #:lower-bound lower-bound
                               ))
	  (cond
	   [out-program
	    (pretty-display "found => skip")
	    (set! output (vector-append output out-program))
	    (loop (vector-drop code next-pos))]
	   [(and next-pos (>= next-pos (vector-length code)))
	    (set! output (vector-append output code))]
	   [else
	    (set! output (vector-append output (vector (vector-ref code 0))))
	    (loop (vector-drop code 1))])))
	   
      (loop spec)
      output)
    
    (define (clean-up) (void))


    ))
