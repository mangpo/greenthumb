#lang racket

(require "printer.rkt")
(provide stat% print-stat-all create-stat-from-file get-best-info)

(define-syntax-rule (ratio x y) (~r (exact->inexact (/ x y)) #:precision 6))

(define stat%
  (class object%

    (init-field printer 
                [best-correct-program #f] 
                [best-correct-cost #f]
                [best-correct-len #f]
                [stat-mutations (get-field report-mutations printer)]
                [n (vector-length stat-mutations)]
                [start-time (current-seconds)]
                [time #f]
                [propose-stat (make-vector n 0)]
                [accept-stat (make-vector n 0)]
                [best-correct-time 0]
                [best-program #f]
                [best-cost (arithmetic-shift 1 32)]
                [current-mutate #f]
                [iter-count 0]
                [misalign-count 0]
                [validate-count 0]
                [correct-count 0]
                [accept-count 0]
                [accept-higher-count 0]
                [name #f]
                [simulate-time 0]
                [check-time 0]
                [validate-time 0]
                [mutate-time 0]
		[dir #f]
                )
           
    (super-new)

    (define/public (set-name my-name)
      (set! name my-name)
      (when name
	    (define tokens (string-split my-name "/"))
	    (set! dir (string-join (take tokens (sub1 (length tokens))) "/"))))

    (define/public (inc-iter current-cost)
      ;; (with-output-to-file #:exists 'append (format "~a.csv" name)
      ;;   (thunk
      ;;    (pretty-display (format "~a,~a" iter-count current-cost))))
      (set! iter-count (add1 iter-count))
      (and (= (modulo iter-count 30000) 0) ;;30000
	   (let-values ([(cost len time id) (get-best-info dir)])
             (print-stat-to-file)
             (and cost len))))

    (define/public (inc-propose x)
      (define index (vector-member x stat-mutations))
      (vector-set! propose-stat index 
                   (add1 (vector-ref propose-stat index)))
      (set! current-mutate index))

    (define/public (inc-accept)
      (set! accept-count (add1 accept-count))
      (vector-set! accept-stat current-mutate 
                   (add1 (vector-ref accept-stat current-mutate))))

    (define/public (inc-accept-higher)
      (set! accept-higher-count (add1 accept-higher-count)))

    (define/public (inc-correct)
      (set! correct-count (add1 correct-count)))

    (define/public (inc-validate)
      (set! validate-count (add1 validate-count)))
    
    (define/public (inc-misalign)
      (set! misalign-count (add1 misalign-count)))

    (define/public (get-best-info-stat)
      (get-best-info dir))

    (define/public (update-best program cost)
      (set! best-program program)
      (set! best-cost cost)

      (with-output-to-file #:exists 'truncate (format "~a.cand" name)
        (thunk
         ;; (pretty-display (format "best-correct-cost: ~a" best-correct-cost))
         ;; (pretty-display (format "best-correct-time: ~a" best-correct-time))
         (send printer print-syntax (send printer decode program))))
      )
    
    (define/public (update-best-correct-program program)
      (set! best-correct-program program))


    (define/public (update-best-correct program cost)
      (set! best-correct-program program)
      (set! best-correct-cost cost)
      (set! best-correct-len (vector-length program))
      (set! best-correct-time (- (current-seconds) start-time))
      (pretty-display `(time ,best-correct-time))

      (when dir
	    (define info-file (format "~a/best.info" dir))
	    (define-values (cost-r len-r time-r id-r) (get-best-info dir))
            
	    (when (or (not cost-r) (< cost cost-r))
		(pretty-display "FOUND!!!")
		(with-output-to-file #:exists 'append (format "~a/summary" dir)
		  (thunk
		   (pretty-display 
		    (format
		     "~a,~a,~a,~a" 
		     best-correct-cost best-correct-len best-correct-time name))))
		(with-output-to-file #:exists 'truncate (format "~a/best.s" dir)
		  (thunk
		   (send printer print-syntax (send printer decode best-correct-program))))

		(with-output-to-file #:exists 'truncate info-file
		  (thunk
		   (pretty-display best-correct-cost)
		   (pretty-display best-correct-len)
		   (pretty-display best-correct-time)
		   (pretty-display name)
		   ))
		(with-output-to-file #:exists 'truncate (format "~a/best-~a.s" dir best-correct-cost)
		  (thunk
		   (send printer print-syntax (send printer decode best-correct-program))))
                )
	    (with-output-to-file #:exists 'truncate (format "~a.best" name)
              (thunk
	       (send printer print-syntax (send printer decode best-correct-program))))
	    )
      )

    (define/public (simulate x) (set! simulate-time (+ simulate-time x)))
    (define/public (check x)    (set! check-time (+ check-time x)))
    (define/public (validate x) (set! validate-time (+ validate-time x)))
    (define/public (mutate x)   (set! mutate-time (+ mutate-time x)))
    
    (define/public (print-stat-to-file)
      (set! time (- (current-seconds) start-time))
      (with-output-to-file #:exists 'truncate (format "~a.stat" name)
        (thunk (print-stat)))
      ;;(print-stat)
      )

    (define/public (print-stat)
      (pretty-display "---------------------------------------------------------")
      ;(pretty-display (format "memory-use:\t~a" (exact->inexact (/ (current-memory-use) 1000))))
      (pretty-display (format "elapsed-time:\t~a" time))
      (pretty-display (format "mutate-time:\t~a" mutate-time))
      (pretty-display (format "simulate-time:\t~a" simulate-time))
      (pretty-display (format "check-time:\t~a" check-time))
      (pretty-display (format "validate-time:\t~a" validate-time))
      (newline)
      (pretty-display (format "validate-count:\t~a" validate-count))
      (pretty-display (format "correct-count:\t~a" correct-count))
      ;(pretty-display (format "misalign-count:\t~a" misalign-count))
      (newline)
      (pretty-display (format "iterations:\t~a" iter-count))
      (when (> time 0)
            (pretty-display (format "iterations/s:\t~a" (exact->inexact (/ iter-count time)))))
      (pretty-display (format "best-cost:\t~a" best-cost))
      (pretty-display (format "best-correct-cost:\t~a" best-correct-cost))
      (pretty-display (format "best-correct-time:\t~a" best-correct-time))
      (newline)
      (define proposed (foldl + 0 (vector->list propose-stat)))
      (define accepted (foldl + 0 (vector->list accept-stat)))

      (pretty-display (format "Mutate\tProposed\t\tAccepted\t\tAccepted/Proposed"))
      (for ([i n])
           (pretty-display (format "~a\t~a\t~a\t~a" 
                                   (vector-ref stat-mutations i)
                                   (exact->inexact (/ (vector-ref propose-stat i) proposed))
                                   (exact->inexact (/ (vector-ref accept-stat i) proposed))
                                   (if (> (vector-ref propose-stat i) 0)
                                       (exact->inexact (/ (vector-ref accept-stat i) (vector-ref propose-stat i)))
                                       0))))
      (newline)
      ;; (pretty-display (format "acceptance-rate:\t~a" 
      ;;                         (exact->inexact (/ accepted proposed))))
      (pretty-display (format "accept-count:\t~a" accept-count))
      (pretty-display (format "accept-higher-count:\t~a" accept-higher-count))
      ;; (pretty-display (format "accept-higher-percent:\t~a" 
      ;;                         (exact->inexact (/ accept-higher-count accept-count))))
      
    )
    ))

;; (define (get-output-id stat-list)

;;   (define best-correct-cost (arithmetic-shift 1 32))
;;   (define best-correct-time (arithmetic-shift 1 32))
;;   (define best-correct-id #f)

;;   (for ([stat stat-list]
;;         [id (length stat-list)])
;;        (when stat
;; 	     (let ([correct-cost (get-field best-correct-cost stat)]
;; 		   [correct-time (get-field best-correct-time stat)])
;; 	       (when (< correct-cost best-correct-cost)
;; 		     (set! best-correct-cost correct-cost)
;; 		     (set! best-correct-time correct-time)
;; 		     (set! best-correct-id id)
;; 		     )
;; 	       (when (and (= correct-cost best-correct-cost)
;; 			  (< correct-time best-correct-time))
;; 		     (set! best-correct-time correct-time)
;; 		     (set! best-correct-id id)
;; 		     ))))

;;   (pretty-display (format "best-correct-cost:\t~a" best-correct-cost))
;;   (pretty-display (format "best-correct-time:\t~a" best-correct-time))
;;   best-correct-id)
  

(define (print-stat-all stat-list printer)
  (define stat-mutations (get-field report-mutations printer))
  (define n (vector-length stat-mutations))
  (define-syntax-rule (reduce+ field)
    (foldl + 0 (map (lambda (x) (get-field field x)) stat-list)))

  ;(pretty-display "time")
  (define time (reduce+ time))
  (define mutate-time (reduce+ mutate-time))
  (define simulate-time (reduce+ simulate-time))
  (define check-time (reduce+ check-time))
  (define validate-time (reduce+ validate-time))
  ;(pretty-display "iter-count")
  (define iter-count (reduce+ iter-count))
  ;(pretty-display "validate-count")
  (define validate-count (reduce+ validate-count))
  ;(pretty-display "correct-count")
  (define correct-count (reduce+ correct-count))
  ;(pretty-display "misalign-count")
  ;(define misalign-count (reduce+ misalign-count))
  ;(pretty-display "accept-higher-count")

  (define accept-higher-count (reduce+ accept-higher-count))
  (define accept-count (reduce+ accept-count))

  (define all 0)
  ;(pretty-display "propose-stat")
  (define propose-stat
    (for/vector ([i n])
      (foldl + 0 (map (lambda (x) (vector-ref (get-field propose-stat x) i))
                      stat-list))))
  ;(pretty-display "accept-stat")
  (define accept-stat
    (for/vector ([i n])
      (foldl + 0 (map (lambda (x) (vector-ref (get-field accept-stat x) i)) 
                      stat-list))))

  (define best-correct-cost (arithmetic-shift 1 32))
  (define best-correct-time (arithmetic-shift 1 32))
  (define best-cost (arithmetic-shift 1 32))

  (for ([stat stat-list]
        [id (length stat-list)])
       (let ([correct-cost (get-field best-correct-cost stat)]
             [correct-time (get-field best-correct-time stat)]
             [cost (get-field best-cost stat)])
         (when (< correct-cost best-correct-cost)
               (set! best-correct-cost correct-cost)
               (set! best-correct-time correct-time)
               )
         (when (and (= correct-cost best-correct-cost)
                    (< correct-time best-correct-time))
               (set! best-correct-time correct-time)
               )
         (when (< cost best-cost)
               (set! best-cost cost))))


  (define stat (new stat%
                    [printer printer]
                    [time (quotient time (length stat-list))]
                    [mutate-time (ratio (/ mutate-time 1000) time)]
                    [simulate-time (ratio (/ simulate-time 1000) time)]
                    [check-time (ratio (/ check-time 1000) time)]
                    [validate-time (ratio (/ validate-time 1000) time)]
                    [iter-count iter-count]
                    [validate-count (ratio validate-count iter-count)]
                    [correct-count (ratio correct-count iter-count)]
                    [accept-count (ratio accept-count iter-count)]
                    [accept-higher-count (ratio accept-higher-count iter-count)]
                    ;[misalign-count (ratio misalign-count iter-count)]
                    [propose-stat propose-stat]
                    [accept-stat accept-stat]
                    [best-correct-program #f]
                    [best-correct-time best-correct-time]
                    [best-correct-cost best-correct-cost]
                    [best-cost best-cost]))
  (send stat print-stat))

(define (create-stat-from-file file printer)
  (define in-port (open-input-file file))
  
  (define stat-mutations (get-field report-mutations printer))
  (define n (vector-length stat-mutations))

  (define id #f)
  (define time #f)
  (define mutate-time #f)
  (define simulate-time #f)
  (define check-time #f)
  (define validate-time #f)

  (define iter-count #f)
  (define validate-count #f)
  (define correct-count #f)
  ;(define misalign-count #f)
  (define accept-count #f)
  (define accept-higher-count #f)
  (define propose-stat (make-vector n))
  (define accept-stat (make-vector n))

  (define best-correct-cost #f)
  (define best-correct-time #f)
  (define best-correct-id #f)
  (define best-cost #f)
  
  (define (parse)
    (define line (read-line in-port))
    (unless (equal? eof line)
      (define tokens (string-split line))
      ;(pretty-display `(tokens ,tokens))

      (define-syntax-rule (pattern (a b) ...)
        (cond
         [(< (length tokens) 2) #f]
         
         [(regexp-match a (car tokens)) (set! b (string->number (second tokens)))]
         ...
         
         [(vector-member (string->symbol (car tokens)) stat-mutations)
          (let ([index (vector-member (string->symbol (car tokens)) stat-mutations)])
            (vector-set! propose-stat index (string->number (list-ref tokens 1)))
            (vector-set! accept-stat index (string->number (list-ref tokens 2))))]
         ))
  
      (pattern (#rx"elapsed-time" time)
               (#rx"mutate-time" mutate-time)
               (#rx"simulate-time" simulate-time)
               (#rx"check-time" check-time)
               (#rx"validate-time" validate-time)
               (#rx"iterations:" iter-count)
               (#rx"validate-count:" validate-count)
               (#rx"correct-count:" correct-count)
               ;(#rx"misalign-count:" misalign-count)
               (#rx"accept-count:" accept-count)
               (#rx"accept-higher-count:" accept-higher-count)
               (#rx"best-cost" best-cost)
               (#rx"best-correct-cost" best-correct-cost)
               (#rx"best-correct-time" best-correct-time))

      (parse)))
 
  (parse)
  (close-input-port in-port)

  ;; (pretty-display `(validate-count ,validate-count))
  ;; (pretty-display `(correct-count ,correct-count))
  ;; (pretty-display `(misalign-count ,misalign-count))

  (new stat% 
       [printer printer]
       [time time]
       [mutate-time mutate-time]
       [simulate-time simulate-time]
       [check-time check-time]
       [validate-time validate-time]
       [iter-count iter-count]
       [validate-count validate-count]
       [correct-count correct-count]
       ;[misalign-count misalign-count]
       [accept-count accept-count]
       [accept-higher-count accept-higher-count]
       [propose-stat propose-stat]
       [accept-stat accept-stat]
       [best-correct-program #f]
       [best-correct-time best-correct-time]
       [best-correct-cost best-correct-cost]
       [best-cost best-cost]))


(define (get-best-info dir)
  (define info-file (format "~a/best.info" dir))
  ;; (pretty-display `(get-best-info ,info-file ,(file-exists? info-file)))
  (if (file-exists? info-file)
      (with-handlers* 
       ([exn?
         (lambda (e) (values #f #f #f #f))])
       (let* ([in-port (open-input-file info-file)]
              [cost (read-line in-port)]
              [len (read-line in-port)]
              [time (read-line in-port)]
              [id (read-line in-port)])
         (close-input-port in-port)
         (values (string->number cost) (string->number len) time id)))
      (values #f #f #f #f)))
