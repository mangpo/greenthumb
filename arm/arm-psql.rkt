#lang racket

(require db)
(require "arm-machine.rkt" "ast.rkt")

(provide arm-psql%)

(struct entry (progs states))

(define arm-psql%
  (class object%
    (super-new)
    (init-field machine printer)
    (public db-connect db-disconnect init create-table insert)

    (define pgc #f)
    (define nregs (send machine get-nregs))
    (define nmems (send machine get-nmems))
    (define state-cols #f)
    (define reg-range-db
      (cond
       [(= nregs 1) (list->vector (range -31 33))]
       [(= nregs 2) (list->vector (range -3 5))]
       [(= nregs 3) (list->vector (range -1 3))]
       [else (raise "arm-psql: does not support nregs > 3")]))
    (define z-range-db (vector -1 0 1 2 3))

    ;; TODO: include #live-in
    (define table-name (format "arm_r~a_m~a" nregs nmems))

    (define (db-connect)
      (set! pgc (postgresql-connect #:user "mangpo" #:database "mangpo" #:password "")))

    (define (db-disconnect)
      (disconnect pgc))

    (define (create-table size ntests)
      (define query (format "create table ~a_size~a (~a)" 
                            table-name size (get-header ntests)))
      (pretty-display (format "query: ~a" query))
      (query-exec pgc query))

    (define (get-header ntests)
      (define o (open-output-string))
      (for ([t ntests])
           ;; (for ([r nregs]) (display (format "i~a_r~a integer, " t r) o))
           ;; (for ([m nmems]) (display (format "i~a_m~a integer, " t m) o))
           ;; (display (format "i~a_z integer, " t) o)
           (for ([r nregs]) (display (format "o~a_r~a integer, " t r) o))
           (for ([m nmems]) (display (format "o~a_m~a integer, " t m) o))
           (display (format "o~a_z integer, " t) o)
           )
      (display "cost integer, program text" o)
      (get-output-string o))

    (define (init ntests)
      (define tmp (list))
      (for ([t ntests])
           (let ([tmp-inner (list)])
             (for ([r nregs]) (set! tmp-inner (cons (format "o~a_r~a" t r) tmp-inner)))
             (for ([m nmems]) (set! tmp-inner (cons (format "o~a_m~a" t m) tmp-inner)))
             (set! tmp-inner (cons (format "o~a_z" t) tmp-inner))
             (set! tmp (cons (string-join (reverse tmp-inner) ",") tmp)))
        )
      (set! state-cols (list->vector (reverse tmp))))

    ;; p is inst.
    (define (insert size cost states-in states-out p)
      
      (define keys (list))
      (define vals (list))
      (for ([col state-cols]
            [out states-out])
           (when out
                 (set! keys (cons col keys))
                 (set! vals (cons (progstate->string out) vals))))
      (unless 
       (empty? keys)
       (set! keys (reverse keys))
       (set! vals (reverse vals))
       
       (define o (open-output-string))
       (parameterize ([current-output-port o])
                     (send printer print-syntax (send printer decode p)))
       (pretty-display (get-output-string o))
       
       (define query
        (format "insert into ~a_size~a (~a,cost,program) values (~a,~a,'~a')" 
                table-name size
                (string-join keys ",") (string-join vals ",")
                cost (get-output-string o)))
       ;;(pretty-display (format "query: ~a" query))
       (query-exec pgc query)))

    
    (define (get-all-states)
      (define nmems (send machine get-nmems))
      (define fp (send machine get-fp))
      ;;(when (> nmems 0) (raise "get-all-states: does not support nmems > 0"))
      (define states (list))
      
      ;;(define count 0)
      (define (recurse-regs z n vals)
        (if (= n 0)
            (begin
              ;;(pretty-display `(debug ,count ,vals))
              ;;(set! count (add1 count))
              (set! states (cons (progstate (list->vector (reverse vals))
                                            (vector) z fp) states))
              )
            (for ([v reg-range-db])
                 (recurse-regs z (sub1 n) (cons v vals)))))

      (for ([z z-range-db])
           (recurse-regs z nregs (list)))

      (reverse states))

    (define (progstate->string state)
      (define regs (progstate-regs state))
      (define memory (progstate-memory state))
      (define regs-str (string-join (map number->string (vector->list regs)) ","))
      (define memory-str (string-join (map number->string (vector->list memory)) ","))
      (if (= (vector-length memory) 0)
          (format "~a,~a" regs-str (progstate-z state))
          (format "~a,~a,~a" regs-str memory-str (progstate-z state))))


    (define (power b p)
      (if (= p 0) 1 (* p (power b (sub1 p)))))

    ;; top - #t, bottom - #f
    (define (progstate->id state)
      (define z (progstate-z state))
      (define regs (progstate-regs state))
      (define id 0)
      (define len (reg-range-db))
      
      (for ([r regs]
            [i (reverse (range nregs))])
           (when (number? id)
                 (let ([index (vector-member r reg-range-db)])
                   (cond
                    [index (set! id (+ id (* index (power len i))))]
                    [(sql-null? r) (set! id #f)]
                    [else (set! id #t)]))))

      (when (number? id)
            (set! id (+ id (* (vector-member z z-range-db) (power len nregs)))))
      id)

    (define (find-program ins-id outs live)
      ;; TODO)

    (define (synthesize-window spec sketch prefix postfix constraint extra 
                               [cost #f] [time-limit 3600]
                               #:hard-prefix [hard-prefix (vector)] 
                               #:hard-postfix [hard-postfix (vector)]
                               #:assume-interpret [assume-interpret #t]
                               #:assume [assumption (send machine no-assumption)])
      (define spec-len (vector-length spec))
      (define ntests 16)
      (define inits
	(send validator generate-input-states ntests (vector-append prefix spec postfix)
              assumption extra #:rand-func (random-from-list reg-range-db)))
      
      (define live2 (send validator get-live-in postfix constraint extra))
      (define states1 
	(map (lambda (x) (send simulator interpret prefix x #:dep #f)) inits))
      (define states2-spec
	(map (lambda (x) (send simulator interpret spec x #:dep #f))) states1)

      (define queue (entry (list (vector)) states1))
      ;; TODO: visit cache
      
      (define (loop)
        (define progs (entry-progs (car queue)))
        (define ins (entry-states (car queue)))
        (set! (cdr queue))
        (define iterator (find-program (map progstate->id ins) state2-spec))
        
        ;; TODO: branch out
        )

      (loop states1)
      )
        
      

    ))
    