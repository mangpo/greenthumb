#lang racket

(require db data/queue)
(require "../path.rkt" "../ast.rkt" "../graph.rkt" 
         "arm-machine.rkt" 
         "arm-simulator-racket.rkt" "arm-validator.rkt" "arm-parser.rkt")

(provide arm-psql% time% (struct-out record))

(struct record (progs states))

(define time%
  (class object%
    (super-new)
    (init-field total-start)
    
    (define total 0)

    (define types '#(z3 sym term eval normal-test extra-test db-insert db-delete db-select hash string vector))
    (define times (make-vector (vector-length types) 0))
    (define times-start (make-vector (vector-length types) #f))

    (define solver-start #f)
    (define solver-ce 0)
    (define solver-noce 0)
    (define ce 0)
    (define noce 0)
   
    (define/public (print-ce)
      (pretty-display (format "#ce:\t~a" ce))
      (pretty-display (format "#no-ce:\t~a" noce)))
      

    (define/public (reset) 
      (set! total-start (current-seconds))
      (set! solver-start 0)
      (set! solver-ce 0)
      (set! solver-noce 0)
      (set! ce 0)
      (set! noce 0)
      (set! times (make-vector (vector-length types) 0))
      (set! times-start (make-vector (vector-length types) #f))
      )
    (define/public (terminate) (set! total (- (current-seconds) total-start)))

    (define/public (start type)
      (vector-set! times-start (vector-member type types) (current-milliseconds)))

    (define/public (end type)
      (define index (vector-member type types))
      ;; (unless (vector-ref times-start index)
      ;;         (pretty-display `(PROBLEM ,type)))
      (when (vector-ref times-start index)
            (vector-set! times index 
                         (+ (vector-ref times index)
                            (- (current-milliseconds) (vector-ref times-start index))))
            (vector-set! times-start index #f))
      )

    (define/public (start-solver) (set! solver-start (current-milliseconds)))

    (define/public (end-solver type) 
      (if type
          (begin
            (set! solver-ce (+ solver-ce (- (current-milliseconds) solver-start)))
            (set! ce (add1 ce)))
          (begin
            (set! solver-noce (+ solver-noce (- (current-milliseconds) solver-start)))
            (set! noce (add1 noce))))
      (set! solver-start #f))

    (define/public (print-stat)
      (newline)
      (pretty-display (format "#ce:\t~a" ce))
      (pretty-display (format "#no-ce:\t~a" noce))
      (newline)
      (when
       (> total 0)
       (pretty-display (format "solver-ce:\t~a\t~a" 
                               solver-ce (exact->inexact (/ solver-ce 1000 total))))
       (pretty-display (format "solver-noce:\t~a\t~a" 
                               solver-noce (exact->inexact (/ solver-noce 1000 total))))

       (define other (- (* 1000 total) solver-ce solver-noce))
       (for ([type types]
             [time times])
            (pretty-display 
             (format "~a:\t~a\t~a" 
                     type time (exact->inexact (/ time 1000 total))))
            (set! other (- other time)))
       (pretty-display (format "other:\t~a\t~a" other (exact->inexact (/ other 1000 total)))))
      )

    (define/public (print-stat-concise)
      (display (format "time: ~a\t| ~a, ~a # ~a" total solver-ce solver-noce (vector-ref times 3)))
      (for ([type '(z3 sym term)]
            [time times])
           (display (format ", ~a" time)))
      ;;;;;;;;;;;;;;;
      (display (format "\t| ~a, ~a # ~a"
                       (if (= ce 0) 0 (quotient solver-ce ce)) 
                       (if (= noce 0) 0 (quotient solver-noce noce))
                       (if (= ce 0) 0 (quotient (vector-ref times 3) ce))))
      (for ([type '(z3 sym term)]
            [time times])
           (display (format ", ~a"
                            (if (= (+ ce noce) 0) 0 (quotient time (+ ce noce))))))
      (display "\t||")
      ;;;;;;;;;;;;;;
      (for ([index (drop (range (vector-length types)) 4)])
           (let ([type (vector-ref types index)]
                 [time (vector-ref times index)])
             (unless (member type '(db-insert db-delete db-select))
                     (display (format " ~a," time)))))
      (newline)
      )
    
    ))

(define arm-psql%
  (class object% ;; TODO arm-enumerative%
    (super-new)
    (init-field machine printer time)
    (public db-connect db-disconnect create-table  
            progstate->id progstate->string
            tx-begin tx-commit)

    (define simulator (new arm-simulator-racket% [machine machine]))
    (define validator (new arm-validator% [machine machine] [printer printer] [time time]))
    (define parser (new arm-parser%))

    (define debug #f)
    (define pgc #f)
    (define bit (get-field bit machine)) ;; machine specific
    (define nregs (send machine get-nregs)) ;; machine specific
    (define nmems (send machine get-nmems)) ;; machine specific
    (define fp (send machine get-fp))
    (define prog-id 0)

    (define fixed-list
      (send validator generate-input-states 128 (vector) (send machine no-assumption) #f))
    (define ce-list (list))
    (define ce-len 64)
       
    ;; extra
      
    (define constraint-all (send machine constraint-all))
    (define constraint-all-vec (send machine progstate->vector constraint-all))

    (define reg-range-db
      (cond
       [(= nregs 1) (list->vector (range -31 33))]
       [(= nregs 2) (list->vector (range -3 5))]
       [(= nregs 3) (list->vector (range -1 3))]
       [else (raise "arm-psql: does not support nregs > 3")]))
    (define z-range-db (vector -1 0 1 2 3))

    ;; TODO: include #live-in
    (define table-name (format "new_arm_r~a_m~a" nregs nmems))

    (define (db-connect)
      (set! pgc (postgresql-connect #:user "mangpo" #:database "mangpo" #:password "")))

    (define (db-disconnect)
      (disconnect pgc))

    (define (tx-begin) (start-transaction pgc))
                                          ;; #:isolation 'read-committed 
                                          ;; #:option 'read-only))
    (define (tx-commit) (commit-transaction pgc))

    (define (create-table size)
      (pretty-display `(create-table ,size))
      (query-exec pgc (format "create table ~a_size~a (~a)" 
                              table-name size (get-header)))
      (query-exec pgc (format "create table program_id_size~a (id integer,program text)" 
                              size))
      ;(create-index-full size)
      )

    ;; (define (create-index-full size)
    ;;   (define lst (list))
    ;;   (define batch (+ nregs nmems 1))
    ;;   (for ([t (quotient 32 batch)])
    ;;        (for ([r nregs]) (set! lst (cons (format "o~a_r~a" t r) lst)))
    ;;        (for ([m nmems]) (set! lst (cons (format "o~a_m~a" t m) lst)))
    ;;        (set! lst (cons (format "o~a_z" t) lst)))
    ;;   (define query (format "create index ~a_size~a_idx_full on ~a_size~a(~a)"
    ;;                         table-name size table-name size
    ;;                         (string-join (reverse lst) ",")))
    ;;   (pretty-display (format "query: ~a" query))
    ;;   (query-exec pgc query)
    ;;   )


    (define (get-header)
      (define o (open-output-string))
      (for ([r nregs]) (display (format "in_r~a integer, " r) o))
      (for ([m nmems]) (display (format "in_m~a integer, " m) o))
      (display "in_z integer, " o)
      (for ([r nregs]) (display (format "out_r~a integer, " r) o))
      (for ([m nmems]) (display (format "out_m~a integer, " m) o))
      (display "out_z integer, " o)
      (display "id integer" o)
      (get-output-string o))

    (define/public (bulk-insert size classes states-in-str all) ;; TODO in-states-str
      ;; caution: may insert duplicate outputs to existing rows
      (pretty-display `(bulk-insert ,size))
      (send time start `db-insert)
      (define bulk-port (open-output-file (format "~a/tmp.csv" srcpath) 
                                          #:exists 'truncate))
      (define id-port (open-output-file (format "~a/id.csv" srcpath) 
                                        #:exists 'truncate))
      (send time end `db-insert)

      (define (inner-insert states-out prog-list)
        (define states-out-str (map (lambda (x) (progstate->string x)) states-out))

        (send time start `db-insert)
        (for ([p prog-list])
             ;; Insert to behavior table
             (for ([in-str states-in-str]
                   [out states-out])
                  (let ([out-str (progstate->string out)])
                    (pretty-display (format "~a,~a,~a" in-str out-str prog-id) bulk-port)))

             ;; Insert to prog ID table
             (send time start `db-insert)
             (display (format "~a," prog-id) id-port)
             (display "\"" id-port)
             (parameterize 
              ([current-output-port id-port])
              (send printer print-syntax (send printer decode p)))
             (pretty-display "\"" id-port)
             (send time end `db-insert)
             (set! prog-id (add1 prog-id)))
        (send time end `db-insert))

      (send time start `hash)
      (define pairs (hash->list classes))
      (unless all (set! pairs (take pairs (quotient (length pairs) 2))))
      (send time end `hash)
      (for ([pair pairs])
           (send time start `vector)
           (let* ([key (car pair)]
                  [prog-list (cdr pair)]
                  [outputs (map (lambda (x) (send machine vector->progstate x)) key)])
             (send time end `vector)
             (inner-insert outputs prog-list)
             (unless all
                     (send time start `hash)
                     (hash-remove! classes key)
                     (send time end `hash)
                     )
             ))

      (send time start `db-insert)
      (close-output-port bulk-port)
      (close-output-port id-port)
      (query-exec 
       pgc 
       (format "copy ~a_size~a from '~a/tmp.csv' with (format csv, null 'null')" 
               table-name size srcpath))
      (query-exec 
       pgc 
       (format "copy program_id_size~a from '~a/id.csv' with (format csv, null 'null')" 
               size srcpath))
      (pretty-display 
       (format "copy ~a_size~a from '~a/tmp.csv' with (format csv, null 'null')" 
               table-name size srcpath))
      (pretty-display
       (format "copy program_id_size~a from '~a/id.csv' with (format csv, null 'null')" 
               size srcpath))
      (send time end `db-insert)
      (send validator reset)
      )

    (define/public (same? x y)
      (define 
        all-correct
        (for/and ([ce (append ce-list fixed-list)])
                 (send time start `extra-test)
                 (let ([x-out 
                        (with-handlers*
                         ([exn? (lambda (e) #f)])
                         (send simulator interpret x ce #:dep #f))]
                       [y-out
                        (with-handlers*
                         ([exn? (lambda (e) #f)])
                         (send simulator interpret y ce #:dep #f))])
                   (send time end `extra-test)
                   (if
                    (and x-out y-out)
                    (begin
                      (send time start `vector)
                      (let ([x-out-vec (send machine progstate->vector x-out)]
                            [y-out-vec (send machine progstate->vector y-out)])
                        (send time end `vector)
                        (send time start `extra-test)
                        (let ([ret (send machine state-eq? x-out-vec y-out-vec 
                                         constraint-all-vec)])
                          (send time end `extra-test)
                          ret
                          )))
                    (and (not x-out) (not y-out))))))

      (when all-correct 
            (when debug (pretty-display "CE: search"))
            (send time start-solver)
            )

      ;; (when all-correct
      ;; (with-handlers* 
      ;;  ([exn:break? (lambda (e) 
      ;;                 (if (send validator counterexample x y constraint-all #f)
      ;;                     (pretty-display "TO: ce")
      ;;                     (pretty-display "TO: no")))])
      ;;  (timeout 1 (send validator counterexample x y constraint-all #f))))

      (with-handlers* 
       ;; when timeout, usually thiere is no CE => same
       ([exn:break? (lambda (e) 
                      (when debug (pretty-display "CE: timeout")) 
                      (send time end-solver #f)
                      (send time end `z3)
                      #f)])
       (if all-correct
           (let ([ce (timeout 5 (send validator counterexample x y constraint-all #f))])
             (send time end-solver (if ce #t #f))
             (when debug (when all-correct (pretty-display "CE: done")))
             (if ce
                 (begin
                   (when debug
                         (send printer print-syntax (send printer decode x))
                         (pretty-display "===========")
                         (send printer print-syntax (send printer decode y))
                         (pretty-display `(ce-list ,(length ce-list))))
                   (if (= (length ce-list) ce-len)
                       (set! ce-list (cons ce (remove (last ce-list) ce-list)))
                       (set! ce-list (cons ce ce-list)))
                   #f)
                 #t))
           #f)
       )
      ;all-correct
      )

    (define (select-one-state-in-out in-id out live)
      (define reg-out (progstate-regs out))
      (define reg-live (progstate-regs live))
      (define lst (list))
      (for ([i (vector-length reg-out)]
            [r reg-out]
            [r-live reg-live])
           (when r-live (set! lst (cons (format "o~a_r~a = ~a" in-id i r) lst))))

      (when (progstate-z live)
            (set! lst (cons (format "o~a_z = ~a" in-id (progstate-z out)) lst)))
      (string-join (reverse lst) " and "))

    ;; return list of program
    (define (select-from-in-out size states-in-id states-out live)
      (define ret #f)
      (cond
       [(= 0 (length (filter number? states-in-id)))
        (define query 
          (format "select program from ~a_size~a"
                  table-name size))
        (when debug (pretty-display (format "query: ~a" query)))
        (send time start `db-select)
        (set! ret (query-rows pgc query))
        (send time end `db-select)
        ]

       [else
        (define lst (list))
        (for ([in-id states-in-id]
              [out states-out])
             (when (number? in-id) 
                   (set! lst (cons (select-one-state-in-out in-id out live) lst)))) ;; TODO
        (define query 
          (format "select program from ~a_size~a where ~a"
                  table-name size (string-join (reverse lst) " and ")))
        (when debug (pretty-display (format "query: ~a" query)))
        (send time start `db-select)
        (set! ret (query-rows pgc query))
        (send time end `db-select)
        ])
      
      (flatten
       (for/list 
        ([xs ret])
        (map (lambda (x) (send printer encode (send parser ast-from-string x)))
             (string-split (vector-ref xs 0) ";"))))
      )


    (define/public (select-all size)
      (define query (format "select * from ~a_size~a" table-name size))
      (pretty-display query)

      (for/list 
       ([row (query-rows pgc query)])
       (let* ([ans (db->prog-progstates row)]
	      [progs (map (lambda (x) (send printer encode (send parser ast-from-string x)))
			  (record-progs ans))]
	      [states (record-states ans)])
	 (record progs states))))

    (define (ids2columns states-in-id)
      (define lst (list))
      (for ([in-id states-in-id])
           (when (number? in-id)
                 (let ([tmp (list)])
                   (for ([r nregs]) (set! tmp (cons (format "o~a_r~a" in-id r) tmp)))
                   (set! tmp (cons (format "o~a_z" in-id) tmp))
                   (set! lst (cons (string-join (reverse tmp) ",") lst)))))
      (string-join (reverse lst) ","))

    (define (select-count size)
      (query-value pgc (format "select count(*) from ~a_size~a" table-name size)))

    (define (select-from-in size states-in-id 
                            #:columns [columns #f] 
                            #:offset [offset #f] #:limit [limit #f])
      (define query
        (format "select ~a,program from ~a_size~a" 
                (if columns
                    columns
                    (ids2columns states-in-id))
                table-name size))
      (when limit (set! query (format "~a limit ~a" query limit)))
      (when offset (set! query (format "~a offset ~a" query offset)))
      (when debug (pretty-display (format "query: ~a" query)))

      (send time start `db-select)
      (define ans (query-rows pgc query))
      (send time end `db-select)
      (map (lambda (x) (db->prog-progstates x states-in-id)) ans))
    
    (define/public (get-all-states)
      (define nmems (send machine get-nmems))
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
      (send time start `string)
      (define ret
      (cond
       [state
	(define regs (progstate-regs state))
	(define memory (progstate-memory state))
	(define regs-str (string-join (map number->string (vector->list regs)) ","))
	(define memory-str (string-join (map number->string (vector->list memory)) ","))
	(if (= (vector-length memory) 0)
	    (format "~a,~a" regs-str (progstate-z state))
	    (format "~a,~a,~a" regs-str memory-str (progstate-z state)))
        ]

       [else

	(define regs-str (string-join (build-list nregs (lambda (x) "null")) ","))
	(define memory-str (string-join (build-list nmems (lambda (x) "null")) ","))
	(if (= nmems 0)
	    (format "~a,null" regs-str)
	    (format "~a,~a,null" regs-str memory-str))
	]))
      (send time end `string)
      ret
      )


    (define (power b p)
      (if (= p 0) 1 (* b (power b (sub1 p)))))

    ;; top - #t, bottom - #f
    (define (progstate->id state)
      (cond
       [(progstate? state)
        ;;(send machine display-state state)
        (define z (progstate-z state))
        (define regs (progstate-regs state))
        (define id 0)
        (define len (vector-length reg-range-db))
        
        (for ([r regs]
              [i (reverse (range nregs))])
             (when (number? id)
                   (let ([index (vector-member r reg-range-db)])
                     ;;(pretty-display `(reg ,r ,index ,i ,(power len i)))
                     (cond
                      [index (set! id (+ id (* index (power len i))))]
                      [(sql-null? r) (set! id #f)]
                      [else (set! id #t)]))))

        (when (number? id)
              (set! id (+ id (* (vector-member z z-range-db) (power len nregs)))))
        
        ;;(pretty-display `(progstate->id ,id))
        id]
       [else state]))

    (define (print-graph ins-id x)
      (define count 0)
      (define visit (set))
      (define (inner x indent)
        (cond
         [(neighbor? x)
          (cond 
           [(equal? (vertex-ids (neighbor-node x)) ins-id)
            (display (format "~a|||" indent))
            (send printer print-syntax (send printer decode (neighbor-edge x)))
	    (pretty-display "---")
	    (set! count (add1 count))
            ]

           [(set-member? visit (neighbor-node x))
            (display (format "~a(.)" indent))
            (send printer print-syntax (send printer decode (neighbor-edge x)))
	    (pretty-display "---")
	    (set! count (add1 count))
            ]
           
           [else
            (set! visit (set-add visit (neighbor-node x)))
            (inner (vertex-from (neighbor-node x)) (string-append indent "  "))
            (send printer print-syntax (send printer decode (neighbor-edge x)) indent)
	    (pretty-display "---")
	    (set! count (add1 count))
            ])
          ]
           
         [(equal? x #f) 
	  (pretty-display (format "~a|||" indent))
	  ]

         [(list? x) (for ([i x]) (inner i indent))]
         [else 
	  (send printer print-syntax (send printer decode x) indent)
	  (pretty-display "---")
	  (set! count (add1 count))
	  ]))
      (inner (vertex-from x) "")
      count)


    (define (count-graph ins-id x)
      (define count 0)
      (define visit (set))
      (define (inner x)
        (cond
         [(neighbor? x)
          (cond 
           [(equal? (vertex-ids (neighbor-node x)) ins-id)
	    (set! count (add1 count))
            ]

           [(set-member? visit (neighbor-node x))
	    (set! count (add1 count))
            ]
           
           [else
            (set! visit (set-add visit (neighbor-node x)))
            (inner (vertex-from (neighbor-node x)))
	    (set! count (add1 count))
            ])
          ]
           
         [(equal? x #f) 
	  (void)
	  ]

         [(list? x) (for ([i x]) (inner i))]

         [else 
	  (set! count (add1 count))
	  ]))
      (inner (vertex-from x))
      count)
        
    
    ;; Convert DB response into list of (record prog-list states)
    ;; resp is a vector.
    ;; If ins-id = #f, parse all columns
    (define (db->prog-progstates resp [ins-id #f])
      (define states (list))
      (define index 0)

      (define (get-state)
        (if (sql-null? (vector-ref resp index))
            #f
            (progstate (vector-copy resp index (+ index nregs)) 
                       (vector-copy resp (+ index nregs) (+ index nregs nmems))
                       (vector-ref resp (+ index nregs nmems))
                       fp)))
      
      (cond
       [ins-id
        (for ([in ins-id])
             (if (number? in)
                 (begin
                   (set! states (cons (get-state) states))
                   (set! index (+ index nregs nmems 1)))
                 (set! states (cons in states))))]

       [else
        (define batch (+ nregs nmems 1))
        (for ([i (quotient (sub1 (vector-length resp)) batch)])
             (set! states (cons (get-state) states))
             (set! index (+ index batch)))])

      (when (> (vector-length resp) (add1 index))
            (pretty-display `(bug ,index ,(vector-length resp)))
            (raise "psql:get-state: index mismatches")
            )
      (record 
       (if (equal? (vector-ref resp index) "")
           (list "")
           (string-split (vector-ref resp index) ";"))
       (reverse states)))

    (define start-ids #f)
    ;(define ids2node (make-hash))

    (define/public (synthesize-window spec sketch prefix postfix constraint extra 
                               [cost #f] [time-limit 3600]
                               #:hard-prefix [hard-prefix (vector)] 
                               #:hard-postfix [hard-postfix (vector)]
                               #:assume-interpret [assume-interpret #t]
                               #:assume [assumption (send machine no-assumption)])

      (define spec-len (vector-length spec))
      (define ntests 16)
      (define inits
        (send validator generate-input-states ntests (vector-append prefix spec postfix)
              assumption extra 
              #:rand-func (lambda () (random-from-vec reg-range-db))
              #:db #t))
      ;; (define ntests 4)
      ;; (define inits
      ;;   (list (progstate (vector 0 4) (vector) -1 fp)
      ;;         (progstate (vector -3 1) (vector) -1 fp)
      ;;         (progstate (vector -3 4) (vector) -1 fp)
      ;;         (progstate (vector 2 -3) (vector) -1 fp)))
      
      (define live2 (send validator get-live-in postfix constraint extra))
      (define states1 
	(map (lambda (x) (send simulator interpret prefix x #:dep #f)) inits))
      (define states2-spec
	(map (lambda (x) (send simulator interpret spec x #:dep #f)) states1))
      (define states1-id
        (map (lambda (x) (progstate->id x)) states1))
      (define states2-spec-id
        (map (lambda (x) (progstate->id x)) states2-spec))
      (set! start-ids states1-id)

      (when #t
            (for ([i states1])
                 (send machine display-state i))
            (for ([i states2-spec])
                 (send machine display-state i))
            (pretty-display `(states1-id ,states1-id))
            (pretty-display `(states-spec-id ,states2-spec-id))
	    )
      

      (define graph (new graph% 
                         [machine machine] [validator validator] 
                         [simulator simulator] [printer printer] [parser parser]
                         [spec spec] [constraint constraint] 
                         [extra extra] [assumption assumption]))

      (define queue (make-queue))
      (define level (make-queue))
      (define first-node (make-vertex-root states1-id))
      ;;(vertex states1-id (list) (list) #f (make-hash) #f)])
      (enqueue! queue first-node)
      (enqueue! level 1)
      ;; (define mapping (make-hash))
      ;; (hash-set! mapping states1-id #f)
      ;; (set! ids2node (make-hash))
      ;; (hash-set! ids2node states1-id first-node)
      (db-connect)


      (define found #f)
      (define algo1 0)
      (define algo2 0)

      (define (iterate iterator)
	(define p (iterator))
	(when p
	      (set! found #t)
	      (iterate iterator)))

      (define (lookup-for in-node ins-id size)
        ;(pretty-display `(lookup-for ,size))
        (define prog-list (select-from-in-out size ins-id states2-spec live2))
        (unless (empty? prog-list)
                (pretty-display `(ins-id ,ins-id))
                (define n (count-graph states1-id in-node))
                (pretty-display `(edges ,(* n (length prog-list))))
                ;; (pretty-display "------------------------------------")
                ;; (for ([x prog-list])
                ;;      (send printer print-syntax (send printer decode x))
		;;      (pretty-display "---")
		;;      )
                
                (define my-node 
                  (make-vertex 
		   #t 
		   (map (lambda (x) (neighbor in-node x (send simulator performance-cost x))) 
			prog-list)))
                (pretty-display "11111111111")
                (define t1 (current-milliseconds))
                (iterate (send graph get-correct-iterator my-node))
                ;(pretty-display "22222222222")
                (define t2 (current-milliseconds))
                (iterate (send graph get-correct-iterator2 my-node))
                ;(pretty-display "33333333333")
                (define t3 (current-milliseconds))
		(define t-parse (send graph get-parse-time))
                (pretty-display `(time ,(- t2 t1 t-parse) ,(- t3 t2)))
		(set! algo1 (+ algo1 (- t2 t1 t-parse)))
		(set! algo2 (+ algo2 (- t3 t2)))
                (when found (raise "done"))
                ))
      
      (define max-size 2)
      (define visit #f)

      (define (add-edge my-node edge)
	(pretty-display "11111111111+++")
	(define t1 (current-milliseconds))
	(iterate (send graph get-correct-iterator my-node edge))
	;(pretty-display "22222222222+++")
	(define t2 (current-milliseconds))
	(iterate (send graph get-correct-iterator2 my-node edge))
	;(pretty-display "33333333333+++")
	(define t3 (current-milliseconds))
	(define t-parse (send graph get-parse-time))
	(pretty-display `(time ,(- t2 t1 t-parse) ,(- t3 t2)))
	(set! algo1 (+ algo1 (- t2 t1 t-parse)))
	(set! algo2 (+ algo2 (- t3 t2)))
	(when found (raise "done"))
	)
	
      
      (define (expand in-node ins-id len)
        (define size (modulo len max-size))
        (when (= size 0) (set! size max-size))
        (define n (select-count size))
        ;(pretty-display `(expand-start ,len ,(sub1 (quotient (- len size) max-size)) ,n))
        (define columns (ids2columns ins-id))
	(define ids2node (vector-ref visit (sub1 (quotient (- len size) max-size))))
        (for ([i n])
             (let* ([prog-states (car (select-from-in size ins-id 
                                                      #:columns columns 
                                                      #:offset i #:limit 1))] 
                    ;; TODO: batching
                    [progs (record-progs prog-states)]
                    [states (record-states prog-states)]
                    [ids (map (lambda (x) (progstate->id x)) states)])
               (unless 
                (= (length (filter number? ids)) 0) ;; TODO: check
                ;(pretty-display `(expand ,i ,(hash-has-key? ids2node ids)))
                (let ([edges (map (lambda (x) (neighbor in-node x #f)) progs)])
                  (if (hash-has-key? ids2node ids)
                      (let ([my-node (hash-ref ids2node ids)])
                        (set-vertex-from! my-node 
                                          (append edges (vertex-from my-node)))
			(pretty-display `(hash-empty? ,(hash-empty? (vertex-children my-node))))
                        (unless
                         (hash-empty? (vertex-children my-node))
			 ;; caution:
			 ;; if children is empty -> no match afterward if this is the last expand
			 ;; & assume not calling coroutine if already get an answer.
                         (for ([edge edges])
			      (add-edge my-node edge)
			      ))
                        )
                      (let ([my-node (make-vertex ids edges)])
                        (hash-set! ids2node ids my-node)
                        (search (- len size) my-node ids))))))))
               

      (define (search len in-node ins-id)
        (define ins-id (vertex-ids in-node))
        (if (<= len max-size)
            (lookup-for in-node ins-id len)
            (expand in-node ins-id len)))

      (with-handlers*
       ([string? (lambda (e) 
		   (unless (equal? e "done")
			(raise e)))])
       (for ([len (range 1 4)]) 
	    (set! visit (make-vector (quotient (sub1 len) max-size) (make-hash)))
	    (newline)
	    (pretty-display `(SEARCH ,len))
	    (search len first-node states1-id)))
      (pretty-display (format "TIME: ~a vs ~a" (quotient algo1 1000) (quotient algo2 1000)))
      )
        
      

    ))
    
