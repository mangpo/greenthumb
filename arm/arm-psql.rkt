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

    (define types '#(normal-test extra-test db-insert db-delete db-select hash string vector))
    (define times (make-vector (vector-length types) 0))
    (define times-start (make-vector (vector-length types) #f))

    (define solver-start #f)
    (define solver-ce 0)
    (define solver-noce 0)
    (define ce 0)
    (define noce 0)
   
    (define/public (reset) (set! total-start (current-seconds)))
    (define/public (terminate) (set! total (- (current-seconds) total-start)))

    (define/public (start type)
      (vector-set! times-start (vector-member type types) (current-milliseconds)))

    (define/public (end type)
      (define index (vector-member type types))
      (vector-set! times index (+ (vector-ref times index)
                                  (- (current-milliseconds) (vector-ref times-start index))))
      (vector-set! times-start index #f)
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
      (pretty-display (format "solver-ce:\t~a\t~a" 
                              solver-ce (exact->inexact (/ solver-ce 1000 total))))
      (pretty-display (format "solver-noce:\t~a\t~a" 
                              solver-noce (exact->inexact (/ solver-noce 1000 total))))

      (define other (- (* 1000 total) solver-ce solver-noce))
      (for ([type types]
            [time times])
           (pretty-display (format "~a:\t~a\t~a" 
                                   type time (exact->inexact (/ time 1000 total))))
           (set! other (- other time)))
      (pretty-display (format "other:\t\t~a\t~a" other (exact->inexact (/ other 1000 total))))
      )
    
    ))

(define arm-psql%
  (class object% ;; TODO arm-enumerative%
    (super-new)
    (init-field machine printer time)
    (public db-connect db-disconnect init create-table insert progstate->id
            tx-begin tx-commit)

    (define simulator (new arm-simulator-racket% [machine machine]))
    (define validator (new arm-validator% [machine machine] [printer printer]))
    (define parser (new arm-parser%))

    (define debug #f)
    (define pgc #f)
    (define bit (get-field bit machine)) ;; machine specific
    (define nregs (send machine get-nregs)) ;; machine specific
    (define nmems (send machine get-nmems)) ;; machine specific
    (define fp (send machine get-fp))
    (define state-cols #f)

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
    (define table-name (format "arm_r~a_m~a" nregs nmems))

    (define (db-connect)
      (set! pgc (postgresql-connect #:user "mangpo" #:database "mangpo" #:password "")))

    (define (db-disconnect)
      (disconnect pgc))

    (define (tx-begin) (start-transaction pgc))
                                          ;; #:isolation 'read-committed 
                                          ;; #:option 'read-only))
    (define (tx-commit) (commit-transaction pgc))

    (define (create-table size ntests)
      (pretty-display `(create-table ,size))
      (define query (format "create table ~a_size~a (~a)" 
                            table-name size (get-header ntests)))
      (when debug (pretty-display (format "query: ~a" query)))
      (query-exec pgc query)
      ;(create-index-full size)
      )

    (define (create-index-full size)
      (define lst (list))
      (define batch (+ nregs nmems 1))
      (for ([t (quotient 32 batch)])
           (for ([r nregs]) (set! lst (cons (format "o~a_r~a" t r) lst)))
           (for ([m nmems]) (set! lst (cons (format "o~a_m~a" t m) lst)))
           (set! lst (cons (format "o~a_z" t) lst)))
      (define query (format "create index ~a_size~a_idx_full on ~a_size~a(~a)"
                            table-name size table-name size
                            (string-join (reverse lst) ",")))
      (pretty-display (format "query: ~a" query))
      (query-exec pgc query)
      )


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
      (display "program text" o)
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

    (define (insert size states-in states-out p)
      (when debug (pretty-display "insert: start"))
      ;(send printer print-syntax (send printer decode p))
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
       ;;(pretty-display (get-output-string o))

       (define query
         (format "insert into ~a_size~a (~a,program) values (~a,'~a')" 
                 table-name size
                 (string-join keys ",") (string-join vals ",")
                 (get-output-string o)))
       ;;(pretty-display (format "query: ~a" query))
       (send time start `db-insert)
       (query-exec pgc query)
       (send time end `db-insert)
       (when debug (pretty-display "insert: done"))
       ))

    ;; (define bulk-port #f)
    ;; (define/public (bulk-insert-start) 
    ;;   (send time start `db-insert)
    ;;   (set! bulk-port (open-output-file (format "~a/tmp.csv" srcpath) #:exists 'truncate))
    ;;   (send time end `db-insert)
    ;;   )
    ;; (define/public (bulk-insert-end size) 
    ;;   (send time start `db-insert)
    ;;   (close-output-port bulk-port)
    ;;   (define query
    ;;     (format "copy ~a_size~a from '~a/tmp.csv' with (format csv, null 'null')" 
    ;;             table-name size srcpath))
    ;;   (query-exec pgc query)
    ;;   (send time end `db-insert)
    ;;   )
    ;; (define/public (bulk-insert cost states-in states-out p)
    ;;   (for ([out states-out])
    ;;        (let ([str (progstate->string out)])
    ;;          (send time start `db-insert)
    ;;          (display str bulk-port)
    ;;          (display "," bulk-port)
    ;;          (send time end `db-insert)
    ;;        ))
    ;;   (send time start `db-insert)
    ;;   (display (format "~a,\"" cost) bulk-port)
    ;;   (parameterize ([current-output-port bulk-port])
    ;;     	    (send printer print-syntax (send printer decode p)))
    ;;   (pretty-display "\"" bulk-port)
    ;;   (send time end `db-insert)
    ;;   )

    (define/public (bulk-insert size classes) 
      ;; caution: may insert duplicate outputs to existing rows
      (pretty-display `(bulk-insert ,size))
      (send time start `db-insert)
      (define bulk-port (open-output-file (format "~a/tmp.csv" srcpath) 
                                          #:exists 'truncate))
      (send time end `db-insert)

      (define (inner-insert states-out prog-list)
        (for ([out states-out])
             (let ([str (progstate->string out)])
               (send time start `db-insert)
               (display str bulk-port)
               (display "," bulk-port)
               (send time end `db-insert)
               ))
        (send time start `db-insert)
        (display "\"" bulk-port)
        (define begin #t)
        (parameterize 
         ([current-output-port bulk-port])
         (for ([p prog-list])
              (if begin 
                  (set! begin #f)
                  (display ";"))
              (send printer print-syntax (send printer decode p))))
        (pretty-display "\"" bulk-port)
        (send time end `db-insert)
        )

      (send time start `hash)
      (define pairs (hash->list classes))
      (send time end `hash)
      (for ([pair pairs])
           (send time start `vector)
           (let* ([key (car pair)]
                  [prog-list (cdr pair)]
                  [outputs (map (lambda (x) (send machine vector->progstate x)) key)])
             (send time end `vector)
             (inner-insert outputs prog-list)))

      (send time start `db-insert)
      (close-output-port bulk-port)
      (define query
        (format "copy ~a_size~a from '~a/tmp.csv' with (format csv, null 'null')" 
                table-name size srcpath))
      (query-exec pgc query)
      (send time end `db-insert)
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

    ;; (define/public (check-db size cost states-in states-out p)
    ;;   (when debug (pretty-display "check: start"))
    ;;   (define keys (list))
    ;;   (define vals (list))
    ;;   (for ([col state-cols]
    ;;         [out states-out])
    ;;        (when out
    ;;              (set! keys (cons col keys))
    ;;              (set! vals (cons (progstate->string out) vals))))
    ;;   (unless 
    ;;    (empty? keys)
    ;;    (set! keys (reverse keys))
    ;;    (set! vals (reverse vals))
       
    ;;    (define o (open-output-string))
    ;;    (parameterize ([current-output-port o])
    ;;                  (send printer print-syntax (send printer decode p)))
    ;;    ;;(pretty-display (get-output-string o))

    ;;    (define unique #t)
    ;;    (define act #t)
    ;;    (define filtered-ids (list))
    ;;    (define filtered-states-out (list))
    ;;    (for ([i (length states-out)]
    ;;          [out states-out])
    ;;         (when out 
    ;;               (set! filtered-ids (cons i filtered-ids))
    ;;               (set! filtered-states-out (cons out filtered-states-out))))

    ;;    (for ([i (range 1 (add1 size))] #:break (not unique))
    ;;         (let ([rets 
    ;;                (select-from-in-out i 
    ;;                                    (reverse filtered-ids)
    ;;                                    (reverse filtered-states-out)
    ;;                                    constraint-all)])
    ;;           (when debug (pretty-display (format "check: ~a" (length rets))))
    ;;           (for ([ret-prog rets] #:break (not unique))
    ;;                (let ([ret-cost (send simulator performance-cost ret-prog)]
    ;;                      [same (same? ret-prog p)])
    ;;                  (when debug (pretty-display "check: done"))
    ;;                  ;; TODO constraint, extra
    ;;                  (when 
    ;;                   same
    ;;                   (set! unique #f)
    ;;                   (when debug
    ;;                         (pretty-display "--------- same ----------")
    ;;                         (send printer print-syntax (send printer decode p))
    ;;                         (pretty-display "---")
    ;;                         (send printer print-syntax (send printer decode ret-prog)))
    ;;                   (if (<= ret-cost cost)
    ;;                       (set! act #f)
    ;;                       (let ([query
    ;;                              (format "delete from ~a_size~a where program='~a'"
    ;;                                      table-name size str)])
    ;;                         (pretty-display "delete: start")
    ;;                         (send time start `db-delete)
    ;;                         (query-exec pgc query)
    ;;                         (send time end `db-delete)
    ;;                         (pretty-display "delete: done")
    ;;                         )))
    ;;                  )))
    ;;         )
              
    ;;    act
    ;;    ))

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
      (map (lambda (x) (db->prog-progstates x)) (query-rows pgc query)))

    (define (select-from-in size states-in-id)
      (define lst (list))
      (for ([in-id states-in-id])
           (when (number? in-id)
                 (let ([tmp (list)])
                   (for ([r nregs]) (set! tmp (cons (format "o~a_r~a" in-id r) tmp)))
                   (set! tmp (cons (format "o~a_z" in-id) tmp))
                   (set! lst (cons (string-join (reverse tmp) ",") lst)))))
      (define query
        (format "select ~a,program from ~a_size~a" 
                (string-join (reverse lst) ",") table-name size))
      (when debug (pretty-display (format "query: ~a" query)))
      (map (lambda (x) (db->prog-progstates x states-in-id)) (query-rows pgc query)))
    
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
      (define visit (set))
      (define (inner x indent)
        (cond
         [(neighbor? x)
          (cond 
           [(equal? (vertex-ids (neighbor-node x)) ins-id)
            ;(display (format "~a|||~a" indent (neighbor-edge x)))
            (display (format "~a|||" indent))
            (send printer print-syntax (send printer decode (neighbor-edge x)))
            ]

           [(set-member? visit (neighbor-node x))
            ;(display (format "~a(.)~a" indent (neighbor-edge x)))
            (display (format "~a(.)" indent))
            (send printer print-syntax (send printer decode (neighbor-edge x)))
            ]
           
           [else
            (set! visit (set-add visit (neighbor-node x)))
            (inner (vertex-from (neighbor-node x)) (string-append indent "  "))
            ;(display (format "~a~a" indent (neighbor-edge x)))
            (send printer print-syntax (send printer decode (neighbor-edge x)) indent)
            ])
          ]
           
         [(equal? x #f) (pretty-display (format "~a|||" indent))]

         [(list? x) (for ([i x]) (inner i indent))]
         [else (send printer print-syntax (send printer decode x) indent)]))
         ;[else (pretty-display (format "~a~a" indent x))]))
      (inner (vertex-from x) ""))
        
    
    ;; Convert DB response into list of (record prog-list states)
    ;; resp is a vector.
    ;; If ins-id = #f, parse all columns
    (define (db->prog-progstates resp [ins-id #f])
      (define states (list))
      (define index 0)
      
      (cond
       [ins-id
        (for ([in ins-id])
             (if (number? in)
                 (let ([x (progstate (vector-copy resp index (+ index nregs)) 
                                     (vector-copy resp (+ index nregs) (+ index nregs nmems))
                                     (vector-ref resp (+ index nregs nmems))
                                     fp)])
                   ;;(send machine display-state x)
                   (set! states (cons x states))
                   (set! index (+ index nregs nmems 1)))
                 (set! states (cons in states))))]

       [else
        (define batch (+ nregs nmems 1))
        (for ([i (quotient (sub1 (vector-length resp)) batch)])
             (let ([x (progstate (vector-copy resp index (+ index nregs)) 
                                 (vector-copy resp (+ index nregs) (+ index nregs nmems))
                                 (vector-ref resp (+ index nregs nmems))
                                 fp)])
               (set! states (cons x states))
               (set! index (+ index batch))))])

      (record 
       (if (equal? (vector-ref resp index) "")
           (list (vector))
           (map (lambda (x) (send printer encode (send parser ast-from-string x)))
                (string-split (vector-ref resp index) ";")))
       (reverse states)))

    (define start-ids #f)
    (define ids2node (make-hash))

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
      (when #t
            (for ([i states1])
                 (send machine display-state i))
            (pretty-display `(states1-id ,states1-id))
            (for ([i states2-spec])
                 (send machine display-state i)))
      (set! start-ids states1-id)

      (define graph (new graph% 
                         [machine machine] [validator validator] 
                         [simulator simulator] [printer printer]
                         [spec spec] [constraint constraint] 
                         [extra extra] [assumption assumption]
                         [start-ids states1-id]))

      (define queue (make-queue))
      (define level (make-queue))
      (let ([first-node (make-vertex states1-id)])
             ;;(vertex states1-id (list) (list) #f (make-hash) #f)])
        (enqueue! queue first-node)
        (enqueue! level 1)
        ;; (define mapping (make-hash))
        ;; (hash-set! mapping states1-id #f)
        (set! ids2node (make-hash))
        (hash-set! ids2node states1-id first-node)
        (db-connect))


      (define found #f)

      (define (iterate iterator)
	(define p (iterator))
	(when p
	      (set! found #t)
	      (iterate iterator)))

      (define (enqueue in-node my-level size)
        (define prog-states-list (select-from-in size (vertex-ids in-node)))
        (pretty-display `(enqueue-new ,(length prog-states-list)))
        ;; TODO enqueue!
        (for ([prog-states prog-states-list])
             (let* ([progs (record-progs prog-states)]
                    [states (record-states prog-states)]
                    [ids (map (lambda (x) (progstate->id x)) states)])
               (unless (member #f ids) ;; If there is #f at all, ignore it.
                       ;; (when (= (length (filter number? ids)) 0)
                       ;;       (pretty-display `(OUT ,ins-id ,current-progs ,prog))
                       ;;       (raise "out")
                       ;;       )
                       ;;(pretty-display `(hash-has-key? ,(hash-has-key? ids2node ids)))
                       (let ([edges (map (lambda (x) (neighbor in-node x)) progs)])
                         (if (hash-has-key? ids2node ids)
                             (let ([my-node (hash-ref ids2node ids)])
                               (set-vertex-from! my-node 
                                                 (append edges (vertex-from my-node)))
                               ;; TODO: get-correct-iterator => either dfs in-node or connect-graph
                               ;; path doesn't have to be complete
                               (unless 
                                (hash-empty? (vertex-children my-node))
                                (for ([edge edges])
                                     (iterate (send graph get-correct-iterator my-node 
                                                    edge)))) ;; TODO: check
                               )
                             (let ([my-node (make-vertex ids edges)])
                               (hash-set! ids2node ids my-node)
                               (enqueue! queue my-node)
                               (enqueue! level (add1 my-level))
                               ))))))
        )

      (define (search-for in-node ins-id size)
        (pretty-display `(search-for ,size))
        (define prog-list (select-from-in-out size ins-id states2-spec live2))
        (unless (empty? prog-list)
                (pretty-display `(ins-id ,ins-id))
                (pretty-display `(current))
                (print-graph states1-id in-node)
                (for ([x prog-list])
                     (send printer print-syntax (send printer decode x)))
                (pretty-display "------------------------------------")
                
                (define my-node 
                  (make-vertex #t (map (lambda (x) (neighbor in-node x)) prog-list)))
                (iterate (send graph get-correct-iterator my-node))
                (when found (raise "done"))
                ))
      
      (define max-size 2)
      (define (loop)
        (define in-node (dequeue! queue))
        (define ins-id (vertex-ids in-node))
        (define my-level (dequeue! level))
        (when #t
              (newline)
              (pretty-display `(my-level ,my-level))
              (pretty-display `(number-of-pairs ,(length (filter number? ins-id)))))
        (cond
         [(= 0 (length (filter number? ins-id)))
          ;; post-pone this for later
          (enqueue! queue in-node)
          (enqueue! level my-level)]

         [(> my-level 1)
          (search-for in-node ins-id max-size)
          (enqueue in-node my-level max-size)]

         [else
          (for ([i (range 1 (add1 max-size))])
               (search-for in-node ins-id i))
          (for ([i (range 1 (add1 max-size))])
               (enqueue in-node my-level i))
          ]

         )
        (loop)
        )

      (loop)
      )
        
      

    ))
    
