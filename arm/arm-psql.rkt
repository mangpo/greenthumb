#lang racket

(require db data/queue)
(require "../ast.rkt" "../graph.rkt" 
         "arm-machine.rkt" 
         "arm-simulator-racket.rkt" "arm-validator.rkt" "arm-parser.rkt")

(provide arm-psql% time%)

(struct entry (prog states))

(define time%
  (class object%
    (super-new)
    (init-field total-start)
    
    (define total 0)
    (define normal-test 0)
    (define extra-test 0)
    (define solver-ce 0)
    (define solver-noce 0)
    (define ce 0)
    (define noce 0)

    (define normal-test-start #f)
    (define extra-test-start #f)
    (define solver-start #f)
    
    (define/public (start) (set! total-start (current-seconds)))
    (define/public (end) (set! total (- (current-seconds) total-start)))

    (define/public (start-normal-test) (set! normal-test-start (current-seconds)))
    (define/public (start-extra-test) (set! extra-test-start (current-seconds)))
    (define/public (start-solver) (set! solver-start (current-seconds)))

    (define/public (end-normal-test) 
      (set! normal-test (+ normal-test (- (current-seconds) normal-test-start)))
      (set! normal-test-start #f))

    (define/public (end-extra-test) 
      (set! extra-test (+ extra-test (- (current-seconds) extra-test-start)))
      (set! extra-test-start #f))

    (define/public (end-solver type) 
      (if type
          (begin
            (set! solver-ce (+ solver-ce (- (current-seconds) solver-start)))
            (set! ce (add1 ce)))
          (begin
            (set! solver-noce (+ solver-noce (- (current-seconds) solver-start)))
            (set! noce (add1 noce))))
      (set! solver-start #f))

    (define/public (print-stat)
      (newline)
      (pretty-display (format "normal-test:\t~a" (exact->inexact (/ normal-test total))))
      (pretty-display (format "extra-test:\t~a" (exact->inexact (/ extra-test total))))
      (pretty-display (format "solver-ce:\t~a" (exact->inexact (/ solver-ce total))))
      (pretty-display (format "solver-noce:\t~a" (exact->inexact (/ solver-noce total))))
      (pretty-display 
       (format "other:\t\t~a" 
               (exact->inexact (/ (- total 
                                     (+ normal-test extra-test solver-ce solver-noce)) 
                                  total))))

      (newline)
      (pretty-display (format "#ce:\t~a" ce))
      (pretty-display (format "#no-ce:\t~a" noce))
      )
    
    ))

(define arm-psql%
  (class object% ;; TODO arm-enumerative%
    (super-new)
    (init-field machine printer time)
    (public db-connect db-disconnect init create-table insert check-delete)

    (define simulator (new arm-simulator-racket% [machine machine]))
    (define validator (new arm-validator% [machine machine] [printer printer]))
    (define parser (new arm-parser%))

    (define debug #f)
    (define pgc #f)
    (define nregs (send machine get-nregs))
    (define nmems (send machine get-nmems))
    (define fp (send machine get-fp))
    (define state-cols #f)

    (define ce-list (list))
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

    (define (insert size cost states-in states-out p)
      (pretty-display "insert: start")
      (send printer print-syntax (send printer decode p))
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
         (format "insert into ~a_size~a (~a,cost,program) values (~a,~a,'~a')" 
                 table-name size
                 (string-join keys ",") (string-join vals ",")
                 cost (get-output-string o)))
       ;;(pretty-display (format "query: ~a" query))
       (query-exec pgc query)
       (pretty-display "insert: done")
       ))


    (define (same? x y)
      (define 
        all-correct
        (for/and ([ce ce-list])
                 (send time start-extra-test)
                 (let ([x-out 
                        (with-handlers*
                         ([exn? (lambda (e) #f)])
                         (send simulator interpret x ce #:dep #f))]
                       [y-out
                        (with-handlers*
                         ([exn? (lambda (e) #f)])
                         (send simulator interpret y ce #:dep #f))])
                   (send time end-extra-test)
                   (if
                    (and x-out y-out)
                    (let ([x-out-vec (send machine progstate->vector x-out)]
                          [y-out-vec (send machine progstate->vector y-out)])
                      (send machine state-eq? x-out-vec y-out-vec constraint-all-vec))
                    (and (not x-out) (not y-out))))))

      (when all-correct 
            (when debug (pretty-display "CE: search"))
            (send time start-solver)
            )

      (with-handlers* 
       ([exn:break? (lambda (e) (pretty-display "CE: timeout") #f)])
       (if all-correct
           (let ([ce (timeout 120 (send validator counterexample x y constraint-all #f))])
             (send time end-solver (if ce #t #f))
             (when debug (when all-correct (pretty-display "CE: done")))
             (if ce
                 (begin
                   (when debug (pretty-display `(ce-list ,(length ce-list))))
                   (set! ce-list (cons ce ce-list))
                   #f)
                 #t))
           #f)))

    (define (check-delete size cost states-in states-out p)
      (when debug (pretty-display "check: start"))
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

       (define unique #t)
       (define act #t)
       (define filtered-ids (list))
       (define filtered-states-out (list))
       (for ([i (length states-out)]
             [out states-out])
            (when out 
                  (set! filtered-ids (cons i filtered-ids))
                  (set! filtered-states-out (cons out filtered-states-out))))

       (for ([i (range 1 size)] #:break (not unique))
            (let ([rets 
                   (select-from-in-out i 
                                       (reverse filtered-ids)
                                       (reverse filtered-states-out)
                                       constraint-all)])
              (when debug (pretty-display (format "check: ~a" (length rets))))
              (for ([ret rets] #:break (not unique))
                   (let* ([str (vector-ref ret 0)]
                          [ret-prog (send printer encode 
                                          (send parser ast-from-string str))]
                          [ret-cost (vector-ref ret 1)]
                          [same (same? ret-prog p)]) 
                     (when debug (pretty-display "check: done"))
                     ;; TODO constraint, extra
                     (when 
                      same
                      (set! unique #f)
                      (pretty-display "--------- same ----------")
                      (send printer print-syntax (send printer decode p))
                      (pretty-display "---")
                      (pretty-display str)
                      (if (<= ret-cost cost)
                          (set! act #f)
                          (let ([query
                                 (format "delete from ~a_size~a where program='~a'"
                                         table-name size str)])
                            (pretty-display "delete: start")
                            (query-exec pgc query)
                            (pretty-display "delete: done")
                            )))
                     ))))
              
       act
       ))

    (define (select-one-state-in-out in-id out live)
      (define reg-out (progstate-regs out))
      (define reg-live (progstate-regs live))
      (define lst (list))
      (for ([i (vector-length reg-out)]
            [r reg-out]
            [r-live reg-live])
           (when r-live (set! lst (cons (format "o~a_r~a = ~a" in-id i r) lst))))
      (string-join lst " and "))

    (define (select-from-in-out size states-in-id states-out live)
      (cond
       [(= 0 (length (filter number? states-in-id)))
        (define query 
          (format "select program from ~a_size~a"
                  table-name size))
        (when debug (pretty-display (format "query: ~a" query)))
        (query-rows pgc query)
        ]

       [else
        (define lst (list))
        (for ([in-id states-in-id]
              [out states-out])
             (when (number? in-id) 
                   (set! lst (cons (select-one-state-in-out in-id out live) lst)))) ;; TODO
        (define query 
          (format "select program, cost from ~a_size~a where ~a"
                  table-name size (string-join lst " and ")))
        ;;(when debug (pretty-display (format "query: ~a" query)))
        (query-rows pgc query)]))

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
      (define regs (progstate-regs state))
      (define memory (progstate-memory state))
      (define regs-str (string-join (map number->string (vector->list regs)) ","))
      (define memory-str (string-join (map number->string (vector->list memory)) ","))
      (if (= (vector-length memory) 0)
          (format "~a,~a" regs-str (progstate-z state))
          (format "~a,~a,~a" regs-str memory-str (progstate-z state))))


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
            (display (format "~a|||~a" indent (neighbor-edge x)))]

           [(set-member? visit (neighbor-node x))
            (display (format "~a(.)~a" indent (neighbor-edge x)))]
           
           [else
            (set! visit (set-add visit (neighbor-node x)))
            (inner (vertex-from (neighbor-node x)) (string-append indent "  "))
            (display (format "~a~a" indent (neighbor-edge x)))])]
           
         [(equal? x #f) (pretty-display (format "~a|||" indent))]

         [(list? x) (for ([i x]) (inner i indent))]

         [else (pretty-display (format "~a~a" indent x))]))
      (inner (vertex-from x) ""))
        
    
    ;; Convert DB response into list of (entry program states)
    ;; resp is a vector.
    (define (db->prog-progstates resp ins-id)
      (define states (list))
      (define index 0)
      (for ([in ins-id])
           (if (number? in)
               (let ([x (progstate (vector-copy resp index (+ index nregs)) 
                                   (vector)
                                   (vector-ref resp (+ index nregs))
                                   fp)])
                 ;;(send machine display-state x)
                 (set! states (cons x states))
                 (set! index (+ index nregs 1)))
               (set! states (cons in states))))
      ;;(pretty-display `(db->prog-progstates ,resp))
      ;;(pretty-display `(parsed ,(vector-ref resp index) ,(reverse states)))
      (entry (vector-ref resp index) (reverse states)))

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
                         [simulator simulator] [printer printer] [parser parser]
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

      (define (enqueue in-node my-level)
        (define prog-states-list (select-from-in 1 (vertex-ids in-node)))
        (pretty-display `(enqueue-new ,(length prog-states-list)))
        ;; TODO enqueue!
        (for ([prog-states prog-states-list])
             (let* ([prog (entry-prog prog-states)]
                    [states (entry-states prog-states)]
                    [ids (map progstate->id states)])
               (unless (member #f ids) ;; If there is #f at all, ignore it.
                       ;; (when (= (length (filter number? ids)) 0)
                       ;;       (pretty-display `(OUT ,ins-id ,current-progs ,prog))
                       ;;       (raise "out")
                       ;;       )
                       ;;(pretty-display `(hash-has-key? ,(hash-has-key? ids2node ids)))
                       (if (hash-has-key? ids2node ids)
                           (let ([my-node (hash-ref ids2node ids)])
                             (set-vertex-from! my-node 
                                             (cons (neighbor in-node prog)
                                                   (vertex-from my-node)))
			     ;; TODO: get-correct-iterator => either dfs in-node or connect-graph
			     ;; path doesn't have to be complete
			     (unless (hash-empty? (vertex-children my-node))
				     (iterate (send graph get-correct-iterator my-node 
						    (neighbor in-node prog)))) ;; TODO: check
			     )
                           (let ([my-node (make-vertex ids (list (neighbor in-node prog)))])
                             (hash-set! ids2node ids my-node)
                             (enqueue! queue my-node)
                             (enqueue! level (add1 my-level))
                             )))))
        )
      
      (define (loop)
        (define in-node (dequeue! queue))
        (define ins-id (vertex-ids in-node))
        (define my-level (dequeue! level))
        (when debug
              (newline)
              (pretty-display `(my-level ,my-level))
              (pretty-display `(number-of-pairs ,(length (filter number? ins-id)))))
        (cond
         [(= 0 (length (filter number? ins-id)))
          ;; post-pone this for later
          (enqueue! queue in-node)
          (enqueue! level my-level)]

         [else
          (define prog-list (map (lambda (x) (vector-ref x 0))
                                 (select-from-in-out 1 ins-id states2-spec live2)))
          (unless (empty? prog-list)
                  (pretty-display `(ins-id ,ins-id))
                  (pretty-display `(current))
                  (print-graph states1-id in-node)
                  (pretty-display prog-list)
                  (pretty-display "------------------------------------")
                  
                  (define my-node 
                    (make-vertex #t (map (lambda (x) (neighbor in-node x)) prog-list)))
                  (iterate (send graph get-correct-iterator my-node))
                  (when found (raise "done"))

                  )

          (enqueue in-node my-level)])
        (loop)
        )

      (loop)
      )
        
      

    ))
    
