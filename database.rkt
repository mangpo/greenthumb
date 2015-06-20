#lang racket

(require racket/generator db)
(require "path.rkt" "arm/arm-machine.rkt")

(provide database%)

(struct concat (collection inst))
(struct box (val))

(define database%
  (class object%
    (super-new)
    (init-field machine enum simulator simulator-precise
                printer parser
                validator validator-precise)
    (abstract get-all-states vector->id progstate->id progstate->ids
              reduce-precision increase-precision)
    (public synthesize-window)
    
    (define debug #f)

    (define (hash-insert! x k v)
         (if (hash-has-key? x k)
             (hash-set! x k (append v (hash-ref x k)))
             (hash-set! x k v)))
    
    (define (hash-add! x k v)
         (if (hash-has-key? x k)
             (hash-set! x k (cons v (hash-ref x k)))
             (hash-set! x k (list v))))
    
    (define/public (gen-behavior-base)
      (define constraint-all (send machine constraint-all))
      (define constraint-all-vec (send machine progstate->vector constraint-all))
      (define live-list (send machine get-operand-live constraint-all))
      
      (define all-states (get-all-states))
      (set! n-states (length all-states))
      (define all-states-id (range n-states))
      (pretty-display `(n-states ,n-states))

      (define inst-iterator
        (send enum reset-generate-inst 
              (send machine progstate->vector (car all-states))
              live-list #f `all #f))

      (define behavior2progs (make-hash))
      (define n-progs 0)

      (define (loop iterator)
        (define inst-liveout-vreg (iterator))
        (define my-inst (first inst-liveout-vreg))
        (define my-liveout (second inst-liveout-vreg))
        (define my-vreg (third inst-liveout-vreg))

        (when
         my-inst
         ;;(send printer print-syntax-inst (send printer decode-inst my-inst))
         (set! n-progs (add1 n-progs))
         (define behavior (make-vector n-states))
         (for ([state all-states]
               [id all-states-id])
              (let*
                  ([out
                    (with-handlers*
                     ([exn? (lambda (e) #f)])
                     (send simulator interpret (vector my-inst) state #:dep #f))]
                   [out-id (and out (progstate->id out))])
                ;; (pretty-display (format "INPUT(~a) >>>" id))
                ;; (send machine display-state state)
                ;; (pretty-display (format "OUTPUT(~a) >>>" out-id))
                ;; (send machine display-state out)
                (vector-set! behavior id out-id)))
         
         (hash-insert! behavior2progs behavior (list (vector my-inst)))

         (loop iterator)))

      (loop inst-iterator)
      (pretty-display (format "# of programs = ~a" n-progs))
      (pretty-display (format "# of behavior = ~a" (hash-count behavior2progs)))

      behavior2progs
      )

    (define (gen-behavior-usable)
      (define behavior2progs (gen-behavior-base))
      (define n (hash-count behavior2progs))
      (set! id2progs (make-vector n))
      (set! id2beh (make-vector n))
      (set! id2bw (make-vector n))

      (for ([pair (hash->list behavior2progs)]
            [id (in-naturals)])
           (let* ([beh (car pair)]
                  [beh-bw (make-vector (vector-length beh) #f)]
                  [progs (cdr pair)])

             (for ([in (in-naturals)]
                   [out beh])
                  (when out
                        (let ([val (vector-ref beh-bw out)])
                          (if val
                              (vector-set! beh-bw out (cons in val))
                              (vector-set! beh-bw out (list in))))))
             
             (vector-set! id2progs id progs)
             (vector-set! id2beh id beh)
             (vector-set! id2bw id beh-bw))))

    (define/public (gen-behavior)
      (clear-temp-storage)
      
      (define table-size1 (gen-behavior-base))
      (define n-size1 (hash-count table-size1))
      (define table-size2 (make-hash))
      (define count-p 0)
      (define prog_id 0)

      (for ([pair-a (hash->list table-size1)]
            [i n-size1])
           (pretty-display (format "~a/~a" i n-size1))
           (for ([pair-b (hash->list table-size1)])
                (let* ([behavior-a (car pair-a)]
                       [programs-a (cdr pair-a)]
                       [behavior-b (car pair-b)]
                       [programs-b (cdr pair-b)]
                       [behavior-out (concat-behavior behavior-a behavior-b)]
                       [programs-out (concat-programs programs-a programs-b)])
                  (hash-insert! table-size2 behavior-out programs-out)

                  (when (= 100000 count-p)
                        (pretty-display "[save]")
                        (set! prog_id (save-to-file table-size2 prog_id))
                        (set! table-size2 (make-hash))
                        (set! count-p 0)
                        (collect-garbage))
                  (set! count-p (add1 count-p))
                  )))

      (set! prog_id (save-to-file table-size2 prog_id))
      )

    (define (concat-behavior a b)
      (for/vector ([o a]) (and o (vector-ref b o))))

    (define (concat-programs a b)
      (for*/list ([ai a] [bi b]) (vector-append ai bi)))

    (define (behavior->string x)
      (string-join
       (for/list ([i x]) (if (number? i) (number->string i) "null"))
       ";"))

    (define (clear-temp-storage)
      (system (format "rm ~a/_programs.csv" srcpath))
      (system (format "rm ~a/_behaviors.csv" srcpath)))

    (define (save-to-file table prog_id)
      (define programs-port
        (open-output-file (format "~a/_programs.csv" srcpath) #:exists 'append))
      (define behaviors-port
        (open-output-file (format "~a/_behaviors.csv" srcpath) #:exists 'append))

      (for ([pair (hash->list table)])
           (let ([behavior (car pair)]
                 [programs (cdr pair)])
             (pretty-display (format "\"~a\",~a"
                                     (behavior->string behavior) prog_id)
                             behaviors-port)
             (parameterize
                 ([current-output-port programs-port])
               (display (format "~a,\"" prog_id))
               (send printer print-syntax (send printer decode (car programs)))
               (for ([program (cdr programs)])
                   (display ";")
                   (send printer print-syntax (send printer decode program)))
               (pretty-display "\""))
             (set! prog_id (add1 prog_id))
             ))
      (close-output-port programs-port)
      (close-output-port behaviors-port)
      prog_id
      )

    (define/public (save-to-db)
      (define pgc (postgresql-connect #:user "mangpo" #:database "mangpo" #:password ""))
      (query-exec pgc "create table arm_behaviors_size2 (behavior text,id integer primary key)")
      (query-exec pgc "create table arm_programs_size2 (id integer primary key,program text)")

      (query-exec
       pgc 
       (format "copy arm_behaviors_size2 from '~a/_behaviors.csv' with (format csv, null 'null')" srcpath))
      (query-exec 
       pgc 
       (format "copy arm_programs_size2 from '~a/_programs.csv' with (format csv, null 'null')" srcpath))
      
      (disconnect pgc)
      )
    
    (define (convert x) (if (equal? x "null") #f (string->number x)))

    (define/public (expand-behaviors)
      (define pgc (postgresql-connect #:user "mangpo" #:database "mangpo" #:password ""))
      (define inout-port
        (open-output-file (format "~a/_inout.csv" srcpath) #:exists 'truncate))
      (define behavior-port
        (open-input-file (format "~a/_behaviors.csv" srcpath)))
      (define progress 0)

      (define (loop)
        (define line (read-line behavior-port))
        (when
         (string? line)
         (define tokens (string-split line ","))
         (define behavior (map convert (string-split (first tokens) ";")))
         (define id (string->number (second tokens)))
         (for ([in (in-naturals)]
               [out behavior])
              (pretty-display (format "~a,~a,~a" id in out) inout-port))
         (when (= 0 (modulo progress 100))
               (pretty-display `(progress ,progress)))
         (set! progress (add1 progress))
         (loop)))
      (loop)

      (close-output-port inout-port)
      (close-input-port behavior-port)
      
      (query-exec pgc "create table arm_inout_size2 (id integer,in integer,out integer)")

      (query-exec
       pgc 
       (format "copy arm_inout_size2 from '~a/_inout.csv' with (format csv, null 'null')" srcpath))
      
      (disconnect pgc)
      )

    (define c-behaviors 0)
    (define c-progs 0)
    (define (class-insert! class states-id behavior)
      (set! c-progs (add1 c-progs))

      (define (inner x states-id behavior)
        (define key (car states-id))
        (if (= 1 (length states-id))
            (if (hash-has-key? x key)
                (hash-set! x key (cons behavior (hash-ref x key)))
		(begin
		  (set! c-behaviors (add1 c-behaviors))
		  (hash-set! x key (list behavior))))
            (begin
              (unless (hash-has-key? x key)
                      (hash-set! x key (make-hash)))
              (inner (hash-ref x key) (cdr states-id) behavior))))

      (inner class states-id behavior))

    (define c-behaviors-bw 0)
    (define c-progs-bw 0)
    (define (class-insert-bw! class states-id behavior)
      ;;(pretty-display `(class-insert-bw! ,states-id ,behavior))
      (set! c-progs-bw (add1 c-progs-bw))

      (define (inner x states-id behavior)
        (define key-list (car states-id))
        (if (= 1 (length states-id))
            (for ([key key-list])
                 (let ([val (vector-ref x key)])
                   (if val
                       (vector-set! x key (cons behavior val))
                       (begin
                         (set! c-behaviors-bw (add1 c-behaviors-bw))
                         (vector-set! x key (list behavior))))))
            (for ([key key-list])
                 (unless (vector-ref x key)
                         (vector-set! x key (make-vector n-states #f)))
                 (inner (vector-ref x key) (cdr states-id) behavior))))

      (inner class states-id behavior)
      ;;(pretty-display `(end ,class))
      )

    (define n-states #f)
    (define id2progs #f)
    (define id2beh #f)
    (define id2bw #f)

    (define (interpret prog in)
      (define ret in)
      (for ([x prog])
           (set! ret (and ret (vector-ref (vector-ref id2beh x) ret))))
      ret)

    ;; only works with programs with one instruction
    (define (interpret-bw prog out)
      ;;(pretty-display `(interpret-bw ,prog ,out))
      (define x (vector-ref prog 0))
      (vector-ref (vector-ref id2bw x) out))

    (define (get-programs pgc id)
      (define val (vector-ref id2progs id))
      (cond
       [(list? val) val]
       [else
        (define str (query-value pgc (format "select program from arm_programs_size2 where id = ~a" id)))
        ;; (define progs-str (string-split str ";"))
        ;; (define ret
        ;;   (for/list ([p-str progs-str])
        ;;             (send printer encode (send parser ast-from-string p-str))))
        (define p-str (car (string-split str ";")))
        (define ret (list (send printer encode (send parser ast-from-string p-str))))
        (vector-set! id2progs id ret)
        ret]))

    (define (load-behavior pgc id)
      (define str (query-value pgc (format "select behavior from arm_behaviors_size2 where id = ~a" id)))
      (define behavior (map convert (string-split str ";")))
      (define behavior-bw (make-vector (length behavior)))

      (for ([in (in-naturals)]
            [out behavior])
           (when out
                 (let ([val (vector-ref behavior-bw out)])
                   (if val
                       (vector-set! behavior-bw out (cons in val))
                       (vector-set! behavior-bw out (list in))))))
      (values (list->vector behavior) behavior-bw))


    (define (find-first-state x)
      (car (hash-keys x)))

    (define (convert-vec2id x)
      (make-hash
       (for/list ([pair (hash->list x)])
                 (let* ([key (car pair)]
                        [val (cdr pair)]
                        [new-val (if (hash? val) (convert-vec2id val) val)])
                   (cons (vector->id key) new-val)))))

    (define (get-collection-iterator collection)
      (define ans (list))
      (define (loop x postfix)
        (cond
         [(concat? x)
          (loop (concat-collection x) (vector-append (vector (concat-inst x)) postfix))]
         [(vector? x) 
          (set! ans (cons (vector-append x postfix) ans))]
         [(list? x) 
          (if (empty? x)
              (set! ans (cons postfix ans))
              (for ([i x]) (loop i postfix)))]
         [(set? x) 
          (if (set-empty? x)
              (set! ans (cons postfix ans))
              (for ([i x]) (loop i postfix)))]

         ))
      (loop collection (vector))
      ans)

    (define (count-collection x)
      (cond
       [(concat? x) (count-collection (concat-collection x))]
       [(vector? x) 1]
       [(list? x) (foldl + 0 (map count-collection x))]
       [else (raise (format "count-collection: unimplemented for ~a" x))]))

    (define t-load 0)
    (define t-build 0)
    (define t-build-inter 0)
    (define t-build-hash 0)
    (define t-build-hash2 0)
    (define t-intersect 0)
    (define t-interpret 0)
    (define t-extra 0)
    (define t-verify 0)
    (define c-build-hash 0)
    (define c-build-hash2 0)
    (define c-intersect 0)
    (define c-interpret 0)
    (define c-extra 0)

    (define t-collect 0)
    (define t-check 0)
    
    (define (synthesize-window spec sketch prefix postfix constraint extra 
                               [cost #f] [time-limit 3600]
                               #:hard-prefix [hard-prefix (vector)] 
                               #:hard-postfix [hard-postfix (vector)]
                               #:assume-interpret [assume-interpret #t]
                               #:assume [assumption (send machine no-assumption)])
      
      (send machine analyze-opcode prefix spec postfix)
      (send machine analyze-args prefix spec postfix #:vreg 0)
      (define live2 (send validator get-live-in postfix constraint extra))
      (define live2-vec (send machine progstate->vector live2))
      (define live1 (send validator get-live-in spec live2 extra))
      (define live1-list (send machine get-operand-live live1))
      (define live2-list (send machine get-operand-live live2))
             
      (define ntests 6)
      (define inits
        (send validator generate-input-states ntests (vector-append prefix spec postfix)
              assumption extra #:db #t))
      ;; p11
      ;; (define inits
      ;;   (list
      ;;    (progstate (vector 5 -5) (vector) -1 4)
      ;;    (progstate (vector 7 2) (vector) -1 4)))
      ;; p24
      ;; (define inits
      ;;   (list
      ;;    (progstate (vector 3 0 0 0 0 0) (vector) -1 4)
      ;;    (progstate (vector 71 0 0 0 0 0) (vector) -1 4)
      ;;    ))
      (define states1 
	(map (lambda (x) (send simulator interpret prefix x #:dep #f)) inits))
      (define states2
	(map (lambda (x) (send simulator interpret spec x #:dep #f)) states1))
      (define states1-vec 
	(map (lambda (x) (send machine progstate->vector x)) states1))
      (define states2-vec 
	(map (lambda (x) (send machine progstate->vector x)) states2))
      (define states1-id (map (lambda (x) (progstate->id x)) states1))
      (define states2-id-spec (map (lambda (x) (progstate->ids x live2)) states2))

      (pretty-display `(states1-vec ,states1-vec))
      (pretty-display `(states2-vec ,states2-vec))
      (pretty-display `(states2-id-spec ,states2-id-spec))
      (pretty-display `(live2-vec ,live2-vec))
      
      (define ce-limit 10000)
      (define ce-in (make-vector ce-limit))
      (define ce-out-vec (make-vector ce-limit))
      (define ce-in-id (make-vector ce-limit))
      (define ce-out-id (make-vector ce-limit))
      (define ce-count ntests)
      (define ce-count-extra ntests)

      (for ([i ntests]
            [in states1]
            [out states2-vec]
            [in-id states1-id]
            [out-id states2-id-spec])
           (vector-set! ce-in i in)
           (vector-set! ce-out-vec i out)
           (vector-set! ce-in-id i in-id)
           (vector-set! ce-out-id i out-id)
           )

      (define pgc (postgresql-connect #:user "mangpo" #:database "mangpo" #:password ""))
      (define n-behavior (add1 (query-value pgc "select max(id) from arm_behaviors_size2")))
      (set! id2progs (make-vector n-behavior #f))

      (define prev-classes (make-hash))
      (class-insert! prev-classes states1-vec (vector))

      (define classes (make-hash))

      (define (check-eqv progs h2 beh2 my-ce-count)
        (define t00 (current-milliseconds))

          (define (inner-progs p)
            ;; (when (= h2 3130)
            ;;       (pretty-display `(inner-progs))
            ;;       (send printer print-syntax (send printer decode p))
            ;;       (newline))
                 
            ;; (pretty-display "After renaming")
            ;; (send printer print-syntax (send printer decode p))
            (when debug
                  (pretty-display "[2] all correct")
                  (pretty-display `(ce-count-extra ,ce-count-extra))
                  )
            (when (= ce-count-extra ce-limit)
                  (raise "Too many counterexamples")
                  )
            
            (define ce (send validator counterexample 
                             (vector-append prefix spec postfix)
                             (vector-append prefix p postfix)
                             constraint extra #:assume assumption))

            (if ce
                (let* ([ce-input (send simulator interpret prefix ce #:dep #f)]
                       [ce-output
                        (send simulator interpret spec ce-input #:dep #f)]
                       [ce-input-id (progstate->id ce-input)]
                       [ce-output-id (progstate->ids ce-output live2)]
                       [ce-output-vec
                        (send machine progstate->vector ce-output)])
                  (when debug
                        (pretty-display "[3] counterexample")
                        (send machine display-state ce-input)
                        (pretty-display `(ce-out-vec ,ce-output-vec)))
                  (vector-set! ce-in ce-count-extra ce-input)
                  (vector-set! ce-out-vec ce-count-extra ce-output-vec)
                  (vector-set! ce-in-id ce-count-extra ce-input-id)
                  (vector-set! ce-out-id ce-count-extra ce-output-id)
                  (set! ce-count-extra (add1 ce-count-extra))
                  )
                (begin
                  (pretty-display "[4] FOUND!!!")
                  (send printer print-syntax (send printer decode p))
                  (pretty-display `(ce-count-extra ,ce-count-extra))
                  (raise p))))


          (define (inner-behaviors p1 h2 beh2)
            (define t0 (current-milliseconds))
            (when (concat? p1) (pretty-display `(my-ce-count ,my-ce-count ,ntests)))
            ;; (when (= h2 3130)
            ;;       (pretty-display `(ce ,my-ce-count ,ce-count-extra))
            ;;       (send printer print-syntax (send printer decode p1))
            ;;       (newline))

            (define
              pass
              (for/and ([i (reverse (range my-ce-count ce-count-extra))])
                       (let* ([input (vector-ref ce-in i)]
                              [output-ids (vector-ref ce-out-id i)]
                              [my-id-iter
                               ;; (with-handlers*
                               ;;  ([exn? (lambda (e) #f)])
                               (progstate->id (send simulator interpret p1 input #:dep #f))
                               ;; )
                               ]
                              [my-id
                               (and my-id-iter (vector-ref beh2 my-id-iter))])
                         ;; (when (= h2 3130)
                         ;;       (pretty-display `(id ,input ,output-ids ,my-id-iter ,my-id)))
                         (and my-id (member my-id output-ids)))))
            (define t1 (current-milliseconds))
            (set! t-extra (+ t-extra (- t1 t0)))
            (set! c-extra (+ c-extra (- ce-count-extra my-ce-count)))
            (when 
             pass
             (define half2 (get-programs pgc h2))

             (for* ([h2 half2])
                   (inner-progs (vector-append p1 h2)))
             (define t2 (current-milliseconds))
             (set! t-verify (+ t-verify (- t2 t1)))

             ))

          (define h1
            (if (= my-ce-count ntests)
                (get-collection-iterator progs)
                progs))
             
          (define t11 (current-milliseconds))
          
          (for ([p1 h1])
               (inner-behaviors p1 h2 beh2))
          (define t22 (current-milliseconds))
          (set! t-collect (+ t-collect (- t11 t00)))
          (set! t-check (+ t-check (- t22 t11)))
          )

      (define (refine my-classes id level)
        (define t00 (current-milliseconds))
        (define-values (behavior behavior-bw) (load-behavior pgc id))
        (define t11 (current-milliseconds))
        (set! t-load (+ t-load (- t11 t00)))
        (define (outer my-classes level)
          (define real-hash my-classes)
          ;; (when (list? real-hash)
          ;;       (pretty-display `(refine ,id ,level ,(count-collection real-hash))))
          (when (and (list? real-hash) (> (count-collection real-hash) 0))
                ;;(pretty-display "here!!!!!")
                ;; list of programs
                (define t0 (current-milliseconds))
                (set! real-hash (make-hash))
                (define input (vector-ref ce-in level))
                (define count-progs 0)
                (define (loop iterator)
                  (define prog (and (not (empty? iterator)) (car iterator)))
                  (when 
                   prog
                   (set! count-progs (add1 count-progs))
                   (let ([state
                          (progstate->id (send simulator interpret prog input #:dep #f))])
                     (if (hash-has-key? real-hash state)
                         (hash-set! real-hash state
                                    (cons prog (hash-ref real-hash state)))
                         (hash-set! real-hash state (list prog))))
                   (loop (cdr iterator))
                   ))

                (if (= level ntests)
                    (loop (get-collection-iterator my-classes)) ;; check
                    (loop my-classes)) ;; check
                (define t1 (current-milliseconds))
                (set! t-build (+ t-build (- t1 t0)))
                )

          (define expect (vector-ref ce-out-id level))

          (define (inner)
            (define t0 (current-milliseconds))
            (define inters-fw (list->set (hash-keys real-hash)))
            (define inters-bw
              (list->set
               (flatten (for/list ([e expect]) (vector-ref behavior-bw e)))))

            (define inters (set-intersect inters-fw inters-bw))
            (define t1 (current-milliseconds))
            (set! t-intersect (+ t-intersect (- t1 t0)))
            (set! c-intersect (add1 c-intersect))

            ;; (when (= id 3130)
            ;;       (pretty-display `(refine-inner ,expect ,inters-fw ,inters-bw ,inters)))

            (if (= 1 (- ce-count level))
                (begin
                  (for ([inter inters])
                       (check-eqv (hash-ref real-hash inter) id behavior ce-count))
                  (set! ce-count ce-count-extra)
                  )
                (for ([inter inters])
                     (hash-set! real-hash inter
                                (outer (hash-ref real-hash inter) (add1 level))))))
          
          (cond
           [(hash? real-hash)
            (inner)
            real-hash]

           [(list? real-hash)
            (check-eqv real-hash id behavior level)
            (box real-hash)]

           [(box? real-hash)
            (check-eqv (box-val real-hash) id behavior level)
            real-hash]

           )
          )
        (outer my-classes level)
        )

              ;; Enmerate all possible program of one instruction
      (define (enumerate iterator) 
        ;; Call instruction generator
        (define inst-liveout-vreg (iterator))
        (define my-inst (first inst-liveout-vreg))
        (define cache (make-hash))
        (when 
         my-inst
         
         (when debug
               (send printer print-syntax-inst (send printer decode-inst my-inst)))

         (define (recurse x states2-vec)
           (if (list? x)
               (class-insert! classes (reverse states2-vec) (concat x my-inst))
               (for ([pair (hash->list x)])
                    (let* ([state (car pair)]
                           [state-vec (send machine vector->progstate state)]
                           [val (cdr pair)]
                           [out 
                            (if (and (list? val) (hash-has-key? cache state-vec))
                                (hash-ref cache state-vec)
                                (let ([tmp
                                       (with-handlers*
                                        ([exn? (lambda (e) #f)])
                                        (send machine progstate->vector 
                                              (send simulator interpret 
                                                    (vector my-inst)
                                                    state-vec
                                                    #:dep #f)))])
                                  (when (list? val) (hash-set! cache state-vec tmp))
                                  tmp))
                            ])
                      (when out (recurse val (cons out states2-vec)))))))
         
         (recurse prev-classes (list))
         (enumerate iterator)))
      
      ;; Grow
      (for ([i 2])
        (newline)
        (pretty-display `(grow ,i))
        (set! classes (make-hash))
        (let ([state-rep (find-first-state prev-classes)]) ;; TODO
          (enumerate
           (send enum reset-generate-inst state-rep
                 (range (send machine get-nregs))
                 #f `all #f)))
        (set! prev-classes classes)
        (pretty-display `(behavior ,c-behaviors ,c-progs))
        (set! c-behaviors 0)
        (set! c-progs 0)
        )

      (newline)
      (pretty-display `(grow done))
      (set! prev-classes (convert-vec2id prev-classes))
      (pretty-display `(convert done))

      ;; Search
      (define ttt (current-milliseconds))
      (for ([id (range n-behavior)])
           (when (= 0 (modulo id 10))
                 (pretty-display (format "search ~a/~a | ~a = ~a ~a ~a/~a ~a/~a ~a = ~a ~a" id n-behavior
                                         (- (current-milliseconds) ttt)
                                         t-load t-build
                                         t-intersect c-intersect
                                         t-extra c-extra
                                         t-verify
                                         t-collect t-check
                                         ))
                 (set! t-load 0) (set! t-build 0) (set! t-intersect 0) (set! t-extra 0) (set! t-verify 0)
                 (set! c-intersect 0) (set! c-extra 0)
                 (set! t-collect 0) (set! t-check 0)
                 (set! ttt (current-milliseconds)))
           (refine prev-classes id 0)
           )
      
      )

    (define (collect-behaviors x)
      (cond
       [(list? x)  x]
       [(hash? x)
        (let ([ans (list)])
          (for ([val (hash-values x)])
               (set! ans (append (collect-behaviors val) ans)))
          ans)]
       [(vector? x)
        (let ([ans (list)])
          (for ([val x])
               (when val (set! ans (append (collect-behaviors val) ans))))
          ans)]
       [(box? x) (collect-behaviors (box-val x))]
       [else
        (raise (format "collect-behaviors: unimplemented for ~a" x))]

       ))

    (define (get-one-program beh-vec)
      (for/vector ([b beh-vec])
                  (vector-ref (car (vector-ref id2progs b)) 0)))

    (define (get-all-programs beh-vec)
      (define ret (list))
      (define (inner ans rest)
        (if (empty? rest)
            (set! ret (cons ans ret))
            (for ([x (vector-ref id2progs (car rest))])
                 (inner (vector-append ans x) (cdr rest)))))
      (inner (vector) (vector->list beh-vec))
      ret)     
     
    (define/public
      (synthesize-window2 spec sketch prefix postfix constraint extra 
                          [cost #f] [time-limit 3600]
                          #:hard-prefix [hard-prefix (vector)] 
                          #:hard-postfix [hard-postfix (vector)]
                          #:assume-interpret [assume-interpret #t]
                          #:assume [assumption (send machine no-assumption)])

      (define start-time (current-seconds))
      (define spec-precise spec)
      (define prefix-precise prefix)
      (define postfix-precise postfix)
      (set! spec (reduce-precision spec))
      (set! prefix (reduce-precision prefix))
      (set! postfix (reduce-precision postfix))

      (send printer print-syntax (send printer decode spec))
      
      (send machine analyze-opcode prefix spec postfix)
      (send machine analyze-args prefix spec postfix #:vreg 0)
      (define live2 (send validator get-live-in postfix constraint extra))
      (define live2-vec (send machine progstate->vector live2))
      (define live1 (send validator get-live-in spec live2 extra))
      (define live1-list (send machine get-operand-live live1))
      (define live2-list (send machine get-operand-live live2))
             
      (define ntests 3)
      ;; (define inits
      ;;   (send validator generate-input-states ntests (vector-append prefix spec postfix)
      ;;         assumption extra #:db #t))
      ;; p11
      ;; (define inits
      ;;   (list
      ;;    (progstate (vector 4 0) (vector) -1 4)
      ;;    (progstate (vector -8 -4) (vector) -1 4)
      ;;    (progstate (vector -6 4) (vector) -1 4)))
      ;; p24
      (define inits
        (list
         (progstate (vector 5 0) (vector) -1 4)
         (progstate (vector 3 0) (vector) -1 4)
         (progstate (vector -1 0) (vector) -1 4)
         ))
      (define states1 
	(map (lambda (x) (send simulator interpret prefix x #:dep #f)) inits))
      (define states2
	(map (lambda (x) (send simulator interpret spec x #:dep #f)) states1))
      (define states1-vec 
	(map (lambda (x) (send machine progstate->vector x)) states1))
      (define states2-vec 
	(map (lambda (x) (send machine progstate->vector x)) states2))
      (define states1-id (map (lambda (x) (progstate->id x)) states1))
      (define states2-id-spec (map (lambda (x) (progstate->ids x live2)) states2))

      (pretty-display `(states1-vec ,states1-vec))
      (pretty-display `(states2-vec ,states2-vec))
      (pretty-display `(states2-id-spec ,states2-id-spec))
      (pretty-display `(live2-vec ,live2-vec))
      
      (define ce-limit 10000)
      (define ce-in (make-vector ce-limit))
      (define ce-out-vec (make-vector ce-limit))
      (define ce-in-id (make-vector ce-limit))
      (define ce-out-id (make-vector ce-limit))
      (define ce-count ntests)
      (define ce-count-extra ntests)

      (define ce-in-final (list))
      (define ce-out-vec-final (list))

      (for ([i ntests]
            [in states1]
            [out states2-vec]
            [in-id states1-id]
            [out-id states2-id-spec])
           (vector-set! ce-in i in)
           (vector-set! ce-out-vec i out)
           (vector-set! ce-in-id i in-id)
           (vector-set! ce-out-id i out-id)
           )

      (gen-behavior-usable)
      (pretty-display `(gen-behavior-usable ,(- (current-seconds) start-time)))
      (define n-behaviors (vector-length id2progs))

      (define prev-classes (make-hash))
      (class-insert! prev-classes states1-id (vector))
      (define prev-classes-bw (make-vector n-states #f))
      (class-insert-bw! prev-classes-bw states2-id-spec (vector))

      (define classes (make-hash))
      (define classes-bw (make-vector n-states #f))

      (define (check-final p)
        (pretty-display (format "[5] check-final ~a" (length ce-in-final)))
        (send printer print-syntax (send printer decode p))
        (define
          pass
          (for/and ([input ce-in-final]
                    [output-vec ce-out-vec-final])
                   (let ([my-output-vec
                          (send machine progstate->vector
                                (send simulator-precise interpret p input #:dep #f))])
                     (send machine state-eq? output-vec my-output-vec live2-vec))))

        (when
         pass
         (define ce (send validator-precise counterexample 
                          (vector-append prefix-precise spec-precise postfix-precise)
                          (vector-append prefix-precise p postfix-precise)
                          constraint extra #:assume assumption))

         (if ce
             (let* ([ce-input
                     (send simulator-precise interpret prefix-precise ce #:dep #f)]
                    [ce-output
                     (send simulator-precise interpret spec-precise ce-input #:dep #f)]
                    [ce-output-vec
                     (send machine progstate->vector ce-output)])
               (when debug
                     (pretty-display "[6] counterexample (precise)")
                     (send machine display-state ce-input)
                     (pretty-display `(ce-out-vec ,ce-output-vec)))
               (set! ce-in-final (cons ce-input ce-in-final))
               (set! ce-out-vec-final (cons ce-output-vec ce-out-vec-final))
               )
             (begin
               (pretty-display "[7] FOUND!!!")
               (send printer print-syntax (send printer decode p))
               (pretty-display `(ce-count ,ce-count-extra))
               (pretty-display `(ce-count-precise ,(length ce-in-final)))
	       (pretty-display `(time ,(- (current-seconds) start-time)))
               (raise p))))
        )
      
      (define (check-eqv progs progs-bw beh-id beh my-ce-count)
        (define t00 (current-milliseconds))

        (define (inner-progs h)
          (define p (get-one-program h))
          
          ;; (pretty-display "After renaming")
          ;; (send printer print-syntax (send printer decode p))
          (when debug
                (pretty-display "[2] all correct")
                (pretty-display `(ce-count-extra ,ce-count-extra))
                )
          (when (= ce-count-extra ce-limit)
                (raise "Too many counterexamples")
                )
          
          (define ce (send validator counterexample 
                           (vector-append prefix spec postfix)
                           (vector-append prefix p postfix)
                           constraint extra #:assume assumption))

          (if ce
              (let* ([ce-input (send simulator interpret prefix ce #:dep #f)]
                     [ce-output
                      (send simulator interpret spec ce-input #:dep #f)]
                     [ce-input-id (progstate->id ce-input)]
                     [ce-output-id (progstate->ids ce-output live2)]
                     [ce-output-vec
                      (send machine progstate->vector ce-output)])
                (when debug
                      (pretty-display "[3] counterexample")
                      (send machine display-state ce-input)
                      (pretty-display `(ce-out-vec ,ce-output-vec)))
                (vector-set! ce-in ce-count-extra ce-input)
                (vector-set! ce-out-vec ce-count-extra ce-output-vec)
                (vector-set! ce-in-id ce-count-extra ce-input-id)
                (vector-set! ce-out-id ce-count-extra ce-output-id)
                (set! ce-count-extra (add1 ce-count-extra))
                )
              (begin
                (pretty-display "[4] found")
                (send printer print-syntax (send printer decode p))
                (for ([p (get-all-programs h)])
                     (check-final (increase-precision p)))
                )))

          (define (inner-behaviors p1 p2 beh-id beh)
            (define t0 (current-milliseconds))

            (define
              pass
              (for/and ([i (reverse (range my-ce-count ce-count-extra))])
                       (let* ([input (vector-ref ce-in-id i)]
                              [output-ids (vector-ref ce-out-id i)]
                              [inter1 (interpret p1 input)]
                              [inter2 (and inter1 (vector-ref beh inter1))]
                              [my-id (interpret p2 inter2)])
                         (and my-id (member my-id output-ids)))))
            (define t1 (current-milliseconds))
            (set! t-extra (+ t-extra (- t1 t0)))
            (set! c-extra (+ c-extra (- ce-count-extra my-ce-count)))
            (when 
             pass
             (inner-progs (vector-append p1 (vector beh-id) p2))
             ;; (define progs (vector-append p1 (vector beh-id) p2))
             ;; (inner-progs
             ;;  (for/vector ([p progs])
             ;;              (vector-ref (car (vector-ref id2progs p)) 0)))
             (define t2 (current-milliseconds))
             (set! t-verify (+ t-verify (- t2 t1)))

             ))

          (define h1
            (if (= my-ce-count ntests)
                (get-collection-iterator progs)
                progs))

          (define h2
            (if (= my-ce-count ntests)
                (get-collection-iterator progs-bw)
                progs-bw))
             
          (define t11 (current-milliseconds))
          
          (for* ([p1 h1]
                 [p2 h2])
               (inner-behaviors p1 p2 beh-id beh))
          (define t22 (current-milliseconds))
          (set! t-collect (+ t-collect (- t11 t00)))
          (set! t-check (+ t-check (- t22 t11)))
          )

      (define (refine my-classes my-classes-bw beh-id)
        (define t00 (current-milliseconds))
        (define behavior (vector-ref id2beh beh-id))
        (define t11 (current-milliseconds))
        (set! t-load (+ t-load (- t11 t00)))
        
        (define (outer my-classes my-classes-bw level)
          ;;(pretty-display `(outer ,beh-id ,level ,my-classes ,my-classes-bw))
          (define real-hash my-classes)
          (define real-hash-bw my-classes-bw)
          (when (and (list? real-hash) (> (count-collection real-hash) 8))
                ;; list of programs
                (define t0 (current-milliseconds))
                (set! real-hash (make-hash))
                (define input (vector-ref ce-in-id level))
                
                (define (loop iterator)
                  (define prog (and (not (empty? iterator)) (car iterator)))
                  (when 
                   prog
                   (let* ([s0 (current-milliseconds)]
                          [state (interpret prog input)]
                          [s1 (current-milliseconds)])
                     (if (hash-has-key? real-hash state)
                         (hash-set! real-hash state
                                    (cons prog (hash-ref real-hash state)))
                         (hash-set! real-hash state (list prog)))
                     (let ([s2 (current-milliseconds)])
                       (set! t-build-inter (+ t-build-inter (- s1 s0)))
                       (set! t-build-hash (+ t-build-hash (- s2 s1)))
                       (set! c-build-hash (add1 c-build-hash))
                       )
                     )
                   (loop (cdr iterator))
                   ))

                (if (= level ntests)
                    (loop (get-collection-iterator my-classes))
                    (loop my-classes))
                (define t1 (current-milliseconds))
                (set! t-build (+ t-build (- t1 t0)))
                )
          
          (when (and (list? real-hash-bw) (> (count-collection real-hash-bw) 128))
                ;; list of programs
                (define t0 (current-milliseconds))
                (set! real-hash-bw (make-vector n-states #f))
                (define outputs (vector-ref ce-out-id level))
                
                (define (loop-bw iterator)
                  (define prog (and (not (empty? iterator)) (car iterator)))
                  (when 
                   prog
                   (for ([out outputs])
                        (let ([s0 (current-milliseconds)]
                              [states (interpret-bw prog out)]
                              [s1 (current-milliseconds)])
                          (when
                           states
                           (for ([state states])
                                (let ([val (vector-ref real-hash-bw state)])
                                  (if val
                                      (vector-set! real-hash-bw state
                                                   (cons prog val))
                                      (vector-set! real-hash-bw state (list prog))))))
                          
                          (let ([s2 (current-milliseconds)])
                            (set! t-build-inter (+ t-build-inter (- s1 s0)))
                            (set! t-build-hash2 (+ t-build-hash2 (- s2 s1)))
                            (set! c-build-hash2 (add1 c-build-hash2))
                            )
                          ))
                   (loop-bw (cdr iterator))
                   ))
                
                (if (= level ntests)
                    (loop-bw (get-collection-iterator my-classes-bw))
                    (loop-bw my-classes-bw))
                (define t1 (current-milliseconds))
                (set! t-build (+ t-build (- t1 t0)))
                )

          (define (inner)
            (define t0 (current-milliseconds))
            (define inters-fw (hash-keys real-hash))
            (define t1 (current-milliseconds))
            (set! t-intersect (+ t-intersect (- t1 t0)))
            (set! c-intersect (add1 c-intersect))

            (for ([inter inters-fw])
                 (let* ([t0 (current-milliseconds)]
                        [out (vector-ref behavior inter)]
                        [condition (and out (vector-ref real-hash-bw out))]
                        [t1 (current-milliseconds)]
                        )
                   (set! t-interpret (+ t-interpret (- t1 t0)))
                   (set! c-interpret (add1 c-interpret))
                   (when
                    condition
                    (if (= 1 (- ce-count level))
                        (begin
                          (check-eqv (hash-ref real-hash inter)
                                     (vector-ref real-hash-bw out)
                                     beh-id behavior ce-count)
                          (set! ce-count ce-count-extra))
                        (let-values ([(a b)
                                      (outer (hash-ref real-hash inter)
                                             (vector-ref real-hash-bw out)
                                             (add1 level))])
                          (hash-set! real-hash inter a)
                          (vector-set! real-hash-bw out b))))))
            )
            
          (cond
           [(and (hash? real-hash) (vector? real-hash-bw))
            (inner)
            (values real-hash real-hash-bw)]

           [else
            (check-eqv (collect-behaviors real-hash)
                       (list->set (collect-behaviors real-hash-bw))
                       beh-id behavior level)
            (values (cond
                     [(hash? real-hash) real-hash]
                     [(box? real-hash) real-hash]
                     [else (box real-hash)])
                    (cond
                     [(vector? real-hash-bw) real-hash-bw]
                     [(box? real-hash-bw) real-hash-bw]
                     [else (box real-hash-bw)]))
            ]))
       
        (outer my-classes my-classes-bw 0)
        )

      (define (build-hash beh-id)
        (define behavior (vector-ref id2beh beh-id))
         (define (recurse x states2-id)
           (if (list? x)
               (class-insert! classes (reverse states2-id) (concat x beh-id))
               (for ([pair (hash->list x)])
                    (let* ([state-id (car pair)]
                           [val (cdr pair)]
                           [out (vector-ref behavior state-id)])
                      (when out (recurse val (cons out states2-id)))))))
         
         (recurse prev-classes (list)))

      (define (build-hash-bw beh-id)
        (define behavior-bw (vector-ref id2bw beh-id))
        (define (recurse x states-id-accum)
          (if (list? x)
              (class-insert-bw! classes-bw (reverse states-id-accum) (concat x beh-id))
              (for ([state-id (in-naturals)]
                    [val x])
                   (when
                    val
                    (let ([out (vector-ref behavior-bw state-id)])
                      (when out (recurse val (cons out states-id-accum))))))))
         
        (recurse prev-classes-bw (list)))
      
      ;; Grow forward
      (for ([i 2])
        (newline)
        (pretty-display `(grow ,i))
        (set! classes (make-hash))
        (for ([beh-id n-behaviors])
             (build-hash beh-id))
        (set! prev-classes classes)
        (pretty-display `(behavior ,i ,c-behaviors ,c-progs ,(- (current-seconds) start-time)))
        (set! c-behaviors 0)
        (set! c-progs 0)
        )

      ;; Grow backward
      (for ([i 1])
           (set! classes-bw (make-vector n-states #f))
           (for ([beh-id n-behaviors])
		(pretty-display (format "bw ~a/~a" beh-id n-behaviors))
                (build-hash-bw beh-id))
           (set! prev-classes-bw classes-bw)
           (pretty-display `(behavior-bw ,i ,c-behaviors-bw ,c-progs-bw ,(- (current-seconds) start-time)))
           (set! c-behaviors-bw 0)
           (set! c-progs-bw 0)
        )
      (pretty-display `(grow done))

      ;; Search
      (define ttt (current-milliseconds))
      (for ([id (range n-behaviors)])
           ;;(when (= 0 (modulo id 10))
                 (pretty-display (format "search ~a/~a | ~a = ~a\t(~a + ~a/~a + ~a/~a)\t~a/~a\t~a/~a\t~a/~a ~a" 
                                         id n-behaviors
                                         (- (current-milliseconds) ttt)
                                         t-build t-build-inter t-build-hash c-build-hash t-build-hash2 c-build-hash2
                                         t-intersect c-intersect
                                         t-interpret c-interpret
                                         t-extra c-extra
                                         t-verify
                                         ))
                 (set! t-build 0) (set! t-build-inter 0) (set! t-build-hash 0) (set! t-build-hash2 0) (set! t-intersect 0) (set! t-interpret 0) (set! t-extra 0) (set! t-verify 0)
                 (set! c-build-hash 0) (set! c-build-hash2 0) (set! c-intersect 0) (set! c-interpret 0) (set! c-extra 0)
                 (set! t-collect 0) (set! t-check 0)
                 (set! ttt (current-milliseconds))
                 ;;)
           (refine prev-classes prev-classes-bw id)
           )

      
      )

    ))

        
        
