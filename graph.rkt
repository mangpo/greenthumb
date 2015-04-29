#lang racket

(provide graph% make-vertex (struct-out vertex) (struct-out neighbor))
(require racket/generator)

(struct vertex (ids from to status children cost-from cost-to) #:mutable)
;; children is a hash table that maps state -> child node (refinement)
(struct neighbor (node edge))

(define-syntax make-vertex
  (syntax-rules ()
    ((make-vertex ids) (vertex ids (list) (list) #f (make-hash) #f #f))
    ((make-vertex ids f) (vertex ids f (list) #f (make-hash) #f #f))
    ))
    

(define graph%
  (class object%
    (super-new)
    (init-field machine validator simulator printer  
                spec constraint extra assumption 
                start-ids [dest-ids #t])
    (public get-correct-iterator)

    (define debug #f)
    (define liveout-vec (send machine progstate->vector constraint))
    (define visit (set))
    ;; (define ce-n 1)
    ;; (define ce-input (make-vector 16 #f))
    (define ce-output (make-vector 16 #f))
    (define best-cost 4)

    (define (get-correct-iterator my-node [edge #f])
      (if edge
	  (generator () (dfs-edge my-node 1 0 (list) edge) #f)
	  (generator () (dfs my-node 0 0 (list)) #f)
       )
      )
    
    ;; Return #t if that node has ce.
    (define (dfs my-node cost level path)
      (when debug 
            (pretty-display (format "[dfs] start ~a cost=~a level=~a" 
                                    (vertex-ids my-node) cost level)))
      (cond
       [(equal? (vertex-ids my-node) start-ids)
        (when (vector-ref ce-output level)
              (raise (format "graph% already has counterexample for level ~a" level)))
	
	(define prog (vector))
	(for ([x path])
	     (set! prog (vector-append prog (neighbor-edge x))))

	(send printer print-syntax (send printer decode prog))
        (define ce (send validator counterexample spec prog constraint extra 
                         #:assume assumption))
        (pretty-display (format "[dfs] level=~a ce=~a" level ce))
        (if ce
            (begin
              (vector-set! ce-output level 
                           (send machine progstate->vector
                                 (send simulator interpret spec ce #:dep #f)))
	      (add-forward-edges my-node path)
              (exec-forward my-node ce 0 #f #f level) ;; Same level DFS
	      (pretty-display `(children-table ,(vertex-children my-node)))
              (set! visit (set-add visit my-node))
              )
            (begin
              (yield prog)
              (when (< cost best-cost) (set! best-cost cost))))]

       [else
        (define-syntax-rule (func edge) 
	  (dfs-edge my-node cost level path edge))
          
        ;; Reverse the list because the small programs are usually added to
        ;; the list first.

        ;; Visit not self-loop first.
        (for ([edge (reverse (vertex-from my-node))])
             (let* ([node-prev (neighbor-node edge)]
                    [self-loop (equal? my-node node-prev)])
               (when (not self-loop) (func edge))))

        ;; Visit self-loop.
        (for ([edge (reverse (vertex-from my-node))])
             (let* ([node-prev (neighbor-node edge)]
                    [self-loop (equal? my-node node-prev)])
               (when self-loop (func edge))))
        ]))

    
    ;; Return #t if that node has ce.
    (define (dfs-edge my-node cost level path edge)
	(let* ([node-prev (neighbor-node edge)]
	       [self-loop (equal? my-node node-prev)]
	       [p-prev (neighbor-edge edge)]
	       [this-cost (send simulator performance-cost p-prev)]
	       [total-cost (+ cost this-cost)]
	       [children-table (vertex-children node-prev)])
          (when debug
                (pretty-display (format "  level=~a self-loop=~a"
                                        level self-loop))
                (pretty-display (format "  ~a" (vertex-ids my-node)))
                (pretty-display (format "  --> ~a empty=~a cost=~a" 
                                        (vertex-ids node-prev) 
                                        (hash-empty? children-table)
                                        total-cost)))

	  (when (<= total-cost best-cost)
		(if (hash-empty? children-table)
		    (unless self-loop
			    (dfs node-prev total-cost level 
				 (cons (neighbor my-node p-prev) path)))
		    (connect-graph my-node node-prev p-prev children-table path 
				   total-cost level)))
	  ))

    (define (connect-graph my-node node-prev p-prev children-table path total-cost level)
      (when debug
            (pretty-display (format "[connect] ~a level=~a" (vertex-ids my-node) level)))
      ;; If this path merges with the graph with ce, add forward egdes.
      (set-vertex-to! node-prev 
                      (cons (neighbor my-node p-prev) (vertex-to node-prev)))
      (add-forward-edges my-node path)
      
      (for ([pair (hash->list children-table)])
           (let* ([c-state-vec (car pair)]
                  [c-state (send machine vector->progstate c-state-vec)]
                  [c-node (cdr pair)]
                  [c-cost (vertex-cost-from c-node)]
                  [my-state (send simulator interpret p-prev c-state #:dep #f)]
                  [my-state-vec (send machine progstate->vector my-state)]
                  [perf (send simulator performance-cost p-prev)])
             (when
              (<= (+ c-cost perf) best-cost)
              (let-values ([(new-path new-node) 
                            (exec-forward my-node my-state (+ c-cost perf) 
                                          c-node p-prev level)])
                (when new-path
                      (let ([c-children-table (vertex-children c-node)])
                        (if (hash-empty? c-children-table)
                            (when (<= total-cost best-cost)
                                  (dfs c-node total-cost (add1 level) 
                                       (cons (neighbor new-node p-prev) new-path)))
                            (connect-graph new-node c-node p-prev c-children-table new-path 
                                           total-cost (add1 level)))))
                )))))
  

    (define (add-forward-edges this-node path)
      (when (and (hash-empty? (vertex-children this-node)) (>= (length path) 1))
	    (let* ([step (car path)]
		   [node-next (neighbor-node step)])
	      (set-vertex-to! this-node (cons step (vertex-to this-node)))
	      (add-forward-edges node-next (cdr path)))))
	    
    ;; Return path if find correct paths.
    ;; TODO: ~A* on cost
    (define (exec-forward my-node my-state my-cost c-prev-node p-prev level)
      (when debug 
            (pretty-display "[exec]")
            (send machine display-state my-state))
      (define my-children-table (vertex-children my-node))
      (define my-state-vec (send machine progstate->vector my-state))
      (define ret-path #f)
      (if (hash-has-key? my-children-table my-state-vec)
          (let ([matched-node (hash-ref my-children-table my-state-vec)])
            (when (< my-cost (vertex-cost-from matched-node)) ;; Update cost.
                  ;;(raise "graph: vertex-cost-from decreases!!!")
                  ;; TODO: fix this!!!
                  (set-vertex-cost-from! matched-node my-cost))
	    (when c-prev-node ;; If not source node.
		  (set-vertex-from! matched-node
				    (cons (neighbor c-prev-node p-prev) 
					  (vertex-from matched-node))))
            (if (vertex-status matched-node) ;; correct candidate
                ;; Correct -> DFS
                (let ([path (vertex-status matched-node)])
                  (set! ret-path path)
                  (when debug
                        (pretty-display (format "[exec] has-correct level=~a ~a"  
                                                level (vertex-ids my-node))))
                  )
                (when debug
                      (pretty-display (format "[exec] has-wrong level=~a ~a" 
                                              level (vertex-ids my-node))))
                )
            
            (values ret-path matched-node)
            )
          (let ([new-node (vertex (vertex-ids my-node)
                                  (if c-prev-node
                                      (list (neighbor c-prev-node p-prev))
                                      (list))
                                  (list) #f (make-hash) my-cost 0)
                                  ])
            (hash-set! my-children-table my-state-vec new-node)

	    (cond
	     [(equal? (vertex-ids my-node) dest-ids)
	      ;; Rearch destination
	      (when (send machine state-eq? 
			  (vector-ref ce-output level) 
			  my-state-vec 
			  liveout-vec)
		    (set-vertex-status! new-node (list))
		    (set! ret-path (list)))
	     ]
	     
	     [else
	      (define min-cost 100000)
	      (for ([edge (vertex-to my-node)])
                   (when
                    debug
                    (pretty-display (format "[exec] push self-loop=~a level=~a ~a" 
                                            (equal? (neighbor-node edge) my-node)
                                            level (vertex-ids my-node)
                                            )))
                   (when 
                    (<= my-cost best-cost)
                    (let* ([node-next (neighbor-node edge)]
                           [p-next (neighbor-edge edge)]
                           [state-next (send simulator interpret p-next my-state #:dep #f)]
                           [perf (send simulator performance-cost p-next)])
                      
                      ;; Call exec-forward
                      (let-values ([(ret dummy) 
                                    (exec-forward node-next state-next (+ my-cost perf)
                                                  new-node p-next level)])
                        ;; TODO: check
                        (when ret
                              (let ([my-cost-back
                                     (if (> (length ret) 0)
                                         (+ perf (vertex-cost-to 
                                                  (neighbor-node (car ret))))
                                         perf)]
                                    [step (neighbor dummy p-next)])
                                (when (< my-cost-back min-cost)
                                      (set! min-cost my-cost-back)
                                      (set! ret-path (cons step ret))
                                      (set-vertex-cost-to! new-node min-cost)
                                      (set-vertex-status! new-node ret-path)
                                      )))))
                    )
                   )
	      ])
            (values ret-path new-node)
            )
          ) ;; end if
      )
    
    ))
