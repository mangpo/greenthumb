#lang racket

(provide graph% (struct-out vertex) (struct-out neighbor))
(require racket/generator)

(struct vertex (ids from to status children cost) #:mutable)
;; children is a hash table that maps state -> child node (refinement)
(struct neighbor (node edge))

(define graph%
  (class object%
    (super-new)
    (init-field machine validator simulator printer parser 
                spec constraint extra assumption 
                start-ids [dest-ids #t])
    (public get-correct-iterator)

    (define liveout-vec (send machine progstate->vector constraint))
    (define visit (set))
    ;; (define ce-n 1)
    ;; (define ce-input (make-vector 16 #f))
    (define ce-output (make-vector 16 #f))
    (define best-cost 1000)

    (define (get-correct-iterator my-node)
      ;; (define in (car (send validator generate-input-states 1 spec assumption extra)))
      ;; (define out (send simulator interpret spec init #:dep #f))
      ;; (vector-set! ce-input 0 (send machine progstate->vector in))
      ;; (vector-set! ce-output 0 (send machine progstate->vector out))
      
      (generator () (dfs my-node 0 0 (list)) #f))
    
    ;; Return #t if that node has ce.
    (define (dfs my-node cost level path)
      (pretty-display `(dfs ,(vertex-ids my-node) ,cost ,level))
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
        (pretty-display (format "Graph: level = ~a, ce = ~a" level ce))
        (if ce
            (begin
              (vector-set! ce-output level 
                           (send machine progstate->vector
                                 (send simulator interpret spec ce #:dep #f)))
	      (add-forward-edges my-node path)
              (exec-forward my-node ce #f #f level) ;; Same level DFS
              (set! visit (set-add visit my-node))
              #t
              )
            (begin
              (yield prog)
              (when (< cost best-cost) (set! best-cost cost))
              #f))]

       [else
        (define my-children-table (vertex-children my-node))
        (define ce #t)
        (for ([edge (reverse (vertex-from my-node))])
             ;; Reverse the list because the small programs are usually added to
             ;; the list first.
             (let* ([node-prev (neighbor-node edge)]
                    [raw-p-prev (neighbor-edge edge)]
                    [p-prev 
                     (if (= level 0) ;; haven't converted.
                         (send printer encode (send parser ast-from-string raw-p-prev))
                         raw-p-prev)]
                    [this-cost (send simulator performance-cost p-prev)]
                    [children-table (vertex-children node-prev)]
                    [self-loop (equal? my-node node-prev)])
               (pretty-display (format "  ~a" (vertex-ids my-node)))
               (pretty-display (format "  --> ~a [~a]" (vertex-ids node-prev) self-loop))

	       ;; If this path merges with the graph with ce, add forward egdes.
	       (unless (hash-empty? children-table)
		       (set-vertex-to! node-prev 
				       (cons (neighbor my-node p-prev) (vertex-to node-prev)))
		       (add-forward-edges my-node path))
			  
               (for ([pair (hash->list children-table)])
                    (let* ([c-state-vec (car pair)]
                           [c-state (send machine vector->progstate c-state-vec)]
                           [c-node (cdr pair)]
                           [my-state (send simulator interpret p-prev c-state #:dep #f)]
                           [my-state-vec (send machine progstate->vector my-state)])
		      (pretty-display (format "  [DFS found ce]"))
                      (exec-forward my-node my-state c-node p-prev level)
                      ;; (set! self-loop (and self-loop (equal? c-state-vec my-state-vec)))
                      ))
               (when (and (not (set-member? visit node-prev)) 
                          (not self-loop)
                          (<= (+ cost this-cost) best-cost))
                     ;; Same level DFS
                     (let ([ret (dfs node-prev
                                     (+ cost this-cost) level 
				     (cons (neighbor my-node p-prev) path))])
                       (set! ce (and ce ret))))
               ))
        (when ce (set! visit (set-add visit my-node)))
        ce
        ]))

    (define (add-forward-edges this-node path)
      (pretty-display (format "  (add-forward-edges ~a)" (vertex-ids this-node)))
      (when (and (hash-empty? (vertex-children this-node)) (>= (length path) 1))
	    (let* ([step (car path)]
		   [node-next (neighbor-node step)])
	      (set-vertex-to! this-node (cons step (vertex-to this-node)))
	      (add-forward-edges node-next (cdr path)))))
    

    ;; (define (verify-path my-node path level)
    ;;   (if (> ec-n (add1 level))
    ;; 	  (dfs my-node ?? ?? (add1 level))
    ;; 	  (let* ([prog (vector-append path (vertex-status my-node))]
    ;; 		 [ce (send validator counterexample spec prog constraint extra 
    ;; 			   #:assume assumption)])
    ;; 	    (set! level (add1 level))
    ;; 	    (pretty-display (format "Graph: level = ~a, ce = ~a" level ce))
    ;; 	    (if ce
    ;; 		(begin
    ;; 		  (vector-set! ce-output level 
    ;; 			       (send machine progstate->vector
    ;; 				     (send simulator interpret spec ce #:dep #f)))
    ;; 		  (dfs my-
		
	    

    (define (exec-forward my-node my-state c-prev-node p-prev level)
      (pretty-display (format "   (exec ~a ~a)" (vertex-ids my-node) level))
      (send machine display-state my-state)
      (define my-children-table (vertex-children my-node))
      (define my-state-vec (send machine progstate->vector my-state))
      (define ret-path #f)
      (if (hash-has-key? my-children-table my-state-vec)
          (let ([matched-node (hash-ref my-children-table my-state-vec)])
	    (pretty-display (format "   (hash-has-key)"))
	    (when c-prev-node ;; If not source node.
		  (set-vertex-from! matched-node
				    (cons (neighbor c-prev-node p-prev) 
					  (vertex-from matched-node))))
            (when (vertex-status matched-node) ;; correct candidate
                ;; Correct -> DFS
                (let ([path (vertex-status matched-node)])
                  ;; +1 level DFS
                  (dfs matched-node (vertex-cost matched-node) (add1 level) path)
                  (set! ret-path path)
                  )
                ))
          (let ([new-node (vertex (vertex-ids my-node)
                                  (if c-prev-node
                                      (list (neighbor c-prev-node p-prev))
                                      (list))
                                  (list) #f (make-hash) 0)])
            (hash-set! my-children-table my-state-vec new-node)

	    (cond
	     [(equal? (vertex-ids my-node) dest-ids)
	      ;; Rearch destination
	      (when (send machine state-eq? 
			  (vector-ref ce-output level) 
			  my-state-vec 
			  liveout-vec)
		    (set-vertex-status! new-node (list))
		    ;; Correct -> DFS
		    (dfs new-node 0 (add1 level) (list)) ;; +1 level DFS
		    (set! ret-path (list)))
	     ]
	     
	     [else
	      (define min-cost 100000)
	      (for ([edge (vertex-to my-node)])
		   (pretty-display (format "   [Push ~a]" (equal? (neighbor-node edge) my-node)))
		   (let* ([node-next (neighbor-node edge)]
			  [p-next (neighbor-edge edge)]
			  [state-next (send simulator interpret p-next my-state #:dep #f)]
			  ;; Call exec-forward
			  [ret (exec-forward node-next state-next new-node p-next level)]
			  [perf (send simulator performance-cost p-next)]
			  )
		     ;; TODO: check
		     (when ret
			   (let ([my-cost (if (> (length ret) 0)
					      (+ perf (vertex-cost (neighbor-node (car ret))))
					      perf)]
				 [step (neighbor new-node p-next)])
			     (when (< my-cost min-cost)
				   (set! min-cost my-cost)
				   (set! ret-path (cons step ret))
				   (set-vertex-cost! new-node min-cost)
				   (set-vertex-status! new-node ret-path)
				   )))
		     ))
	      ])))
      ret-path
      )
    
    ))
