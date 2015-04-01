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
    (define ce-output (make-vector 16 #f))
    (define best-cost 1000)

    (define (get-correct-iterator my-node)
      (generator () (dfs my-node (vector) 0 0) #f))
    
    ;; Return #t if that node has ce.
    (define (dfs my-node path cost level)
      (pretty-display `(dfs ,(vertex-ids my-node) ,cost ,level))
      (cond
       [(equal? (vertex-ids my-node) start-ids)
        (when (vector-ref ce-output level)
              (raise (format "graph% already has counterexample for level ~a" level)))
        (define ce (send validator counterexample spec path constraint extra 
                         #:assume assumption))
        (pretty-display (format "Graph: level = ~a, ce = ~a" level ce))
        (if ce
            (begin
              (vector-set! ce-output level 
                           (send machine progstate->vector
                                 (send simulator interpret spec ce #:dep #f)))
              (exec-forward my-node ce #f #f level) ;; Same level DFS
              (set! visit (set-add visit my-node))
              #t
              )
            (begin
              (yield path)
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
               (set-vertex-to! my-node (cons (neighbor node-prev p-prev) (vertex-to my-node))) 
               ;; TODO: set-node-to! here or later?
               (for ([pair (hash->list children-table)])
                    (let* ([c-state-vec (car pair)]
                           [c-state (send machine vector->progstate c-state-vec)]
                           [c-node (cdr pair)]
                           [my-state (send simulator interpret p-prev c-state #:dep #f)]
                           [my-state-vec (send machine progstate->vector my-state)])
                      (exec-forward my-node my-state c-node p-prev level)
                      (set! self-loop (and self-loop (equal? c-state-vec my-state-vec)))
                      ))
               (pretty-display (format "   ~a" (vertex-ids my-node)))
               (pretty-display (format "   --> ~a" (vertex-ids node-prev)))
               (when (and (not (set-member? visit node-prev)) 
                          (not self-loop)
                          (<= (+ cost this-cost) best-cost))
                     ;; Same level DFS
                     (let ([ret (dfs node-prev (vector-append p-prev path) 
                                     (+ cost this-cost) level)])
                       (set! ce (and ce ret))))
               ))
        (when ce (set! visit (set-add visit my-node)))
        ce
        ]))

    (define (exec-forward my-node my-state c-prev-node p-prev level)
      (define my-children-table (vertex-children my-node))
      (define my-state-vec (send machine progstate->vector my-state))
      (define ret-path #f)
      (if (hash-has-key? my-children-table my-state-vec)
          (let ([matched-node (hash-ref my-children-table my-state-vec)])
            (set-vertex-from! matched-node
                            (cons (neighbor c-prev-node p-prev) 
                                  (vertex-from matched-node)))
            (if (vertex-status matched-node)
                ;; Correct -> DFS
                (let ([path (vertex-status matched-node)])
                  ;; +1 level DFS
                  (dfs matched-node path (vertex-cost matched-node) (add1 level)) 
                  (set! ret-path path)
                  ;; Update cost for c-prev-node.
                  ;; (let ([perf (send simulator performance-cost p-prev)])
                  ;;   (set-vertex-cost! c-prev-node (+ (vertex-cost matched-node) perf)))
                  )
                (set-vertex-to! c-prev-node (cons (neighbor matched-node p-prev)
                                                (vertex-to c-prev-node))) ;; add to edge
                ))
          (let ([new-node (vertex (vertex-ids my-node)
                                  (if c-prev-node
                                      (list (neighbor c-prev-node p-prev))
                                      (list))
                                  (list) #f (make-hash) my-node)]
                )
            ;; Rearch destination
            (when (and (equal? (vertex-ids my-node) dest-ids)
                       (send machine state-eq? 
                             (vector-ref ce-output level) 
                             my-state-vec 
                             liveout-vec))
                  ;; Correct -> DFS
                  ;; Set cost for new-node.
                  (set-vertex-cost! new-node 0)
                  (set-vertex-status! new-node (vector))
                  (dfs new-node (vector) 0 (add1 level)) ;; +1 level DFS
                  (set! ret-path (vector))
                  )

            (hash-set! my-children-table my-state-vec new-node)
            (for ([edge (vertex-to my-node)])
                 (let* ([node-next (neighbor-node edge)]
                        [p-next (neighbor-edge edge)]
                        [state-next (send simulator interpret p-next my-state #:dep #f)]
                        ;; Call exec-forward
                        [ret (exec-forward node-next state-next new-node p-next level)])
                   (when (and ret (not ret-path))
                         ;; Update cost for c-prev-node.
                         (let ([perf (send simulator performance-cost p-prev)])
                           (set-vertex-cost! c-prev-node (+ (vertex-cost new-node) perf)))
                         (set-vertex-status! new-node (vector-append p-next ret))
                         (set! ret-path (vector-append p-next ret)))
                   ))
            
            ))
      ret-path
      )
             
    ))