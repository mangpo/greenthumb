#lang racket

(require "arm/arm-machine.rkt")
(provide forwardbackward%)

(struct concat (collection inst))
(struct box (val))

(define forwardbackward%
  (class object%
    (super-new)
    (init-field machine enum simulator simulator-precise
                printer parser
                validator validator-precise
		inverse)
    (abstract vector->id)
    (public synthesize-window)

    (define c-behaviors-bw 0)
    (define c-progs-bw 0)
    (define (class-insert-bw! class live states-vec prog)
      (pretty-display `(class-insert-bw! ,(map length states-vec)))
      (set! c-progs-bw (add1 c-progs-bw))

      (define (insert-inner x states-vec prog)
        (define key-list (car states-vec))
        (if (= (length states-vec) 1)
	    (for ([key key-list])
		 (if (hash-has-key? x key)
		     (hash-set! x key (cons prog (hash-ref x key)))
		     (begin
		       (set! c-behaviors-bw (add1 c-behaviors-bw))
		       (hash-set! x key (list prog)))))
	    (for ([key key-list])
		 (let ([has-key (hash-has-key? x key)])
		   (unless has-key (hash-set! x key (make-hash)))
		   (insert-inner (hash-ref x key) (cdr states-vec) prog)))))

      ;(set! states-vec (map (lambda (x) (abstract x live-list identity)) states-vec))
      (define key live)
      (unless (hash-has-key? class key) (hash-set! class key (make-hash)))
      (insert-inner (hash-ref class key) states-vec prog))

    (define (synthesize-window spec sketch prefix postfix constraint extra 
			       [cost #f] [time-limit 3600]
			       #:hard-prefix [hard-prefix (vector)] 
			       #:hard-postfix [hard-postfix (vector)]
			       #:assume-interpret [assume-interpret #t]
			       #:assume [assumption (send machine no-assumption)])

      (define start-time (current-seconds))

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
      ;; p24
      (define inits
        (list
         (progstate (vector -6 -5 3 5) (vector) -1 4)
         (progstate (vector -5 2 -5 0) (vector) -1 4)
         (progstate (vector 6 3 4 5) (vector) -1 4)
         ))
      (define states1 
	(map (lambda (x) (send simulator interpret prefix x #:dep #f)) inits))
      (define states2
	(map (lambda (x) (send simulator interpret spec x #:dep #f)) states1))
      (define states1-vec 
	(map (lambda (x) (send machine progstate->vector x)) states1))
      (define states2-vec 
	(map (lambda (x) (send machine progstate->vector x)) states2))
      (define states2-vec-list (map list states2-vec))

      (pretty-display `(states1-vec ,states1-vec))
      (pretty-display `(states2-vec ,states2-vec))
      (pretty-display `(live2-vec ,live2-vec))


      (define prev-classes-bw (make-hash))
      (class-insert-bw! prev-classes-bw live2-list states2-vec-list (vector))
      (define classes-bw #f)
      
      (define (gen-inverse-behaviors iterator)
        (define p (iterator))
        (define my-inst (car p))
        (when my-inst
          ;(send printer print-syntax (send printer decode my-inst))
          (send inverse gen-inverse-behavior my-inst)
          (gen-inverse-behaviors iterator)
          ))
      
      (gen-inverse-behaviors (send enum reset-generate-inst #f #f #f #f `all #f #:no-args #t))

      (define (build-hash-bw old-liveout my-hash iterator)
        ;(define inst-liveout-vreg (iterator))
	;(define my-inst (first inst-liveout-vreg))
	;(define my-liveout (third inst-liveout-vreg))
        (define my-inst 
          (vector-ref (send printer encode 
                            (send parser ast-from-string "add r1, r1, r2, asr r3"))
                      0))
	(define my-liveout (list 0 1))

	(when my-inst
          (define t (current-milliseconds))
          (send printer print-syntax-inst (send printer decode-inst my-inst))
          (define (recurse x states-vec-accum)
            (if (list? x)
                (begin
                  (pretty-display `(check-1 ,(- (current-milliseconds) t)))
                  (class-insert-bw! classes-bw my-liveout (reverse states-vec-accum) 
                                    (concat x my-inst))
                  (pretty-display `(check-2 ,(- (current-milliseconds) t)))
                  )
                (for ([pair (hash->list x)])
                  (let* ([state-vec (car pair)]
                         [val (cdr pair)]
                         [out (send inverse interpret-inst my-inst state-vec old-liveout)])
                    (when (and out (not (empty? out)))
                      (recurse val (cons out states-vec-accum)))))))
          
          (recurse my-hash (list))
          ;(build-hash-bw old-liveout my-hash iterator)
          )
	)

      ;; Grow backward
      (for ([i 1])
           (set! c-behaviors-bw 0)
           (set! c-progs-bw 0)
           (set! classes-bw (make-hash))
	   (for ([pair (hash->list prev-classes-bw)])
		(let* ([live-list (car pair)]
                       [my-hash (cdr pair)]
                       [iterator (send enum reset-generate-inst #f #f live-list #f `all #f)])
                  (newline)
                  (pretty-display `(live ,live-list))
		  (build-hash-bw live-list my-hash iterator)))
           (set! prev-classes-bw classes-bw)
           (pretty-display `(behavior-bw ,i ,c-behaviors-bw ,c-progs-bw ,(- (current-seconds) start-time)))
        )
      )
    ))
