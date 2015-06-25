#lang racket

(require "../ops-racket.rkt" "../ast.rkt"
         "arm-ast.rkt" "arm-machine.rkt" "arm-simulator-racket.rkt"
         "arm-printer.rkt" "arm-parser.rkt")

(provide arm-inverse%)

(define arm-inverse%
  (class object%
    (super-new)
    (init-field machine simulator)
    (public gen-inverse-behavior interpret-inst)

    (define bit (get-field bit machine))
    (define inst-id (get-field inst-id machine))
    (define shf-inst-id (get-field shf-inst-id machine))
    (define reg-range-db
      (for/vector ([v (arithmetic-shift 1 bit)]) (finitize v bit)))
    (define fp (send machine get-fp))
    
    (define (get-inst-in-out x)
      (define opcode (inst-op x))
      (define args (inst-args x))
      (define shfop (inst-shfop x))
      (define shfarg (inst-shfarg x))
      (unless shfop (set! shfop 0))

      (define inst-type (list shfop opcode))
      (define in (list))
      (define out (list))
      (when (> shfop 0)
	    (if (member (vector-ref shf-inst-id shfop) '(asr lsl lsr))
		(set! in (cons shfarg in))
		(set! inst-type (cons shfarg inst-type))))
      
      (for ([arg args]
	    [type (send machine get-arg-types (vector-ref inst-id opcode))])
	   (cond
	    [(equal? type `reg-o) (set! out (cons arg out))]
	    [(equal? type `reg-i) (set! in (cons arg in))]
	    [(equal? type `reg-io) (set! in (cons arg in)) (set! out (cons arg out))]
	    [else (set! inst-type (cons arg inst-type))]))

      (values (reverse inst-type) (reverse in) (reverse out)))
    
    (define behaviors-bw (make-hash))
    (define (gen-inverse-behavior my-inst)
      (define opcode-name (vector-ref inst-id (inst-op my-inst)))
      (define in (make-vector 5 #f))
      (define out (make-vector 5 #f))
      (define behavior-bw (make-hash))
      
      (define (recurse-regs in-list in-res)
	(cond
	 [(empty? in-list)
          (define out-state
	    (with-handlers*
	     ([exn? (lambda (e) #f)])
	     (send simulator interpret
		   (vector my-inst)
		   (progstate (list->vector in-res) (vector) -1 fp) #:dep #f)))
          
	  (when 
	   out-state
	   (define in-list-filtered (filter number? in-res))
	   (define out-list (list))
	   (for ([r (progstate-regs out-state)]
		 [m out])
		(when m (set! out-list (cons r out-list))))
	   
	   (define key (reverse out-list))
	   ;;(pretty-display `(inout ,in-res ,(progstate-regs out-state) ,in-list-filtered ,key))
	   (if (hash-has-key? behavior-bw key)
	       (hash-set! behavior-bw key
			  (cons in-list-filtered (hash-ref behavior-bw key)))
	       (hash-set! behavior-bw key (list in-list-filtered))))
	  ]

         [else
          (if (car in-list)
	      (for ([i reg-range-db])
		   (recurse-regs (cdr in-list) (cons i in-res)))
	      (recurse-regs (cdr in-list) (cons #f in-res)))]))
          
      (define arg-types (send machine get-arg-types opcode-name))
      (define shfop (inst-shfop my-inst))
      (define shfarg (inst-shfarg my-inst))
      (when (and shfop (member (vector-ref shf-inst-id shfop) '(asr lsl lsr)))
            (vector-set! in shfarg #t))
      
      (for ([arg (inst-args my-inst)]
	    [type arg-types])
	   (cond
	    [(equal? type `reg-o) (vector-set! out arg #t)]
	    [(equal? type `reg-i) (vector-set! in arg #t)]
	    [(equal? type `reg-io)
             (vector-set! out arg #t) (vector-set! in arg #t)]))

      (recurse-regs (reverse (vector->list in)) (list))
      
      (define-values (x regs-in regs-out) (get-inst-in-out my-inst))
      ;;(pretty-display `(behavior-bw ,behavior-bw))
      (hash-set! behaviors-bw x behavior-bw))

    (define (interpret-inst my-inst state-vec old-liveout)
      ;;(pretty-display `(interpret ,state-vec ,old-liveout))
      (define opcode-name (vector-ref inst-id (inst-op my-inst)))
      (define cond-type (arm-inst-cond my-inst))

      (define regs (vector-ref state-vec 0))
      (define mem (vector-ref state-vec 1))
      (define z (vector-ref state-vec 2))


      (define (exec)
	(define-values (x regs-in regs-out) (get-inst-in-out my-inst))
	(define regs-base (make-vector (vector-length regs) #f))
	(for ([i old-liveout])
	     (unless (member i regs-out) (vector-set! regs-base i (vector-ref regs i))))
	(define regs-out-val 
	  (for/list ([r regs-out]) (vector-ref regs r)))

	(define mapping (hash-ref behaviors-bw x))
        (define ret (and (hash-has-key? mapping regs-out-val) (list)))

	(when ret
	      (define regs-in-val-list (hash-ref mapping regs-out-val))
              ;;(pretty-display `(ref ,regs-out-val ,regs-in-val-list))
	      (for ([regs-in-val regs-in-val-list])
		   (let ([new-regs (vector-copy regs-base)]
			 [pass #t])
		     (for ([r regs-in]
			   [v regs-in-val] #:break (not pass))
			  (cond
			   [(vector-ref new-regs r)
			    (unless (= v (vector-ref new-regs r))
                              
                              (set! pass #f))]
			   [else (vector-set! new-regs r v)]))
		     (when pass (set! ret (cons new-regs ret))))))
	
        ret)
     
      (define (same) (list (vector (vector-copy regs) (vector-copy mem) -1 fp)))
      (define (convert x) (and x (for/list ([i x]) (vector i (vector-copy mem) -1 fp))))
      ;; TODO: z != -1

      (cond
       ;; [(member opcode-name '(tst cmp tst# cmp#))
       ;;  (convert (exec))]

       [(or (equal? cond-type 0) (equal? z -1))
	(convert (exec))]

       [(equal? cond-type 1) ;; eq
	(if (equal? z 0) (convert (exec)) (same))]

       [(equal? cond-type 2) ;; ne
	(if (member z (list 1 2 3)) (convert (exec)) (same))]

       [(equal? cond-type 3) ;; ls
	(if (member z (list 0 2)) (convert (exec)) (same))]

       [(equal? cond-type 4) ;; hi
	(if (equal? z 3) (convert (exec)) (same))]

       [(equal? cond-type 5) ;; cc
	(if (equal? z 2) (convert (exec)) (same))]

       [(equal? cond-type 6) ;; cs
	(if (member z (list 0 3)) (convert (exec)) (same))]
       
       [else (raise (format "illegal cond-type ~a" cond-type))]
       ))
      

    ))

#|
(define machine (new arm-machine% [bit 4]))
(define simulator (new arm-simulator-racket% [machine machine]))

(define inverse (new arm-inverse% [machine machine] [simulator simulator]))
(send machine set-config (list 4 0 4))
(define printer (new arm-printer% [machine machine]))
(define parser (new arm-parser%))
(define my-inst-0
  (vector-ref (send printer encode 
                    (send parser ast-from-string "eor r3, r1, r2, lsl r0"))
              0))

(define my-inst 
  (vector-ref (send printer encode 
                    (send parser ast-from-string "eor r0, r0, r1, lsl r2"))
              0))

(define input-state (vector (vector 4 2 4 6)
			    (vector) -1 4))

(send inverse gen-inverse-behavior my-inst-0)

(define t (current-seconds))
(define x
(for/list ([i (* 16 930)])
  (send inverse interpret-inst my-inst input-state (list 0 1))))
(pretty-display `(t ,(- (current-seconds) t) ,(length x)))|#

