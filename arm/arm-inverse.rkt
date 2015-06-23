#lang racket

(require "../ops-racket.rkt" "../ast.rkt"
         "arm-ast.rkt" "arm-machine.rkt" "arm-simulator-racket.rkt"
         "arm-printer.rkt" "arm-parser.rkt")

(provide arm-inverse%)

(define arm-inverse%
  (class object%
    (super-new)
    (init-field machine simulator)
    (public gen-inverse-behavior)

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

      (values (reverse inst-type) (list->vector (reverse in)) (list->vector (reverse out))))
    
    (define (gen-inverse-behavior my-inst)
      (define opcode-name (vector-ref inst-id (inst-op my-inst)))
      (define in (make-vector 5 #f))
      (define out (make-vector 5 #f))
      (define behavior-bw (make-hash))
      
      (define (recurse-regs in-list in-res)
	(cond
	 [(empty? in-list)
          (define out-state
            (send simulator interpret
                  (vector my-inst)
                  (progstate (list->vector in-res) (vector) -1 fp) #:dep #f))
          
          (define in-list-filtered (filter number? in-res))
          (define out-list (list))
          (for ([r (progstate-regs out-state)]
                [m out])
               (when m (set! out-list (cons r out-list))))
          
          (define key (reverse out-list))
          (if (hash-has-key? behavior-bw key)
              (hash-set! behavior-bw key
                         (cons in-list-filtered (hash-ref behavior-bw key)))
              (hash-set! behavior-bw key (list in-list-filtered)))]

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
      (cons x behavior-bw))

    ))

#|
(define machine (new arm-machine% [bit 4]))
(define simulator (new arm-simulator-racket% [machine machine]))

(define inverse (new arm-inverse% [machine machine] [simulator simulator]))
(send machine set-config (list 5 0 4))
(define printer (new arm-printer% [machine machine]))
(define parser (new arm-parser%))
(define my-inst 
  (vector-ref (send printer encode 
                    (send parser ast-from-string "bfc r0, 1, 2"))
              0))

(send inverse gen-inverse-behavior my-inst)
|#
