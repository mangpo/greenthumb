#lang racket

(require "../printer.rkt" 
         "../inst.rkt" "arm-inst.rkt"
         "arm-machine.rkt")

(provide arm-printer%)

(define arm-printer%
  (class printer%
    (super-new)
    (inherit-field machine report-mutations)
    (override encode-inst decode-inst print-struct-inst print-syntax-inst
              compress-reg-space decompress-reg-space
              output-constraint-string)
    (set! report-mutations (vector-append report-mutations '#(shf cond-type)))

    (define (print-struct-inst x [indent ""])
      (pretty-display (format "~a(inst ~a ~a ~a ~a ~a)" 
                              indent (inst-op x) (inst-args x)
                              (inst-shfop x) (inst-shfarg x)
                              (inst-cond x))))

    (define (print-syntax-inst x [indent ""])
      (define op (inst-op x))
      (define args (vector-copy (inst-args x)))
      (define shfop (inst-shfop x))
      (define shfarg (inst-shfarg x))
      (when (or (equal? op "str") (equal? op "ldr"))
	    (vector-set! args 1 "fp")
	    (when (not (equal? (substring (vector-ref args 2) 0 1) "r"))
		  (vector-set! args 2 (number->string (* 4 (string->number (vector-ref args 2)))))))
      (display (format "~a~a~a ~a" indent op (inst-cond x) (string-join (vector->list args) ", ")))
      (when (and shfop (not (equal? shfop "nop")))
	    (display (format ", ~a ~a" shfop shfarg)))
      (newline))

    (define (name->id name)
      ;;(pretty-display `(name->id ,name))
      (cond
       [(string->number name)
        (string->number name)]
       
       [(and (> (string-length name) 1) (equal? (substring name 0 1) "r"))
        (string->number (substring name 1))]

       [(equal? name "fp") "fp"]
       
       [else 
        (raise (format "encode: name->id: undefined for ~a" name))]))

    ;; Convert an instruction in string format into
    ;; an instruction encoded using numbers.
    (define (encode-inst x)
      (define op (inst-op x))
      (cond
       [(not op) x]
       [op
	(define args (inst-args x))
	(define args-len (vector-length args))
	(when (and (> args-len 0)
		   (not (equal? "r" (substring (vector-ref args (sub1 args-len)) 0 1)))
		   (not (member (string->symbol op) '(bfc bfi sbfx ubfx))))
	      (set! op (string-append op "#")))
	(define shfop (inst-shfop x))
	(define shfarg (inst-shfarg x))
	(when (and shfarg (not (equal? "r" (substring shfarg 0 1))))
	      (set! shfop (string-append shfop "#")))
	
	(let ([cond-type (arm-inst-cond x)])
	  (arm-inst (send machine get-inst-id (string->symbol op))
		    (vector-map name->id args)
		    (and shfop (send machine get-shf-inst-id (string->symbol shfop)))
		    (and shfarg (name->id shfarg))
		    (if (equal? cond-type "")
			(send machine get-cond-inst-id `nop)
			(send machine get-cond-inst-id (string->symbol cond-type)))))]))
                

    ;; Convert an instruction encoded using numbers
    ;; into an instruction in string format.
    (define (decode-inst x)
      (define opcode (send machine get-inst-name (inst-op x)))
      (define args (inst-args x))
      (define cond-type (inst-cond x))
      (define shf (member opcode (get-field inst-with-shf machine)))
      (define shfop (and shf (inst-shfop x) (send machine get-shf-inst-name (inst-shfop x))))
      (define shfarg (and shf (inst-shfarg x)))
      ;;(pretty-display `(decode-inst ,opcode ,class-id))

      (define (convert-op op)
	(let* ([str (symbol->string op)]
	       [len (string-length str)])
	  (if (equal? (substring str (sub1 len)) "#")
	      (substring str 0 (sub1 len))
	      str)))

      (define (convert-shfarg arg op)
	(let* ([str (symbol->string op)]
	       [len (string-length str)])
	  (if (equal? (substring str (sub1 len)) "#")
	      (number->string arg)
	      (format "r~a" arg))))

      (define new-args
	(for/vector 
	 ([arg args]
	  [type (send machine get-arg-types opcode)])
	 (cond
	  [(member type '(reg-o reg-i reg-io)) (format "r~a" arg)]
	  [(number? arg) (number->string arg)]
	  [else arg])))

      (if (equal? opcode `nop) 
	  (arm-inst "nop" (vector) #f #f "")
	  (arm-inst (convert-op opcode) new-args
		    (and shfop (convert-op shfop))
		    (and shfarg (convert-shfarg shfarg shfop))
		    (if (= cond-type (send machine get-cond-inst-id `nop))
			"" 
			(send machine get-cond-inst-name cond-type))))
      )

    ;;;;;;;;;;;;;;;;;;;;;;; For compressing reg space ;;;;;;;;;;;;;;;;;;;;
    (define inst-id (get-field inst-id machine))
    (define branch-inst-id (get-field branch-inst-id machine))

    (define (inner-rename x reg-map)
      (define (register-rename r)
        (cond
         [(and r (> (string-length r) 1) (equal? (substring r 0 1) "r"))
          (format "r~a" (vector-ref reg-map (string->number (substring r 1))))]
         
         [else r]))
      
      (arm-inst (inst-op x) 
		(list->vector (map register-rename (vector->list (inst-args x))))
		(inst-shfop x)
		(register-rename (inst-shfarg x))
		(inst-cond x)))
    ;; Input
    ;; program: string IR format
    ;; Output
    ;; 1) compressed program in the same format as input
    ;; 2) compressed live-out
    ;; 3) compressed live-in
    ;; 4) map-back
    ;; 5) program state config
    (define (compress-reg-space program live-out live-in)
      (define reg-set (mutable-set))
      (define max-reg 0)

      ;; Collect all used register ids.
      (define (inner-collect x)
	(define (f r)
	  (when (and r (> (string-length r) 1) (equal? (substring r 0 1) "r"))
		(let ([reg-id (string->number (substring r 1))])
		  (set-add! reg-set reg-id)
		  (when (> reg-id max-reg) (set! max-reg reg-id)))))
	  
        (for ([r (inst-args x)]) (f r))
	(f (inst-shfarg x)))
      (for ([x program]) (inner-collect x))

      ;; Construct register map from original to compressed version.
      (set-add! reg-set (+ max-reg 1))
      (set! max-reg (+ max-reg 1))
      (define reg-map (make-vector (add1 max-reg) #f))
      (define id 0)
      (for ([i 32])
           (when (set-member? reg-set i)
                 (vector-set! reg-map i id)
                 (set! id (add1 id))))

      ;; Construct register map from compressed back to original version. +2 regs
      (define reg-map-back (make-vector id))
      (set! id 0)
      (for ([i 32])
           (when (set-member? reg-set i)
                 (vector-set! reg-map-back id i)
                 (set! id (add1 id))))

      ;; Check if program access memory or not.
      ;; (define mem-access #f)
      ;; (for ([x program])
      ;;      (let ([opcode (inst-op x)])
      ;;        (when (or (equal? opcode "str") (equal? opcode "ldr"))
      ;;              (set! mem-access #t))))
      (define min-offset #f)
      (define max-offset #f)
      (for ([x program])
	   (let ([op (inst-op x)])
	     (when (and (or (equal? op "str") (equal? op "ldr")))
		   (let ([offset (string->number (vector-ref (inst-args x) 2))])
		     (when offset
			   (when (or (not min-offset) (< offset min-offset))
				 (set! min-offset offset))
			   (when (or (not max-offset) (> offset max-offset))
				 (set! max-offset offset)))))))
      (define nmems (if min-offset (add1 (- max-offset min-offset)) 1))
      (define fp (if min-offset (- min-offset) 0))
      (pretty-display `(min-max-offset ,min-offset ,max-offset))

      ;; Generate outputs.
      (define compressed-program 
        (traverse program inst? (lambda (x) (inner-rename x reg-map)))) 
      (define compressed-live-out 
        (map (lambda (x) (vector-ref reg-map x)) 
             (filter (lambda (x) (and (<= x max-reg) (vector-ref reg-map x))) 
                     (first live-out))))
      (define compressed-live-in 
        (and (first live-in)
             (map (lambda (x) (vector-ref reg-map x)) 
                  (filter (lambda (x) (and (<= x max-reg) (vector-ref reg-map x))) 
                          (first live-in)))))

      (values compressed-program
              (list compressed-live-out (second live-out) (third live-out))
              (list compressed-live-in (second live-in) (third live-in))
              reg-map-back 
              (list id nmems fp)))

    (define (decompress-reg-space program reg-map)
      (traverse program inst? (lambda (x) (inner-rename x reg-map))))

    
    ;; Convert live-out (in compact format) into Racket string
    ;; (in progstate format).
    ;; This is used for creating multiple search instances.
    ;; live-out: a pair of (a list of live registers' ids, live memory, live flag)
    ;; output: output constraint in Racket string format. When executing, the expression should be evaluated to a progstate with #t and #f indicating which entries are constrainted (live).
    (define (output-constraint-string live-out)
      (define live-mem (second live-out))
      (define live-flag (third live-out))
      (cond
       [(first live-out)
        (define live-regs-str (string-join (map number->string (first live-out))))
        (if live-mem
            (format "(constraint machine [reg ~a] [mem-all] [z ~a])" live-regs-str live-flag)
            (format "(constraint machine [reg ~a] [mem] [z ~a])" live-regs-str live-flag))]
       [(or live-mem live-flag)
        (if live-mem
            (format "(constraint machine [reg] [mem-all] [z ~a])" live-flag)
            (format "(constraint machine [reg] [mem] [z ~a])" live-flag))]
       [else #f]))

    ))
