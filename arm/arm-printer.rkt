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

    (define (print-struct-inst x [indent ""])
      (pretty-display (format "~a(inst ~a ~a)" indent (inst-op x) (inst-args x))))

    (define (print-syntax-inst x [indent ""])
      (define ops-vec (inst-op x))
      (define args-vec (inst-args x))

      (define op (vector-ref ops-vec 0))
      (define args (vector-copy (vector-ref args-vec 0)))
      
      (define shfop (vector-ref ops-vec 2))
      (define shfarg (inst-shfarg x))
      
      (when (or (equal? op "str") (equal? op "ldr"))
	    (when (equal? "r11" (vector-ref args 1))
                  (vector-set! args 1 "fp"))
	    (when (not (equal? (substring (vector-ref args 2) 0 1) "r"))
		  (vector-set! args 2 (number->string (* 4 (string->number (vector-ref args 2)))))))
      
      (display (format "~a~a~a ~a" indent op (vector-ref ops-vec 1)
                       (string-join (vector->list args) ", ")))
      (when (and shfop (not (equal? shfop "")))
	    (display (format ", ~a ~a" shfop (vector-ref (vector-ref args-vec 2) 0))))
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
      (define ops-vec (inst-op x))
      (cond
       [(not ops-vec) x]
       [else
        (define args-vec (inst-args x))
        (define op0 (vector-ref ops-vec 0))
	(define args0 (vector-ref args-vec 0))
	(define args0-len (vector-length args0))
	(when (and (> args0-len 0)
		   (not (equal? "r" (substring (vector-ref args0 (sub1 args0-len)) 0 1)))
		   (not (member (string->symbol op0) '(bfc bfi sbfx ubfx))))
	      (set! op0 (string-append op0 "#")))
        
	(define shfop (vector-ref ops-vec 2))
	(define shfarg (and (vector-ref args-vec 2) (vector-ref (vector-ref args-vec 2) 0)))
	(when (and shfarg (not (equal? "r" (substring shfarg 0 1))))
	      (set! shfop (string-append shfop "#")))

        (define cond-type (vector-ref ops-vec 1))
        (inst (vector (send machine get-opcode-id (string->symbol op0))
                      (send machine get-cond-opcode-id (string->symbol cond-type))
                      (send machine get-shf-opcode-id (string->symbol shfop)))
              (vector (vector-map name->id args0)
                      #f
                      (and shfarg (vector (name->id shfarg)))))]))
                
    ;; Convert an instruction encoded using numbers
    ;; into an instruction in string format.
    (define (decode-inst x)
      (define ops-vec (inst-op x))
      (define args-vec (inst-args x))

      (define opcode (send machine get-opcode-name (vector-ref ops-vec 0)))
      (define condtype (send machine get-cond-opcode-name (vector-ref ops-vec 1)))
      (define shfop (send machine get-cond-opcode-name (vector-ref ops-vec 2)))

      (define (convert-op op)
	(let* ([str (symbol->string op)]
	       [len (string-length str)])
	  (if (equal? (substring str (sub1 len)) "#")
	      (substring str 0 (sub1 len))
	      str)))

      (define new-args-vec
	(for/vector 
	 ([args args-vec]
	  [types (send machine get-arg-types ops-vec)])
         (and args
              (for/vector ([arg args] [type types])
                          (cond
                           [(member type '(reg)) (format "r~a" arg)]
                           [(number? arg) (number->string arg)]
                           [else arg])))))

      (inst (vector-map convert-op ops-vec) new-args-vec))

    ;;;;;;;;;;;;;;;;;;;;;;; For compressing reg space ;;;;;;;;;;;;;;;;;;;;
    (define opcodes (get-field opcodes machine))

    (define (inner-rename x reg-map)
      (define (register-rename r)
        (cond
         [(and r (> (string-length r) 1) (equal? (substring r 0 1) "r"))
          (format "r~a" (vector-ref reg-map (string->number (substring r 1))))]
         
         [else r]))

      (define new-args-vec
        (for/vector
         ([args (inst-args x)])
         (and args (for/vector ([arg args]) (register-rename arg)))))

      (inst (inst-op x) new-args-vec))
    
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

        (for ([args (inst-args x)])
             (and args (for ([arg args]) (f args)))))
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

      (define (compress-reg-live live)
        (define ret (make-vector id))
        (for ([l live] [i (in-naturals)])
             (let ([index (vector-ref reg-map i)])
               (when index (vector-set! ret index l))))
        ret)

      ;; Generate outputs.
      (define compressed-program 
        (traverse program inst? (lambda (x) (inner-rename x reg-map)))) 
      (define compressed-live-out
        (progstate (compress-reg-live (progstate-regs live-out))
                   (progstate-memory live-out)
                   (progstate-z live-out)))
      (define compressed-live-in
        (and live-in
             (progstate (compress-reg-live (progstate-regs live-in))
                        (progstate-memory live-in)
                        (progstate-z live-in))))

      (values compressed-program compressed-live-out compressed-live-in
              reg-map-back id))

    (define (decompress-reg-space program reg-map)
      (traverse program inst? (lambda (x) (inner-rename x reg-map))))

    (define/public (encode-live x)
      (define reg (make-vector (send machine get-config) #f))
      (define memory #f)
      (define z #f)
      (for ([i x])
           (cond
            [(number? i) (vector-set! reg i #t)]
            [(equal? i 'memory) (set! memory #t)]
            [(equal? i 'z) (set! z #t)]))
      (progstate reg memory z))

    
    ;; Convert live-out (which is one of the outputs from 
    ;; parser::info-from-file) into string. 
    (define (output-constraint-string live-out)
      (format "(send printer encode-live '~a)" live-out))

    ))
