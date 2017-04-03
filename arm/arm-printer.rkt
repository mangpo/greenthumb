#lang racket

(require "../printer.rkt" 
         "../inst.rkt"
         "arm-machine.rkt")

(provide arm-printer%)

(define arm-printer%
  (class printer%
    (super-new)
    (inherit-field machine report-mutations)
    (override encode-inst decode-inst print-struct-inst print-syntax-inst
              compress-state-space decompress-state-space
              output-constraint-string)

    (define (print-struct-inst x [indent ""])
      (pretty-display (format "~a(inst ~a ~a)" indent (inst-op x) (inst-args x))))

    (define (print-syntax-inst x [indent ""])
      (define ops-vec (inst-op x))
      (define args (vector-copy (inst-args x)))

      (define op (vector-ref ops-vec 0))
      
      (define shfop (vector-ref ops-vec 2))
      
      (when (or (equal? op "str") (equal? op "ldr"))
	    (when (equal? "r11" (vector-ref args 1))
                  (vector-set! args 1 "fp"))
	    (when (not (equal? (substring (vector-ref args 2) 0 1) "r"))
		  (vector-set! args 2 (number->string (* 4 (string->number (vector-ref args 2)))))))

      (define args-list (vector->list args))
      (define len (length args-list))
      (cond
       [(equal? op "nop") (display "nop")]
       [(and shfop (not (equal? shfop "")))
        (display (format "~a~a~a ~a" indent op (vector-ref ops-vec 1)
                         (string-join (take args-list (sub1 len)) ", ")))
        (display (format ", ~a ~a" shfop (last args-list)))]
       [else 
        (display (format "~a~a~a ~a" indent op (vector-ref ops-vec 1)
                         (string-join args-list ", ")))])
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
        (define args (inst-args x))
        (define op0 (vector-ref ops-vec 0))
        (define shfop (vector-ref ops-vec 2))
	(define args-len (vector-length args))
	(when (and (> args-len 0)
		   (not (equal? "r" (substring (vector-ref args (sub1 args-len)) 0 1)))
                   (or (not shfop) (equal? shfop ""))
		   (not (member (string->symbol op0) '(bfc bfi sbfx ubfx))))
	      (set! op0 (string-append op0 "#")))
        
	(define shfarg (and (> args-len 0) (vector-ref args (sub1 args-len))))
	(when (and shfarg (not (equal? "r" (substring shfarg 0 1))))
	      (set! shfop (string-append shfop "#")))

        (define cond-type (vector-ref ops-vec 1))
        (inst (vector (send machine get-base-opcode-id (string->symbol op0))
                      (send machine get-cond-opcode-id (string->symbol cond-type))
                      (send machine get-shf-opcode-id (string->symbol shfop)))
              (vector-map name->id args))]))
                
    ;; Convert an instruction encoded using numbers
    ;; into an instruction in string format.
    (define (decode-inst x)
      (define ops-vec (inst-op x))
      (define args (inst-args x))

      (define test (send machine has-opcode-id? ops-vec))
      (unless test
              (vector-set! ops-vec 2 -1)
              (set! test (send machine has-opcode-id? ops-vec)))
      (unless test
              (vector-set! ops-vec 1 -1)
              (set! test (send machine has-opcode-id? ops-vec)))
      
      (define (convert-op op)
	(let* ([str (symbol->string op)]
	       [len (string-length str)])
	  (if (and (> len 0) (equal? (substring str (sub1 len)) "#"))
	      (substring str 0 (sub1 len))
	      str)))
      
      (define opcode (convert-op (send machine get-base-opcode-name (vector-ref ops-vec 0))))
      ;;(pretty-display `(op ,opcode))
      (define condtype (convert-op (send machine get-cond-opcode-name (vector-ref ops-vec 1))))
      (define shfop (convert-op (send machine get-shf-opcode-name (vector-ref ops-vec 2))))


      (define new-args
        (for/vector ([arg args] [type (send machine get-arg-types ops-vec)])
                     (cond
                      [(member type '(reg reg-sp)) (format "r~a" arg)]
                      [(number? arg) (number->string arg)]
                      [else arg])))

      (inst (vector opcode condtype shfop) new-args))

    ;;;;;;;;;;;;;;;;;;;;;;; For compressing reg space ;;;;;;;;;;;;;;;;;;;;
    (define (inner-rename x reg-map)
      (define (register-rename r)
        (cond
         [(and r (> (string-length r) 1) (equal? (substring r 0 1) "r"))
          (format "r~a" (vector-ref reg-map (string->number (substring r 1))))]
         
         [else r]))

      (define new-args
        (for/vector ([arg (inst-args x)]) (register-rename arg)))

      (inst (inst-op x) new-args))

    ;; Input
    ;; program: string IR format
    ;; Output
    ;; 1) compressed program in the same format as input
    ;; 2) compressed live-out
    ;; 3) map-back
    ;; 4) program state config
    (define (compress-state-space program live-out)
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

      ;; Generate outputs.
      (define compressed-program
        (vector-map (lambda (x) (inner-rename x reg-map)) program))

      (define live-out-extra (filter symbol? live-out))
      (define compressed-live-out 
        (map (lambda (x) (vector-ref reg-map x)) 
             (filter (lambda (x) (and (number? x) (<= x max-reg) (vector-ref reg-map x))) 
                     live-out)))

      (values compressed-program
              (append compressed-live-out live-out-extra)
              reg-map-back id))

    (define (decompress-state-space program reg-map)
      (vector-map (lambda (x) (inner-rename x reg-map)) program))

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
