#lang racket

(require "../printer.rkt" 
         "../ast.rkt" "arm-ast.rkt"
         "arm-machine.rkt")

(provide arm-printer%)

(define arm-printer%
  (class printer%
    (super-new)
    (inherit-field machine report-mutations)
    (override encode-inst decode-inst print-struct-inst print-syntax-inst
              get-constants)
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
    ;; into an instructions encoded using numbers.
    (define (encode-inst x)
      (define op (inst-op x))
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
		      (send machine get-cond-inst-id (string->symbol cond-type))))))
                

    ;; Convert an instruction encoded using numbers
    ;; into an instructions in string format.
    (define (decode-inst x)
      (define opcode (send machine get-inst-name (inst-op x)))
      ;;(pretty-display `(decode-inst ,opcode ,(inst-shfop x)))
      (define args (inst-args x))
      (define cond-type (inst-cond x))
      (define class-id (send machine get-class-id opcode))
      (define shf (member opcode (get-field inst-with-shf machine)))
      (define shfop (and shf (inst-shfop x) (send machine get-shf-inst-name (inst-shfop x))))
      (define shfarg (and shf (inst-shfarg x)))
      
      (define-syntax-rule (make-inst x ...)
        (make-inst-main (list x ...)))

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
	      (imm arg)
	      (reg arg))))
      
      (define (make-inst-main fs)
        (define new-args (for/vector ([f fs] [arg args]) (f arg)))
        (arm-inst (convert-op opcode) new-args
		  (and shfop (convert-op shfop))
		  (and shfarg (convert-shfarg shfarg shfop))
		  (if (= cond-type (send machine get-cond-inst-id `nop))
		      "" 
		      (send machine get-cond-inst-name cond-type))))

      (define (reg x) (format "r~a" x))
      (define imm number->string)

      (cond
       [(equal? class-id 0) (make-inst reg reg reg)]
       [(equal? class-id 1) (make-inst reg reg imm)]
       ;[(equal? class-id 2) (make-inst reg reg imm)]
       [(equal? class-id 2) (make-inst reg reg)]
       [(equal? class-id 3) (make-inst reg imm)]
       [(equal? class-id 4) (make-inst reg reg reg reg)]
       [(equal? class-id 5) (make-inst reg reg imm imm)]
       [(equal? class-id 6) (make-inst reg identity imm)]
       [(member opcode '(bfc)) (make-inst reg imm imm)]
       [(equal? opcode `nop) (arm-inst "nop" (vector) #f #f "")]
       [else (raise (format "decode-inst: undefined for ~a" opcode))]))

    (define (get-constants-inst x)
      (define opcode (send machine get-inst-name (inst-op x)))
      (define args (inst-args x))
      (define class-id (send machine get-class-id opcode))
      (define shf (member opcode (get-field inst-with-shf machine)))
      (define shfop (and shf (inst-shfop x) (send machine get-shf-inst-name (inst-shfop x))))
      (define shfarg (and shf (inst-shfarg x)))
      ;; (pretty-display `(shf ,shf ,shfop))

      (define-syntax-rule (collect x ...)
        (collect-main (list x ...)))

      (define (collect-main fs)
        (define const-set (set))
        (define bit-set (set))
	(define op2-set
	  (if (and shfop (not (equal? shfop `nop)))
	      (set shfarg)
	      (set)))

        (for ([f fs] 
              [arg args])
             (cond
              [(equal? f `op2) (set! op2-set (set-add op2-set arg))]
              [(equal? f `const) (set! const-set (set-add const-set arg))]
              [(equal? f `bit) (set! bit-set (set-add bit-set arg))]
	      ))
        (list op2-set const-set bit-set))

      (define reg #f)
      (define bit `bit)
      (define op2 `op2) 
      (define const `const)

      (cond
       [(equal? class-id 0) (collect reg reg reg)]
       [(equal? class-id 1) (collect reg reg op2)]
       [(equal? class-id 2) (collect reg reg)]
       [(equal? class-id 3) (collect reg const)]
       [(equal? class-id 4) (collect reg reg reg reg)]
       [(equal? class-id 5) (collect reg reg bit bit)]
       [(equal? class-id 6) (collect reg #f #f)]
       [(member opcode '(bfc)) (collect reg bit bit)]
       [(equal? opcode `nop) (cons (list) (list))]
       [else (raise (format "decode-inst: undefined for ~a" opcode))]))

    (define (get-constants code)
      (define op2-set (set))
      (define const-set (set))
      (define bit-set (set))
      (for ([x code])
           (let ([ans (get-constants-inst x)])
	     ;; (print-struct-inst x)
	     ;; (pretty-display `(const ,(set->list (first ans)) ,(set->list (second ans)) ,(set->list (third ans))))
             (set! op2-set (set-union op2-set (first ans)))
             (set! const-set (set-union const-set (second ans)))
             (set! bit-set (set-union bit-set (third ans)))))
      (list op2-set const-set bit-set)) 
    ))
