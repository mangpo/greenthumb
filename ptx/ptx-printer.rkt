#lang racket

(require "../printer.rkt" "../inst.rkt" "ptx-machine.rkt")

(provide ptx-printer%)

(define ptx-printer%
  (class printer%
    (super-new)
    (inherit-field machine)
    (override encode-inst decode-inst print-syntax-inst)

    ;; Print in the assembly format.
    ;; x: string IR
    (define (print-syntax-inst x [indent ""])
      (define opcode-name (inst-op x))
      
      (define new-opcode-name
	(cond
	 [(member opcode-name (list "shr" "mul.wide" "rem"))
	  (format "~a.u32" opcode-name)]

	 [(member opcode-name (list "and" "setp" "selp"))
	  (format "~a.b32" opcode-name)]

	 [(member opcode-name (list "not"))
	  (format "~a.pred" opcode-name)]

	 [else
	  (format "~a.s32" opcode-name)]
	 ))
      
      (pretty-display (format "~a ~a;"
                              new-opcode-name
                              (string-join (vector->list (inst-args x)) ", "))))

    ;; Convert an instruction x from string-IR to encoded-IR format.
    (define (encode-inst x)
      ;; Example
      (define opcode-name (inst-op x))

      (cond
       [opcode-name
        (define args (inst-args x))
        (define last-arg (vector-ref args (sub1 (vector-length args))))

        ;; If last arg is not a register, append "#" to opcode-name.
        ;; This is for distinguishing instructions that take (reg reg reg) vs (reg reg const).
        ;; They need distinct opcode names.
        (unless (equal? (substring last-arg 0 1) "%")
                (set! opcode-name (string-append opcode-name "#")))

        ;; A function to convert argument in string format to number.
        (define (convert-arg arg)
	  (cond
	   ;; [(equal? (substring arg 0 3) "%rd")
	   ;;  (string->number (substring arg 3))]

	   [(equal? (substring arg 0 1) "%") ;; %p, %r
	    (string->number (substring arg 2))]

	   [else
	    (string->number arg)]))
	
        (inst (send machine get-opcode-id (string->symbol opcode-name))
              ;; get-opcode-id takes symbol (not string) as argument
              (vector-map convert-arg args))]

       ;; opcode-name is #f, x is an unknown instruction (a place holder for synthesis)
       ;; just return x in this case
       [else x]))

            

    ;; Convert an instruction x from encoded-IR to string-IR format.
    (define (decode-inst x)
      ;; Example
      (define opcode-id (inst-op x))
      ;; get-opcode-name returns symbol, so we need to convert it to string
      (define opcode-name (symbol->string (send machine get-opcode-name opcode-id)))
      (define str-len (string-length opcode-name))
      (define arg-types (send machine get-arg-types opcode-id))
      (define args (inst-args x))

      ;; Remove #
      (when (equal? "#" (substring opcode-name (sub1 str-len)))
            (set! opcode-name (substring opcode-name 0 (sub1 str-len))))

      #;(define new-opcode-name
	(cond
	 [(member opcode-name (list "shr" "mul.wide" "rem"))
	  (format "~a.u32" opcode-name)]

	 [(member opcode-name (list "and" "setp" "selp"))
	  (format "~a.b32" opcode-name)]

	 [(member opcode-name (list "not"))
	  (format "~a.pred" opcode-name)]

	 [else
	  (format "~a.s32" opcode-name)]
	 ))

      (define new-args
        (for/vector ([arg args] [type arg-types])
                    (cond
                     [(equal? type 'reg) (format "%r~a" arg)]
                     [(equal? type 'pred) (format "%p~a" arg)]
                     [else (number->string arg)])))

      (inst opcode-name new-args))

    ;;;;;;;;;;;;;;;;;;;;;;;;; For cooperative search ;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; Convert live-out (the output from parser::info-from-file) into string. 
    ;; The string will be used as a piece of code the search driver generates as
    ;; the live-out argument to the method superoptimize of 
    ;; stochastics%, forwardbackward%, and symbolic%.
    ;; The string should be evaluated to a program state that contains 
    ;; #t and #f, where #t indicates that the corresponding element is live.
    (define/override (output-constraint-string live-out)
      ;; Method encode-live is implemented below, returning
      ;; live infomation in a program state format.
      (format "(send printer encode-live '~a)" live-out))
    
    (define/override (output-assume-string assume)
      ;; Method encode-live is implemented below, returning
      ;; live infomation in a program state format.
      (format "(send printer encode-assume '~a)" assume))

    ;; Convert liveness infomation to the same format as program state.
    (define/public (encode-live x)
      (define config (send machine get-config))
      (define reg-live (make-vector (car config) #f))
      (define pred-live (make-vector (cdr config) #f))
      
      (for ([v (car x)]) (vector-set! reg-live v #t))
      (for ([v (cdr x)]) (vector-set! pred-live v #t))
      (progstate reg-live pred-live))

    (define/public (encode-assume x)
      (define config (send machine get-config))
      (define reg-assume (make-vector (car config) #f))
      (define pred-assume (make-vector (cdr config) #f))
      
      (for ([c (car x)])
	   (let ([index (first c)]
		 [op (second c)]
		 [val (third c)])
	     (vector-set! reg-assume index (cons op val))))
      
      (for ([c (cdr x)])
	   (let ([index (first c)]
		 [op (second c)]
		 [val (third c)])
	     (vector-set! pred-assume index (cons op val))))
      (progstate reg-assume pred-assume))
    
    ;; Return program state config from a given program in string-IR format.
    ;; program: string IR format
    ;; output: program state config
    (define/override (config-from-string-ir program)
      ;; config = number of registers
      ;; Find the highest register ID and return that as a config
      (define max-reg 2)
      (define max-pred 1)
      (for* ([x program]
	     [arg (inst-args x)])
	    (when (equal? (substring arg 0 1) "%") ;; register or predicate
		  (let ([id (string->number (substring arg 2))])
		    (cond
		     [(equal? (substring arg 1 2) "r")
		      (when (> id max-reg) (set! max-reg id))]
		     [(equal? (substring arg 1 2) "p")
		      (when (> id max-pred) (set! max-pred id))])
		    )))
			
      (cons (add1 max-reg) (add1 max-pred)))
    
    ))

