#lang racket

(require "../printer.rkt" 
         "../inst.rkt")

(provide llvm-demo-printer%)

(define llvm-demo-printer%
  (class printer%
    (super-new)
    (inherit-field machine)
    (override encode-inst decode-inst print-syntax-inst
              ;; Required method for cooperative search
              config-from-string-ir output-constraint-string)

    ;; Print in LLVM IR format.
    ;; x: instruction
    (define (print-syntax-inst x [indent ""])
      (define op (inst-op x))
      (unless (equal? op "nop")
              (define args (inst-args x))
              (display (format "~a = ~a i32 ~a" 
                               (vector-ref args 0)
                               op
                               (vector-ref args 1)))
              (for ([i (range 2 (vector-length args))])
                   (display (format ", ~a" (vector-ref args i))))
              (newline)))

    (define name2num (make-hash))
    (define num2name (make-vector 100))
    (define n 0)

    (define-syntax-rule (char1=% x) (equal? (substring x 0 1) "%"))
				  
    ;; Convert an insstruction x from string-IR to encoded-IR format.
    (define (encode-inst x)
      (cond
       [(equal? (inst-op x) "nop") (inst (get-field nop-id machine) (vector))]
       [(equal? (inst-op x) #f) x]
       [else
        (define args (inst-args x))
        ;; First input argument.
        (define first-in (vector-ref args 1))
        ;; Last input argument.
        (define last-in (vector-ref args (sub1 (vector-length args))))
        (define new-args
          (for/vector ([arg args])
                      (if (equal? (substring arg 0 1) "%")
                          ;; arg is a variable.
                          ;; Look up if the variable is already seen before.
                          (if (hash-has-key? name2num arg)
                              (hash-ref name2num arg)
                              ;; If not in the table, give it a fresh number
                              ;; and update the table.
                              (let ([id n])
                                (set! n (add1 n))
                                (hash-set! name2num arg id)
                                (vector-set! num2name id arg)
                                id))
                          (string->number arg))))
        (define op
          (string->symbol
           (cond
            [(and (char1=% first-in) (char1=% last-in))
             (inst-op x)]
            
            ;; Append # to opcode to indicate that last argument is a constant.
            [(char1=% first-in)
             (string-append (inst-op x) "#")]

            ;; Prepend _ to opcode to indicate that first argument is a constant.
            [(char1=% last-in)
             (string-append "_" (inst-op x))]
            
            [else
             (raise "Not support %out = op <imm>, <imm>")])))
        
        (inst (send machine get-opcode-id op) new-args)]))

    (define (fresh-name)
      (define numbers
	(filter 
	 number?
	 (map (lambda (x) (and (string? x) (string->number (substring x 1))))
	      (vector->list num2name))))
      (format "%~a" (add1 (foldl max 0 numbers))))
    
    (define (num->name x)
      (define name (vector-ref num2name x))
      (when (equal? name 0)
            (set! name (fresh-name))
            (vector-set! num2name x name))
      name)
    
    ;; Convert an instruction x from encoded-IR to string-IR format.
    (define (decode-inst x)
      (define op (symbol->string (send machine get-opcode-name (inst-op x))))
      (cond
       [(equal? op "nop") (inst op (vector))]
       [else
        (define args (inst-args x))
        (define first-in (vector-ref args 1))
        (define last-in (vector-ref args (sub1 (vector-length args))))
        (cond
         [(regexp-match #rx"#" op)
          (set! op (substring op 0 (sub1 (string-length op))))
          (set! first-in (num->name first-in))
          ]
         [(regexp-match #rx"_" op)
          (set! op (substring op 1))
          (set! last-in (num->name last-in))
          ]
         [else
          (set! first-in (num->name first-in))
          (set! last-in (num->name last-in))])
        
        (define out (num->name (vector-ref args 0)))

        (if (= (vector-length args) 3)
            (inst op (vector out first-in last-in))
            (inst op (vector out first-in)))]))

    ;; Convert liveness infomation to the same format as program state.
    ;; x: a list of variables' names.
    ;; output: vector of #t and #f.
    (define/public (encode-live x)
      (define live (make-vector (send machine get-config) #f))
      (for ([v x])
           (vector-set! live (hash-ref name2num (if (symbol? v) (symbol->string v) v)) #t))
      live)

    ;;;;;;;;;;;;;;;;;;;;; For cooperative search ;;;;;;;;;;;;;;;;;;
    
    ;; Return program state config from a given program in string-IR format.
    ;; program: string IR format
    ;; output: program state config, an input to machine:set-config
    (define (config-from-string-ir program)
      (define vars (list))
      (for* ([x program]
	     [arg (inst-args x)])
	    (when (and (equal? "%" (substring arg 0 1))
		       (not (member arg vars)))
		  (set! vars (cons arg vars))))
      (add1 (length vars)))
    
    ;; Convert live-out (which is one of the outputs from 
    ;; parser::info-from-file) into string. 
    (define (output-constraint-string live-out) 
      (format "(send printer encode-live '~a)" live-out))

    
    ))
