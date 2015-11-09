#lang racket

(require "../compress.rkt" "../ast.rkt" "arm-ast.rkt" "../machine.rkt" "arm-machine.rkt")

(provide arm-compress%)

(define arm-compress%
  (class compress%
    (super-new)
    (inherit-field machine)
    (override compress-reg-space decompress-reg-space)
    
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
    ;; program, a list of (inst opcode args) where opcode is string and args is a list of string
    ;; Output
    ;; 1) compressed program in the same format as input
    ;; 2) compressed live-out
    ;; 3) map-back
    ;; 4) machine-info in custom format--- (list nregs nmem) for arm
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

    ))
