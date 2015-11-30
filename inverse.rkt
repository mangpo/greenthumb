#lang racket

(require "inst.rkt")
(provide inverse% hash-insert-to-list)

(define-syntax-rule (hash-insert-to-list table key val)
  (if (hash-has-key? table key)
      (hash-set! table key
                 (cons val (hash-ref table key)))
      (hash-set! table key (list val))))

(define inverse%
  (class object%
    (super-new)
    (init-field machine simulator)
    (abstract gen-inverse-behavior interpret-inst)
    (public uid-inst-in-out lookup-bw)
    
    (define inst-id (get-field inst-id machine))

    ;; Return unique representation for
    ;;  i) instruction including opcode and constant arguments (IDs list)
    ;; and extract IDs of
    ;;  ii) input arguments (IDs list)
    ;;  iii) output arguments (IDs list)
    (define (uid-inst-in-out x)
      (define opcode (inst-op x))
      (define args (inst-args x))
      (define inst-type (list opcode))

      (define in (list))
      (define out (list))
      
      (for ([arg args]
	    [type (send machine get-arg-types (vector-ref inst-id opcode))])
	   (cond
	    [(member type '(reg-o var-o)) (set! out (cons arg out))]
	    [(member type '(reg-i var-i)) (set! in (cons arg in))]
	    [(member type '(reg-io var-io))
             (set! in (cons arg in)) (set! out (cons arg out))]
	    [else (set! inst-type (cons arg inst-type))]))

      (values (reverse inst-type) (reverse in) out))

    (define (lookup-bw mapping in out-vals state-base)
      (define ret (and (hash-has-key? mapping out-vals) (list)))

      (when ret
	    (define vars-in-val-list (hash-ref mapping out-vals))
	    (for ([vars-in-val vars-in-val-list])
		 (let ([new-state (vector-copy state-base)]
		       [pass #t])
		   (for ([r in]
			 [v vars-in-val] #:break (not pass))
			(cond
			 [(vector-ref new-state r)
			  (unless (= v (vector-ref new-state r))
				  (set! pass #f))]
			 [else (vector-set! new-state r v)]))
		   (when pass (set! ret (cons new-state ret))))))

      ret)
      
    
    ))
