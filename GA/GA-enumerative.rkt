#lang racket

(require "../ast.rkt" "../enumerative.rkt" 
         "GA-simulator-racket.rkt" "GA-validator.rkt")
(require racket/generator)

(provide GA-enumerative%)

(define GA-enumerative%
  (class enumerative%
    (super-new)
    (inherit-field machine printer simulator validator generate-inst)
    (override len-limit window-size reset-generate-inst)

    (define (len-limit) 8)
    (define (window-size) 14)
    (set! simulator (new GA-simulator-racket% [machine machine]))
    (set! validator (new GA-validator% [machine machine] [printer printer]))
    
    (define inst-id (get-field inst-id machine))

    ;; Return pair of (instruction, live-out)
    ;; Since we don't use live-in to prune the search space here, we just return #f for live-out
    (define (reset-generate-inst states live-in)
      (define const-range (get-field const-range machine))
      (set! generate-inst
	    (generator 
	     ()
	     (for ([opcode-id (vector-length inst-id)])
		  (let ([opcode-name (vector-ref inst-id opcode-id)])
		    (cond 
		     [(equal? opcode-name `nop) (void)]
		     [(equal? opcode-name `@p)
		      (for ([c const-range])
			   (yield (cons (inst opcode-id c) #f)))]
		     [else (yield (cons (inst opcode-id #f) #f))])))
	     (yield #f))))
    ))
