#lang racket

(require "stat.rkt")

(define code 
"
	movsi LR4, -1
	and LR3, LR1, LR0
	xor LR1, LR0, LR4
	and LR2, LR2, LR1
	or LR0, LR2, LR3
")

(define bit 32)
(define nregs 5)
(define nmems 1)
(define live-regs (list 0))
(define live-memory (list))
(define synthesize #t)
(define path "output/driver")
(define cores 10)

(system (format "rm ~a*" path))

(define (create-file id)
  (define (req file)
    (format "(file \"/bard/wilma/pphothil/superopt/modular-optimizer/~a\")" file))
  (define require-files 
    (string-join 
     (map req 
          '(ast.rkt stochastic.rkt 
                    vpe/parser.rkt vpe/state.rkt vpe/print.rkt 
                    vpe/solver-support.rkt))))
  (define live-regs-str (string-join (map number->string live-regs)))
  (define live-memory-str (string-join (map number->string live-memory)))
  (with-output-to-file #:exists 'truncate (format "~a-~a.rkt" path id)
    (thunk
     (pretty-display (format "#lang racket"))
     (pretty-display (format "(require ~a)" require-files))
     (pretty-display (format "(set-bit! ~a)" bit))
     (pretty-display (format "(set-nregs! ~a)" nregs))
     (pretty-display (format "(set-nmems! ~a)" nmems))
     (pretty-display (format "(define code (ast-from-string \"~a\"))" code))
     (pretty-display (format "(define encoded-code (encode code #f))"))
     (pretty-display (format "(stochastic-optimize encoded-code ~a (constraint ~a [reg ~a] [mem ~a]) #:synthesize ~a #:name \"~a-~a\")" 
                             nmems nmems live-regs-str live-memory-str synthesize path id))))
  
  )

(define (run-file id)
  ;(define out-port (open-output-file (format "~a-~a.log" path id) #:exists 'truncate))
  (define-values (sp o i e) (subprocess #f #f #f
                                        (find-executable-path "racket") (format "~a-~a.rkt" path id)))
  sp)

(define processes
  (for/list ([id cores])
    (create-file id)
    (run-file id)))

(with-handlers* 
 ([exn:break? (lambda (e)
                (for ([sp processes])
                  (when (equal? (subprocess-status sp) 'running)
                    (subprocess-kill sp #f)))
                (sleep 3)
                )])
 (for ([sp processes])
   (sync sp)))

(define stats
  (for/list ([id cores])
    (create-stat-from-file (format "~a-~a.stat" path id))))

(print-stat-all stats)

(pretty-display "done")


                       