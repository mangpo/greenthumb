#lang racket

(require "stat.rkt" 
         "vpe/llvm-parser.rkt" "vpe/parser.rkt"
         "vpe/machine.rkt" "vpe/compress.rkt" "vpe/print.rkt")
(provide optimize)

(define (optimize-inner code-org live-out-org synthesize dir cores time-limit)
  (define path (format "~a/driver" dir))
  (system (format "mkdir ~a" dir))
  (system (format "rm ~a*" path))
  
  (pretty-display ">>> select code:")
  (print-syntax code-org #:LR #f)
  (pretty-display (format ">>> live-out-org: ~a" live-out-org))

  ;; Use the fewest number of registers possible.
  (define-values (code live-out map-back machine-info) 
    (compress-reg-space code-org live-out-org))
  (pretty-display ">>> compressed-code:")
  (print-syntax code #:LR #f)
  (pretty-display (format ">>> machine-info: ~a" machine-info))
  (pretty-display (format ">>> live-out: ~a" live-out))
  
  (define (create-file id)
    (define (req file)
      (format "(file \"/bard/wilma/pphothil/superopt/modular-optimizer3/~a\")" file))
    (define require-files 
      (string-join 
       (map req 
            '(ast.rkt stochastic.rkt 
                      vpe/parser.rkt vpe/machine.rkt vpe/print.rkt 
                      vpe/solver-support.rkt))))
    (with-output-to-file #:exists 'truncate (format "~a-~a.rkt" path id)
      (thunk
       (pretty-display (format "#lang racket"))
       (pretty-display (format "(require ~a)" require-files))
       (pretty-display (set-machine-config-string machine-info))
       (pretty-display (format "(define code (ast-from-string \""))
       (print-syntax code #:LR #f)
       (pretty-display "\"))")
       (pretty-display (format "(define encoded-code (encode code #f))"))
       (pretty-display (format "(stochastic-optimize encoded-code ~a ~a \"~a-~a\" ~a)" 
                               (output-constraint-string live-out)
                               synthesize path id time-limit))
       ;;(pretty-display "(dump-memory-stats)"
       )))
  
  (define (run-file id)
    (define out-port (open-output-file (format "~a-~a.log" path id) #:exists 'truncate))
    (define-values (sp o i e) 
      (subprocess out-port #f out-port (find-executable-path "racket") (format "~a-~a.rkt" path id)))
    sp)
  
  (define (wait)
    ;;(pretty-display "wait")
    (sleep 10)
    (define check-file 
      (with-output-to-string (thunk (system (format "ls ~a*.stat | wc -l" path)))))
    (define n 
      (string->number (substring check-file 0 (- (string-length check-file) 1))))
    ;;(pretty-display `(n ,check-file ,n ,cores ,(= n cores)))
    (pretty-display (format "There are currently ~a stats." n))
    (unless (= n cores)
            (wait)))
  
  (define (update-stats)
    (unless (andmap (lambda (sp) (not (equal? (subprocess-status sp) 'running))) processes)
            (get-stats)
            (sleep 10)
            (update-stats)))
  
  (define (get-stats)
    (define stats
      (for/list ([id cores])
                (create-stat-from-file (format "~a-~a.stat" path id))))
    (with-handlers* 
     ([exn? (lambda (e) (pretty-display "Error: print stat"))])
     (let ([output-id (print-stat-all stats)])
       (pretty-display (format "output-id: ~a" output-id))
       output-id
       )
     )
    )
  
  (define processes
    (for/list ([id cores])
              (create-file id)
              (run-file id)))
  
  (with-handlers* 
   ([exn:break? (lambda (e)
                  (for ([sp processes])
                       (when (equal? (subprocess-status sp) 'running)
                             (subprocess-kill sp #f)))
                  (sleep 5)
                  )])
   (wait)
   (update-stats)
   )
  
  (define id (get-stats))
  (define output-code (ast-from-file (format "~a-~a.best" path id)))
  (if output-code
      (let ([decompressed-code (decompress-reg-space output-code map-back)])
        (print-syntax decompressed-code #:LR #f)
        decompressed-code)
      code-org)
  )

(define (optimize-filter code-org live-out synthesize dir cores time-limit)
  (define-values (pass start stop extra-live-out) (select-code code-org))
  (pretty-display `(select-code ,pass ,start ,stop ,extra-live-out))
  (if pass
      (let ([middle-output
             (optimize-inner 
              pass
              (combine-live-out live-out extra-live-out)
              synthesize dir cores time-limit)])
        (combine-code code-org middle-output start stop))
      code-org)
  )

;; Inputs
;; input: can be in 3 different types.
;;   1) parsed AST, 2) .s file, 3) LLVM IR file
;; live-out: live-out info in custom format--- for vpe, a list of live registers
;;   synthesize: #t = synthesize mode, #f = optimize mode
(define (optimize input live-out synthesize 
                  #:is-file [is-file #t] 
                  #:is-llvm [is-llvm #f]
                  #:need-filter [need-filter #f]
                  #:dir [dir "output"] 
                  #:cores [cores 12]
                  #:time-limit [time-limit 36000])
  (define code-org 
    (if is-file
        (if is-llvm
            (ast-from-file-llvm input)
            (ast-from-file input))
        input))

  (if need-filter
      (optimize-filter code-org live-out synthesize dir cores time-limit)
      (optimize-inner code-org live-out synthesize dir cores time-limit)))

;; (optimize "vpe/programs/ntt.ll"
;;           (list 2 3 7 8 26 27 28)
;;           #f #:dir "output" #:cores 12)
          




                       
