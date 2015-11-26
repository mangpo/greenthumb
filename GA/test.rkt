#lang racket

(require "../controller.rkt" "../inst.rkt" 
	 "state.rkt" "interpret.rkt" "compress.rkt" "print.rkt")

(define-syntax-rule (timeout sec expr)
  (let* ([t (let ([parent (current-thread)])
              (thread
               (thunk
                (thread-send 
                 parent
                 (with-handlers [(exn? identity)]
                   expr)))))]
         [out (sync/timeout sec t)])
    (cond [out  (thread-receive)]
          [else (break-thread t)
                (raise (thread-receive))])))

(define t (current-seconds))

;; (define-values (output cost)
;; (superoptimize (encode 
;;                 (list
;;                  (block "dup drop 1" "dup drop 1" #f)
;;                  (forloop
;;                   (list)
;;                   (list (block "2*" "2*" #f))
;;                   2
;;                   )))
;;                (encode 
;;                 (list
;;                  (block "_ _ _" "dup drop 1" #f)
;;                  (forloop
;;                   (list)
;;                   (list (block "_" "2*" #f))
;;                   2
;;                   )))
;;                (syninfo 0 0 #f)
;;                (constraint t memory))

;; (decompress (list
;;              (block "1" "dup drop 1" #f)
;;              (forloop
;;               (list)
;;               (list (block "2*" "2*" #f))
;;               2
;;               ))
;;             (syninfo 0 0 #f)
;;             (constraint r s t)
;;             (no-assumption)
;;             program-eq?)

;; (program-eq? (encode "a! over or dup a and or or") (encode "- push over or pop and or")
;;              (syninfo 0 0 #f) (constraint t)
;;              #:assume (constrain-stack '((<= . 65535) (<= . 65535) (<= . 65535))))

;; (print-struct
;; (binary-search (block "1 65536 2* +" #f #f)
;;                (syninfo 0 0 #f)
;;                (constraint t)
;; 	       #:prefix (list (block "-131071" #f #f))))

;; (print-struct
;; (linear-search (encode (list (block "1 65536 2* +" #f #f)))
;;                (encode (list (block "_ _ _ _" #f #f)))
;;                (syninfo 0 0 #f)
;;                (constraint t)
;; 	       #:prefix (encode (list (block "-131071" #f #f)))))

;; (print-struct
;; (linear-search (encode 
;;                       (list 
;;                        (-iftf 
;;                         (list 
;;                          (block
;;                           "drop dup 16 - 1 + + push drop pop"
;;                           "drop dup 16 - 1 + + push drop pop"
;;                           #f)
;;                          )
;;                         (list 
;;                          (block
;;                           "drop"
;;                           "drop"
;;                           (blockinfo '((data . -1) (return . 0) memory ) 0))
;;                          ))))
;;                (encode 
;;                       (list 
;;                        (-iftf 
;;                         (list 
;;                          (block
;;                           "_ _ _ _ _ _ _ _ _ _"
;;                           "drop dup 16 - 1 + + push drop pop"
;;                           #f)
;;                          )
;;                         (list 
;;                          (block
;;                           "_"
;;                           "drop"
;;                           #f)))))
;;                (syninfo 0 0 #f)
;;                (constraint t)))

;; (linear-search (encode "1 2 3")
;;                (encode "_ _ _")
;;                (syninfo 5 0 #f)
;;                (constraint [data 2] s t))
;; (binary-search (block "1 2 3" #f #f)
;;                (syninfo 5 0 #f)
;;                (constraint [data 2] s t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (superoptimize (encode "325 b! @b push drop pop 325 b! @b 0 b! !b dup 0 b! @b or over 0 b! @b and or")
;;                (encode "_ _ _ _ _ _ _ _ _ _ _ _ _ _")
;;                (cons 1 2)
;;                (constraint r s t))

;;;;;;;;;;;;;;; test cases ;;;;;;;;;;;;;;;;

;; can't init with all 0
;; (superoptimize (encode "0 b! @b -5 + b! @b")
;;                (encode "0 b! @b _ + b! @b")
;;                (cons 5 0)
;;                (constraint t))

;; assume interpret-spec
;; (superoptimize (encode "b! @b")
;;                (encode "_ _")
;;                (cons 5 0)
;;                (constraint t))

;; (superoptimize (encode "65536 2*")
;;                (encode "_")
;;                (cons 0 0)
;;                (constraint [data 1] s t))

;; (print-program
;; (superoptimize (encode (list (block "-3" "-3" #f) (-iftf "1" "2")))
;;                (encode (list (block "_" "_" #f) (-iftf "1" "2")))
;;                (cons 0 0)
;;                (constraint t)))

;;;;;;;;;;;;;;;; fixed prefix ;;;;;;;;;;;;;;

;; (superoptimize (encode "a 277 b! dup or a! @+ !b @+ !b @+
;; 277 a! ! 3 b! @b ! 0 b! @b !
;; 277 b! @b 0 b! !b 1 b! @b 277 b! !b 277 b! @b 1")
;;                (encode "a 277 b! dup or a! @+ !b @+ !b @+
;; 277 a! ! 3 b! @b ! 0 b! @b !
;; _ _ _ _ _ _ _ _")
;;                (cons 4 2)
;;                (constraint memory s t))

;;;;;;;;;;;;;;;; assume ;;;;;;;;;;;;;;;;;;

;; (superoptimize (encode "0 a! !+ push !+ pop dup 1 b! @b 0 b! @b 65535 or over - and + or push drop pop")
;;                (encode "_ _ _ _ _ _ _ _ _")
;;                ;;(encode "65535 or push over - pop and or or")
;;                (syninfo 2 0 #f)
;;                (constraint s t)
;;                #:assume (constrain-stack '((<= . 65535) (<= . 65535) (<= . 65535))))

;;;;;;;;;;;;;;;; no comm ;;;;;;;;;;;;;;;;;;
(superoptimize (encode "2 b! @b 3 b! !b 1 b! @b 2 b! !b")
               (encode "_ _ _ _ _ _ _ _")
               (syninfo 4 0 #f)
               (constraint memory s t)) ; 19, 15, 27
;; (superoptimize (encode "0 a! !+ !+ !+ !+ 3 b! @b 1 b! @b")
;;                (encode "_ _ _ _ _ _ _ _ _ _")
;;                (syninfo 4 0 #f)
;;                (constraint [data 1] memory s t)) ; (121 125), (99 18 39), (140 126)

;;;;;;;;;;;;;;;; communication ;;;;;;;;;;;;;;;;;;;
;; (superoptimize (encode "325 b! !b 277 b! !b 373 b! !b 469 b! !b")
;;                (encode "_ _ _ _ _ _ _ _ _ _ _ _")
;;                (cons 0 0)
;;                (constraint memory s t))
;; (superoptimize (encode "2 b! @b 277 b! !b 1 b! @b 277 b! !b")
;;                (encode "_ _ _ _ _ _ _ _")
;;                (cons 3 0)
;;                (constraint memory s t))
;; (superoptimize (encode "4 a! !+ 4 b! @b 373 b! @b +")
;;                (encode "_ _ _ _ _ _ _ _")
;;                (cons 5 1)
;;                (constraint memory s t))
;; (superoptimize (encode "5 b! !b 373 b! @b 5 b! @b 277 b! !b")
;;                (encode "_ _ _ _ _ _ _ _ _ _")
;;                (syninfo 6 1 #f)
;;                (constraint memory s t)) ; (22 23), (74 27 68)
(pretty-display `(time ,(- (current-seconds) t)))
