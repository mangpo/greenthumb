#lang racket

(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc
	 "../parser.rkt" "../inst.rkt")

(provide llvm-parser%)

(define llvm-parser%
  (class parser%
    (super-new)
    (inherit-field asm-parser asm-lexer)

    (define-tokens a (VAR WORD NUM ITYPE))
    (define-empty-tokens b (EOF EQ COMMA HOLE LT GT X STAR))

    (define-lex-abbrevs
      (digit10 (char-range "0" "9"))
      (number10 (number digit10))
      (snumber10 (re-or number10 (re-seq "-" number10)))

      (identifier-characters (re-or (char-range "A" "Z") (char-range "a" "z")))
      (identifier-characters-ext (re-or digit10 identifier-characters "_" "*"))
      (identifier (re-seq identifier-characters 
                          (re-* identifier-characters-ext)))

      (var   (re-: "%" (re-+ (re-or identifier-characters digit10))))
      (itype (re-: "i" (re-+ digit10) (re-? "*")))
      
      )

    (set! asm-lexer
      (lexer-src-pos
       (","        (token-COMMA))
       ("="        (token-EQ))
       ("<"        (token-LT))
       (">"        (token-GT))
       ("x"        (token-X))
       ("?"        (token-HOLE))
       ("*"        (token-STAR))
       (snumber10  (token-NUM lexeme))
       (var        (token-VAR lexeme))
       (itype      (token-ITYPE lexeme))
       (identifier (token-WORD lexeme))
       (whitespace   (position-token-token (asm-lexer input-port)))
       ((eof) (token-EOF))))

    
    (set! asm-parser
      (parser
       (start code)
       (end EOF)
       (error
        (lambda (tok-ok? tok-name tok-value start-pos end-pos)
          (raise-syntax-error 'parser
                              (format "syntax error at '~a' in src l:~a c:~a"
                                      tok-name
                                      (position-line start-pos)
                                      (position-col start-pos)))))
       (tokens a b)
       (src-pos)
       (grammar
        (words
         ((WORD) (list $1))
         ((WORD words) (cons $1 $2)))

        (type-num
         ((ITYPE NUM) (cons $1 $2)))

        (type-num-list
         ((type-num) (list $1))
         ((type-num COMMA type-num-list) (cons $1 $3)))
        
        (arg
         ((VAR) $1)
         ((NUM) $1)
         ((LT type-num-list GT) $2))

        (arg-list
         ((arg) (list $1))
         ((arg COMMA arg-list) (cons $1 $3)))

        (base-type
         ((ITYPE) (convert-type $1))
         ((LT NUM X ITYPE GT) (cons $2 $4)))

        (type
         ((base-type) $1)
         ((base-type STAR) (pointer $1)))

        (type-arg
         ((type arg) (cons $1 $2)))

        (type-arg-list
         ((type-arg) $1)
         ((type-arg COMMA type-arg-list) (cons $1 $3)))
        
        (instruction
         ((HOLE)         (inst #f #f))
         ((WORD type arg COMMA type arg) (convert2store $1 $2 $3 $5 $6))
         ((VAR EQ words type COMMA type arg) (convert2load $3 $4 $1 $6 $7))
         ((VAR EQ words type arg-list) (convert2inst $3 $4 $1 $5))
         ((VAR EQ words type-arg-list) (convert2inst $3 $1 $4))
         ;; <result> = shufflevector <4 x i32> %v1, <4 x i32> %v2, <4 x i32> <i32 0, i32 4, i32 1, i32 5>
         )
        
        (inst-list 
         (() (list))
         ((instruction inst-list) (cons $1 $2)))
        
        (code
         ((inst-list) (list->vector $1)))
       )))

    (define/override (info-from-file file)
      (define lines (file->lines file))
      (define live-out-var (string-split (first lines) ","))
      (define live-out-vec4 (string-split (second lines) ","))
      (vector live-out-var live-out-vec4 #t))

    ))

(define (convert-type t)
  (define len (string-length t))
  (if (equal? "*" (substring t (sub1 len) len))
      (pointer (substring t 0 (sub1 len)))
      t))

(define (compress-vector-args args type-ref)
  (for/list
    ([arg args])
    (cond
     [(string? arg) arg]
     [(list? arg)
      (for/vector ([type-num arg])
                  (let ([type (car type-num)]
                        [num (cdr type-num)])
                    ;; check type
                    (unless (equal? type-ref type)
                            (raise (format "Type mismatch at constant vector ~a." arg)))
                    ;; only keep number
                    num))]
     [else (raise (format "Cannot parse operand ~a." arg))])))

(define (convert2inst ops type lhs args)
  (define op
    (if (equal? (first ops) "icmp")
	(second ops)
	(first ops))) ;; TODO: currently ignore terms like nuw

  (cond
   [(equal? type "i32") (inst op (list->vector (cons lhs args)))]
   [(and (pair? type) (equal? (cdr type) "i32"))   
    (inst (format "~a_v~a" op (car type))
          (list->vector (cons lhs (compress-vector-args args "i32"))))]
   [else (raise "Currently only support i32 type.")]))

(define (convert2inst-type-arg ops lhs type-arg-list)
  
  (define op
    (if (equal? (first ops) "icmp")
	(second ops)
	(first ops))) ;; TODO: currently ignore terms like nuw

  (define types
    (for/list
     ([type-arg type-arg-list])
     (let ([type (car type-arg)])
       (cond
        [(equal? type "i32") "s"]
        [(and (pair? type) (equal? (cdr type "i32"))) (format "v~a" (car type))]
        [else (raise "Currently only support i32 type.")]))))
  
  (inst (format "~a_~a" op (string-join "_" types))
        (list->vector (cons lhs (map cdr type-arg-list)))))

(define (convert2store op val-type val-arg p-type p-arg)
  (unless (equal? val-type "i32")
	  (raise "Currently only support i32 type."))
  (unless (pointer-type? p-type "i32")
	  (raise "Currently only support i32 type."))

  (inst op (vector val-arg p-arg)))

(define (convert2load ops dest-type dest-arg p-type p-arg)
  (unless (equal? dest-type "i32")
	  (raise "Currently only support i32 type."))
  (unless (pointer-type? p-type "i32")
	  (raise "Currently only support i32 type."))

  (inst (car ops) (vector dest-arg p-arg)))

;; (define all-names (set))      

;; (define (rename inst-vec no-rename)
;;   ;; Rename variable from old to new up to instruction at index.
;;   (define (rename-var old new index)
;;     (define def #f)
;;     (for ([i (reverse (range index))] #:break def)
;; 	 (let* ([my-inst (vector-ref inst-vec i)]
;; 		[args (inst-args my-inst)])
;; 	   (for ([arg args]
;; 		 [arg-i (in-naturals)] #:break def)
;; 		(when (equal? arg old)
;; 		      (vector-set! args arg-i new)
;; 		      (when (= arg-i 0) (set! def #t)))))))

;;   ;; Check if arg is an input variable to the program.
;;   (define (is-input? arg index)
;;     (define def #f)
;;     (for ([i index] #:break def)
;;          (let* ([my-inst (vector-ref inst-vec i)]
;;                 [args (inst-args my-inst)])
;;            (when (equal? arg (vector-ref args 0))
;;                  (set! def #t))))
;;     (not def))

;;   (define (exclude? arg)
;;     (or (member arg no-rename) (member (string->symbol arg) no-rename)))
  
;;   (define len (vector-length inst-vec))
;;   (define names (list))
;;   (for ([index (reverse (range len))])
;;        (let ([my-inst (vector-ref inst-vec index)])
;;          (when
;;           (inst-op my-inst)
;;           (let* ([args (inst-args my-inst)]
;;                  [arg-o (vector-ref args 0)])
;;             (unless (member args names) (set! names (cons arg-o names)))

;; 	    ;; remove from names first
;;             (for ([i (vector-length args)])
;;                  (let ([arg (vector-ref args i)])
;;                    (cond
;;                     [(= i 0)
;;                      (unless (set-member? all-names arg)
;;                              (set! all-names (set-add all-names arg)))]

;; 		    [(set-member? all-names arg)
;; 		     (set! names (remove arg names))])))

;; 	    ;; rename intermediates
;;             (for ([i (vector-length args)])
;;                  (let ([arg (vector-ref args i)])
;;                    (cond
;;                     [(or (= i 0) ;; output of this inst
;;                          (vector? arg) ;; constant vector
;; 			 (not (equal? (substring arg 0 1) "%")) ;; constant
;;                          (set-member? all-names arg) ;; name is in the set
;;                          ;;(use-after arg i)
;;                          (is-input? arg index) ;; input to the program
;;                          (exclude? arg) ;; in no-rename list
;;                          )
;;                      (void)] ;; don't rename
                    
;;                     [(empty? names)
;;                      (set! all-names (set-add all-names arg))]
                    
;;                     [else
;;                      (rename-var arg (car names) (add1 index))
;;                      (set! names (cdr names))]))))
;;           )))

;;   inst-vec)
