#lang racket

(require "../forwardbackward.rkt" "../ast.rkt" "../ops-racket.rkt"
         "llvm-demo-machine.rkt"
         "llvm-demo-simulator-racket.rkt" "llvm-demo-simulator-rosette.rkt"
         "llvm-demo-validator.rkt"
         "llvm-demo-enumerative.rkt" "llvm-demo-inverse.rkt")

(provide llvm-demo-forwardbackward%)

(define llvm-demo-forwardbackward%
  (class forwardbackward%
    (super-new)
    (inherit-field machine printer
                   enum inverse simulator-abst validator-abst)
    (override len-limit window-size
              reduce-precision increase-precision)

    ;; Num of instructions that can be synthesized within a minute.
    (define (len-limit) 2)

    ;; Context-aware window decomposition size L.
    ;; The cooperative search tries L/2, L, 2L, 4L.
    (define (window-size) 4)

    ;; Actual bitwidth
    (define bit-precise (get-field bit machine))
    ;; Reduce bitwidth
    (define bit 4)
    
    ;; Initlized required fields.
    (let ([machine-abst (new llvm-demo-machine% [bit bit]
                             [config (send machine get-config)])])
      (set! simulator-abst (new llvm-demo-simulator-racket% [machine machine-abst]))
      (set! validator-abst (new llvm-demo-validator% [machine machine-abst] [printer printer] [simulator (new llvm-demo-simulator-rosette% [machine machine-abst])]))
      (set! inverse (new llvm-demo-inverse% [machine machine-abst] [simulator simulator-abst]))
      (set! enum (new llvm-demo-enumerative% [machine machine-abst] [printer printer]))
      ;; Set machine to reduced-bidwith machine.
      (set! machine machine-abst))
    
    (define max-val (arithmetic-shift 1 bit))
    (define mask (sub1 (arithmetic-shift 1 bit)))
    (define mask-1 (sub1 (arithmetic-shift 1 (sub1 bit))))
    (define inst-id (get-field inst-id machine))
    
    (define (reduce-inst x change)
      (define opcode-name (send machine get-inst-name (inst-op x)))
      (define args (inst-args x))
      (define types (send machine get-arg-types opcode-name))
      
      (define new-args
        (for/vector
         ([arg args]
          [type types])
         (if (member type '(const bit))
             (change arg type)
             arg)))

      (inst (inst-op x) new-args))

    
    (define (reduce-inst-list x change)
      (define op (inst-op x))
      (define opcode-name (send machine get-inst-name op))
      (define args (inst-args x))
      (define types (send machine get-arg-types opcode-name))
      
      (define new-args
        (for/list
         ([arg args]
          [type types])
         (if (member type '(const bit))
             (change arg type)
             (list arg))))

      (define ret (list))
      (define (recurse args-list args-final)
        (cond
         [(empty? args-list)
          (set! ret (cons (inst op (list->vector args-final)) ret))]

         [else
          (for ([x (car args-list)])
               (recurse (cdr args-list) (cons x args-final)))]))

      (recurse (reverse new-args) (list))
      ret)
    
    ;; Convert input program into reduced-bitwidth program by replacing constants.
    ;; output: a pair of (reduced-bitwidth program, replacement map*)
    ;;   *replacement map maps reduced-bitwidth constants to sets of actual constants.
    (define (reduce-precision prog)
      ;; TODO: common one
      (define mapping (make-hash))
      (define (change arg type)
        (define (inner)
          (cond
           [(equal? type `bit)
            (cond
             [(and (> arg 0) (<= arg (/ bit-precise 4)))
              (/ bit 4)]
             [(and (> arg (/ bit-precise 4)) (< arg (* 3 (/ bit-precise 4))))
              (/ bit 2)]
             [(and (>= arg (* 3 (/ bit-precise 4))) (< arg bit-precise))
              (* 3 (/ bit 4))]
             [(= arg bit-precise) bit]
             [(> arg 0) (bitwise-and arg mask-1)]
             [else (finitize (bitwise-and arg mask) bit)])]

           [(> arg 0) (bitwise-and arg mask-1)]
           [else (finitize (bitwise-and arg mask) bit)]))

        (define ret (inner))
        (if (hash-has-key? mapping ret)
            (let ([val (hash-ref mapping ret)])
              (unless (member arg val)
                      (hash-set! mapping ret (cons arg val))))
            (hash-set! mapping ret (list arg)))
        ret)
        
      (cons (for/vector ([x prog]) (reduce-inst x change)) mapping))
    
    ;; Convert reduced-bitwidth program into program in precise domain.
    ;; prog: reduced bitwidth program
    ;; mapping: replacement map returned from 'reduce-precision' function
    ;; output: a list of potential programs in precise domain
    (define (increase-precision prog mapping)
      (define (change arg type)
        (define (finalize x)
          (if (hash-has-key? mapping arg)
              (let ([val (hash-ref mapping arg)])
                (if (member x val) val (cons x val)))
              (list x)))
        
        (cond
         [(= arg bit) (finalize bit-precise)]
         [(= arg (sub1 bit)) (finalize (sub1 bit-precise))]
         [(= arg (/ bit 2)) (finalize (/ bit-precise 2))]
         [else (finalize arg)]))

      (define ret (list))
      (define (recurse lst final)
        (if (empty? lst)
            (set! ret (cons (list->vector final) ret))
            (for ([x (car lst)])
                 (recurse (cdr lst) (cons x final)))))
      
      (recurse (reverse (for/list ([x prog]) (reduce-inst-list x change)))
               (list))
      ret)

    ))
    
    
