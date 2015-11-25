#lang racket

(require "../forwardbackward.rkt" "../ast.rkt")

(provide llvm-demo-forwardbackward%)

(define llvm-demo-forwardbackward%
  (class forwardbackward%
    (super-new)
    (inherit-field machine printer)
    (override len-limit
              change-inst change-inst-list)

    ;; Num of instructions that can be synthesized within a minute.
    (define (len-limit) 2)
    
    (define (change-inst x change)
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

    
    (define (change-inst-list x change)
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

    ))
    
    
