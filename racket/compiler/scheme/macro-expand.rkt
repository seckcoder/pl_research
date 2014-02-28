#lang racket

; macro expander
(require "base.rkt")

(provide (rename-out [expand macro-expand]))
(define (expand x)
  (match x
    [`(let* () ,body ...)
      (expand `(let () ,@body))]
    [`(let* ([,v0 ,e0]) ,body ...)
      (expand `(let ([,v0 ,(expand e0)]) ,@body))]
    [`(let* ([,v0 ,e0]
             ,bind* ...) ,body ...)
      `(let ([,v0 ,e0])
         ,(expand `(let* ,bind* ,@body)))]
    [(? immediate?) x]
    [(? string?) x]
    [(? symbol?) x]
    [`(quote ,v)
      x]
    [`(make-vector ,v* ...)
      (expand `(make-vec ,@v*))]
    [`(vector ,v* ...)
      (expand `(vec ,@v*))]
    [`(vector-set! ,v* ...)
      (expand `(vec-set! ,@v*))]
    [`(vector-ref ,v* ...)
      (expand `(vec-ref ,@v*))]
    [`(make-vec)
      (expand `(make-vec 0))]
    [`(make-string)
      (expand `(make-string 0))]
    [`(set! ,v ,val)
      `(set! ,v ,(expand val))]
    [`(fxadd1 ,v)
      (expand `(fx+ ,v 1))]
    [`(add1 ,v)
      (expand `(+ ,v 1))]
    [`(fxsub1 ,v)
      (expand `(fx- ,v 1))]
    [`(sub1 ,v)
      (expand `(- ,v 1))]
    [`(fxzero? ,v)
      (expand `(zero? ,v))]
    [`(fixnum? ,v)
      (expand `(number? ,v))]
    [(list (? prim-op? op) v* ...)
     `(,op ,@(map expand v*))]
    [`(cond (,pred* ,body*) ...)
      (if (not (null? pred*))
        (let ((pred (car pred*))
              (body (car body*)))
          (cond ((and (eq? pred 'else)
                      (null? (cdr pred*)))
                 (expand body))
                ((eq? pred 'else)
                 (error 'expand "cond else should be the last expression"))
                (else
                  (expand `(if ,pred
                            ,body
                            (cond ,@(map (lambda (pred body)
                                           (list pred body))
                                         (cdr pred*)
                                         (cdr body*))))))))
        (error 'expand "empty cond"))]
    [`(if ,test ,then ,else)
      `(if ,(expand test)
         ,(expand then)
         ,(expand else))]
    [`(let ([,v* ,e*] ...) ,body* ...)
      `(let ,(map list v* (map expand e*))
         ,(if (null? (cdr body*))
            (expand (car body*))
            (expand `(begin ,@body*))))]
    [`(lambda (,v* ...) ,body* ...)
      `(lambda ,v*
         ,(if (null? (cdr body*))
            (expand (car body*))
            (expand `(begin ,@body*))))]
    [`(begin ,exp0 ,exp* ...)
      `(begin
         ,@(map expand (cons exp0 exp*)))]
    [`(app-proc ,rator ,rand* ...)
      ; procedure call, for debug purpose
      x]
    [`(labels ((,v* ,e*) ...) ,exp)
      ; again, for debug purpose
      x]
    [`(proc (,v* ...) ,body)
      ; procedure, for debug purpose
      x]
    [`(app ,rator ,rand* ...)
      ; func call, for debug purpose
      x]
    [`(,rator ,rand* ...)
      ; function call
      `(app ,(expand rator)
            ,@(map expand rand*))]
    [_ (error 'expand "failed:~s" x)]))

(module+ test
  (expand '(let ([f (lambda (x) 
                     (set! x (fxadd1 x))
                     x)])
            (f 12)))
  (expand '(lambda (x)
            (set! x (fxadd1 x))
            x)))
