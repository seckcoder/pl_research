#lang racket

; a temporary implementation of parser
(require "base.rkt")

(provide parse)
(define (parse x)
  (match x
    [`(let* () ,body ...)
      (parse `(let () ,@body))]
    [`(let* ([,v0 ,e0]) ,body ...)
      (parse `(let ([,v0 ,(parse e0)]) ,@body))]
    [`(let* ([,v0 ,e0]
             ,bind* ...) ,body ...)
      `(let ([,v0 ,e0])
         ,(parse `(let* ,bind* ,@body)))]
    [(? immediate?) x]
    [(? string?) x]
    [(? symbol?) x]
    [`(quote ,v)
      x]
    [`(make-vector ,v* ...)
      (parse `(make-vec ,@v*))]
    [`(vector ,v* ...)
      (parse `(vec ,@v*))]
    [`(vector-set! ,v* ...)
      (parse `(vec-set! ,@v*))]
    [`(vector-ref ,v* ...)
      (parse `(vec-ref ,@v*))]
    [`(make-vec)
      (parse `(make-vec 0))]
    [`(make-string)
      (parse `(make-string 0))]
    [`(set! ,v ,val)
      `(set! ,v ,(parse val))]
    [`(fxadd1 ,v)
      (parse `(fx+ ,v 1))]
    [`(add1 ,v)
      (parse `(+ ,v 1))]
    [`(fxsub1 ,v)
      (parse `(fx- ,v 1))]
    [`(sub1 ,v)
      (parse `(- ,v 1))]
    [`(fxzero? ,v)
      (parse `(zero? ,v))]
    [`(fixnum? ,v)
      (parse `(number? ,v))]
    [(list (? prim-op? op) v* ...)
     `(,op ,@(map parse v*))]
    [`(cond (,pred* ,body*) ...)
      (if (not (null? pred*))
        (let ((pred (car pred*))
              (body (car body*)))
          (cond ((and (eq? pred 'else)
                      (null? (cdr pred*)))
                 (parse body))
                ((eq? pred 'else)
                 (error 'parse "cond else should be the last expression"))
                (else
                  (parse `(if ,pred
                            ,body
                            (cond ,@(map (lambda (pred body)
                                           (list pred body))
                                         (cdr pred*)
                                         (cdr body*))))))))
        (error 'parse "empty cond"))]
    [`(if ,test ,then ,else)
      `(if ,(parse test)
         ,(parse then)
         ,(parse else))]
    [`(let ([,v* ,e*] ...) ,body* ...)
      `(let ,(map list v* (map parse e*))
         ,(if (null? (cdr body*))
            (parse (car body*))
            (parse `(begin ,@body*))))]
    [`(lambda (,v* ...) ,body* ...)
      `(lambda ,v*
         ,(if (null? (cdr body*))
            (parse (car body*))
            (parse `(begin ,@body*))))]
    [`(begin ,exp0 ,exp* ...)
      `(begin
         ,@(map parse (cons exp0 exp*)))]
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
      `(app ,(parse rator)
            ,@(map parse rand*))]
    [_ (error 'parse "failed:~s" x)]))

(module+ test
  (parse '(let ([f (lambda (x) 
                     (set! x (fxadd1 x))
                     x)])
            (f 12)))
  (parse '(lambda (x)
            (set! x (fxadd1 x))
            x)))
