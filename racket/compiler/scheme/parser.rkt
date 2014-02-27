#lang racket

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
    [`(make-vec)
      (parse `(make-vec 0))]
    [`(make-string)
      (parse `(make-string 0))]
    [(list (? prim-op? op) v* ...)
     `(,op ,@(map parse v*))]
    [`(if ,test ,then ,else)
      `(if ,(parse test)
         ,(parse then)
         ,(parse else))]
    [`(let ([,v* ,e*] ...) ,body* ...)
      `(let ,(map list v* (map parse e*))
         ,(if (null? (cdr body*))
            (parse (car body*))
            (parse `(begin ,@body*))))]
    [`(lambda (,v* ...) ,body)
      `(lambda ,v*
         ,(parse body))]
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
