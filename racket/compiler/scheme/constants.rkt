#lang racket

(require "global.rkt"
         "base.rkt")

(provide (rename-out [lift lift-constant]))

(define (lift e)
  (match e
    [(? immediate?) e]
    [(? symbol?) e]
    [`(quote ,v)
      (let ([n (gensym 'c)])
        (add-global! n v)
        `(constant-ref
           ,n))]
    [(list (? prim-op? op) v* ...)
     `(,op ,@(map lift v*))]
    [`(if ,test ,then ,else)
      `(if ,(lift test)
         ,(lift then)
         ,(lift else))]
    [`(let ((,v* ,e*) ...) ,body)
      `(let ,(map list v* (map lift e*))
         ,(lift body))]
    [`(begin ,exp* ...)
      `(begin
         ,@(map lift exp*))]
    [`(labels ((,v* ,e*) ...) ,exp)
      ; for debugging purpose
      e]
    [`(lambda (,v* ...) ,body)
      `(lambda ,v*
         ,(lift body))]
    [`(app ,rator ,rand* ...)
      (let ([rator (lift rator)]
            [rand* (map lift rand*)])
        `(app ,rator ,@rand*))]
    [`(app-proc ,rator ,rand* ...)
      ; for debugging purpose
      e]
    [_
      (error 'lift-constant
             "~a not match" e)]))


(module+ test
  (define (test-lift-constant e)
    (let ([e (lift e)])
      `(labels ,*global*
         ,e)))
  (test-lift-constant
    `(let ((f (lambda () '(1 . 2))))
       (= (app f) (app f))))
  )
