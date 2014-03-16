#lang racket

(require (prefix-in env: "../compiler/scheme/env.rkt"))

(provide init-class-env!
         ext-class-env!
         app-class-env)

(define class-env (env:empty))

(define (init-class-env!)
  (set! class-env (env:empty))
  )

(define (ext-class-env! class-name cls)
  (set! class-env
    (env:ext class-env class-name cls)))

(define (app-class-env class-name)
  (env:app class-env class-name))
