#lang racket

(require (prefix-in env!: "mutable-env.rkt"))
(provide (all-defined-out))

(define *proc* '())
(define (init-global!)
  (set! *proc* '()))
(define (add-global-proc! n code)
  ;(env!:ext! *global* n proc-n)
  (set! *proc* (cons (list n code)
                     *proc*)))
