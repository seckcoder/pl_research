#lang racket

(provide (all-defined-out))

(define *global* '())

(define (init-global!)
  (set! *global* '()))

(define (add-global! n code)
  (set! *global* (cons (list n code)
                       *global*)))

(define (rep-global* k)
  `(labels ,*global*
     ,(k)))
