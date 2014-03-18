#lang racket


; expand regular expression in a spec
(define (expand-reg reg)
  (cond
    [(null? reg)
     (error 'nfa "empty regular expression")]
    [(null? (cdr reg)) (car reg)]
    [else (cons 'concat reg)]))



