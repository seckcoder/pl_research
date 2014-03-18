#lang racket

(require "nfa.rkt")

(define (make-dfa reg)
  (match (merge-nfa (make-nfa reg))
    [(list start end trans)
     (
