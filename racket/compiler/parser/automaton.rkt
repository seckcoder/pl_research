#lang racket


(require "nfa.rkt"
         "dfa.rkt")

(provide make-automaton)

(define (make-automaton reg type)
  (case type
    [(dfa)
     (make-dfa-merged reg)]
    [(nfa)
     (make-nfa-merged reg)]
    [else
      (error 'make-automaton "wrong type")]
    ))

(define (match-automaton auto str idx k)
  ((auto 'match) str idx k))
