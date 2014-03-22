#lang racket


(require "nfa.rkt"
         "dfa.rkt")

(provide make-automaton
         match-automaton
         FullMatch
         PartialMatch
         NotMatch)

(define (make-automaton reg type)
  (case type
    [(dfa)
     (make-dfa reg)]
    [(nfa)
     (make-nfa reg)]
    [else
      (error 'make-automaton "wrong type")]
    ))

; currently only support dfa
(define (match-automaton auto str idx cont)
  (match-dfa auto str idx cont))
