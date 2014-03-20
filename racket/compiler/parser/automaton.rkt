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

(define (match-automaton auto str idx cont)
  ;(printf "match-automaton")(newline)
  ;(printf "match-automaton ~a" k)(newline)
  ;(+ 1 2)
  (match-dfa auto str idx cont))
