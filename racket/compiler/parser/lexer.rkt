#lang racket

(require "automaton.rkt")

; expand regular expression in a spec
(define (expand-reg reg)
  (cond
    [(null? reg)
     (error 'nfa "empty regular expression")]
    [(null? (cdr reg)) (car reg)]
    [else (cons 'concat reg)]))

(define (make-automatons-from-specs specs)
  (map
    (match-lambda
      [`(,name ,reg ,action)
        (list name (make-automaton (expand-reg reg) 'dfa) action)])
    specs))

(define (make-lexer specs)
  (lambda (prog-str)
    (let ([specs (make-automatons-from-specs specs)])
    (let loop ([cur 0]
               [tokens '()])
      (if (>= cur (string-length prog-str))
        (reverse tokens) ; token is consed
        (letrec
          ([match-specs (lambda (specs k)
                          (cond
                            [(null? specs)
                             (error 'lexer "program is not matched at:~a" cur)]
                            [else
                              (match (car specs)
                                [`(,name ,auto ,action)
                                  (match-automaton
                                    auto
                                    prog-str
                                    cur
                                    (lambda (matched? new-cur str)
                                      (if matched?
                                        (k action new-cur (a-token name str))
                                        (match-specs (cdr specs) k))))])]))])
          (match-specs
            specs
            (lambda (action new-cur new-token)
              (case action
                [(skip)
                 (loop new-cur tokens)]
                [else
                  (loop new-cur
                        (cons new-token tokens))])))))))))
