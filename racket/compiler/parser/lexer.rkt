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

(struct Token (name str) #:transparent)

; create lexer from specs
(define (make-lexer specs)
  (let ([specs (make-automatons-from-specs specs)])
    (lambda (prog-str)
      (let loop ([next 0]
                 [tokens '()])
        (if (>= next (string-length prog-str))
          (reverse tokens) ; token is consed
          (letrec
            ([match-specs (lambda (specs k)
                            ; There is a problem with this implementation,
                            ; we should match all specs with the longgest match
                            ; selected instead of the first.
                            (cond
                              [(null? specs)
                               (error 'lexer "program is not matched at:~a" next)]
                              [else
                                (match (car specs)
                                  [`(,name ,auto ,action)
                                    (match-automaton
                                      auto
                                      prog-str
                                      next
                                      (lambda (res)
                                        (printf "match automaton result:~a" res)(newline)
                                        (match res
                                          [(FullMatch matched-str)
                                           ; full match, the match finished
                                           (k action (string-length prog-str) (Token name matched-str))]
                                          [(PartialMatch matched-str next)
                                           (k action next (Token name matched-str))]
                                          [(NotMatch next)
                                           (match-specs (cdr specs) k)])))])]))])
            (match-specs
              specs
              (lambda (action new-cur new-token)
                ;(printf "~a ~a ~a\n" action new-token new-cur)
                (case action
                  [(skip)
                   (loop new-cur tokens)]
                  [else
                    (loop new-cur
                          (cons new-token tokens))])))))))))

(module+ test
  (require "lex-demos.rkt")
  (let ([lexer (make-lexer general-spec)])
    (lexer "a + b")))
