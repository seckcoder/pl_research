#lang racket

(require "nfa.rkt")

(provide make-dfa
         FullMatch
         PartialMatch
         NotMatch
         match-dfa)
; end of str and we are in end state.
; s is the matched str.
(struct FullMatch (s) #:transparent)
; next char can't be matched, but we are in end state
(struct PartialMatch (s next) #:transparent)
; the final state can't be accepted
(struct NotMatch (next))

; match a dfa to a str, start from the next pos
(define (match-dfa the-dfa str next cont)
  ;(printf "match-dfa:~a" cont)
  (match the-dfa
    [`(,start ,ends ,dfa-trans)
      (letrec
        ([is-end-state? (lambda (state)
                          (memq state ends))]
         [end-of-str? (lambda (next)
                        (>= next (string-length str)))]
         [string-reverse-append
           (lambda (matched-strs)
             (apply
               string-append
               ; matched-strs is consed
               (reverse matched-strs)))])
        (let loop ([cur-state start]
                   [next next]
                   [matched-strs '()])
          (cond
            [(and (end-of-str? next)
                  (is-end-state? cur-state))
             (cont (FullMatch (string-reverse-append matched-strs)))]
            [(end-of-str? next)
             ; not in end state
             (cont (NotMatch next))]
            [else
              (letrec
                ([match-trans (lambda (trans)
                                ;(print "here")(newline)
                                (cond
                                  [(null? trans)
                                   ;(print "null")(newline)
                                   ; no trans matched for current condition
                                   (cond
                                     [(is-end-state? cur-state)
                                      (if (null? matched-strs)
                                        ; Nothing has ever been matched
                                        (cont (NotMatch next))
                                        ; currently, we are in end state,
                                        ; the match for current automation is finished.
                                        ; go on with the next one.
                                        (cont (PartialMatch (string-reverse-append matched-strs) next)))]
                                     [else
                                       ;(printf "not match ~a" cont)(newline)
                                       ; we are not in end state, and we can't match
                                       (cont (NotMatch next))])]
                                  [else
                                    (match-a-transition
                                      (car trans)
                                      (lambda (matched? . rest)
                                        (if matched?
                                          (match rest
                                            [(list new-state new-next str)
                                             (loop new-state new-next (cons str matched-strs))])
                                          (match-trans (cdr trans)))))]))]
                 [match-a-transition (lambda (tran cont)
                                       #|(printf "match ~a with ~a\n"
                                               tran (string-ref str next))|#
                                       (match tran
                                         [`(,start ,a ,end)
                                           (match a
                                             ['whitespace
                                              (match-whitespace a end cont)]
                                             ['letter
                                              (match-letter a end cont)]
                                             ['any
                                              (match-any a end cont)]
                                             ['digit
                                              (match-digit a end cont)]
                                             [`(not ,c)
                                               (match-negate-char c end cont)]
                                             [(? string? s)
                                              (match-string s end cont)]
                                             [(? char? c)
                                              (match-char c end cont)]
                                             [_ (error 'match-transition "not supported regular expression unit:~a" a)]
                                             )]))]
                 [match-string (lambda (to-match to-state cont)
                                 (let str-loop ([i 0]
                                                [next next])
                                   (cond
                                     [(>= i (string-length to-match))
                                      (cont #t to-state next to-match)]
                                     [(end-of-str? next)
                                      (cont #f)]
                                     [(char=? (string-ref str next)
                                              (string-ref to-match i))
                                      (str-loop (add1 i)
                                                (add1 next))]
                                     [else
                                       (cont #f)])))]
                 [match-char (lambda (to-match to-state cont)
                               (cond
                                 [(end-of-str? next)
                                  (cont #f)]
                                 [(char=? (string-ref str next)
                                          to-match)
                                  (cont #t to-state (add1 next) (string to-match))]
                                 [else
                                   (cont #f)]))]
                 [match-whitespace (lambda (to-match to-state cont)
                                     ; longest match
                                     (let whitespace-loop ([i 0]
                                                           [next next]
                                                           [white-spaces '()])
                                       (cond
                                         [(and (end-of-str? next)
                                               (zero? i))
                                          ; end of str at the beginning
                                          (cont #f)]
                                         [(end-of-str? next)
                                          ; end of str, we have loggest white-spaces
                                          (cont #t to-state next (string-reverse-append white-spaces))]
                                         [(char-whitespace? (string-ref str next))
                                          (whitespace-loop (add1 i)
                                                           (add1 next)
                                                           (cons (string (string-ref str next))
                                                                 white-spaces))]
                                         [(zero? i)
                                          ; the first match failed
                                          (cont #f)]
                                         [else
                                           ; current match failed, we have loggest white-spaces
                                           (cont #t to-state next (string-reverse-append white-spaces))]
                                         )))]
                 [match-letter (lambda (to-match to-state cont)
                                 ; match a letter
                                 (cond
                                   [(end-of-str? next)
                                    (cont #f)]
                                   [(not (char-whitespace? (string-ref str next)))
                                    (cont #t to-state (add1 next) (string (string-ref str next)))]
                                   [else
                                     (cont #f)]))]
                 [match-digit (lambda (to-match to-state cont)
                                ; match a digit
                                (cond
                                  [(end-of-str? next)
                                   (cont #f)]
                                  [(char-numeric? (string-ref str next))
                                   (cont #t to-state (add1 next) (string (string-ref str next)))]
                                  [else
                                    (cont #f)]))]
                 [match-any (lambda (to-match to-state cont)
                              (cont #t to-state (add1 next) (string (string-ref str next))))]
                 [match-negate-char (lambda (to-match to-state cont)
                                      (cond
                                        [(end-of-str? next)
                                         (cont #f)]
                                        [(char=? (string-ref str next)
                                                 to-match)
                                         (cont #f)]
                                        [else
                                          (cont #t to-state (add1 next) (string (string-ref str next)))]))]
                 )
                (match-trans
                  (hash-ref dfa-trans cur-state '())))])))]))

; make dfa from regular expression
(define (make-dfa reg)
  (define gen-state
    (let ([n -1])
      (lambda ()
        (set! n (add1 n))
        n)))
  ; create nfa
  (match (merge-nfa (make-nfa reg))
    [(list nfa-start nfa-end nfa-trans)
     (letrec
       ([sigma-closure
          ; get sigma closure of a state. use dynamic programming
          (let ([mem (make-hasheq)])
            ; Dynamic Programming
            (lambda (s)
              (if (hash-has-key? mem s)
                (hash-ref mem s)
                ; get the sigma-closure of some state, return as a set
                (let loop ([acc (seteq s)]
                           [sub-trans-list (hash-ref nfa-trans s '())])
                  (let ([new-states
                          (set->list
                            (set-subtract
                              (list->seteq
                                (map
                                  trans-end
                                  (filter sigma-trans? sub-trans-list)))
                              acc))])
                    (if (null? new-states)
                      (begin
                        (hash-set! mem s acc)
                        acc)
                      (loop (set-union acc (list->seteq new-states))
                            (foldl
                              (lambda (s ret)
                                (append
                                  ret
                                  (hash-ref nfa-trans s '())))
                              '()
                              new-states))))))))])
       (letrec ([dfa-start (sigma-closure nfa-start)]
                [dfa-trans (list)]
                [dfa-end-tags (list)]
                [dfa-states (make-hash)]
                [get-nfa-trans (lambda (nfa-state)
                                 (hash-ref nfa-trans nfa-state '()))]
                [is-new-state? (lambda (state)
                                 (if (hash-has-key? dfa-states state)
                                   #f
                                   #t))]
                [add-dfa-end-tags (lambda (state)
                                    (set! dfa-end-tags
                                      (cons state dfa-end-tags)))]
                [add-state! (lambda (state)
                              ; we map each dfa state to a number/tag
                              ; The smallest state number should be the
                              ; start state.
                              (let ([state-tag (gen-state)])
                                ; if nfa end state is in the dfa state,
                                ; then the dfa state is an end state
                                (when (set-member? state nfa-end)
                                  (add-dfa-end-tags state-tag))
                                (hash-ref! dfa-states state state-tag)
                                state-tag))]
                [dfa-start-tag
                  ; this should be evaluated before other add-state! expression,
                  ; since we want to give the start state smallest tag
                  (add-state! dfa-start)]
                [get-state-tag (lambda (state)
                                 ;(print state)(newline)
                                 (hash-ref dfa-states state (lambda ()
                                                              (error 'get-state-tag "state not found"))))]
                [add-dfa-trans! (lambda (start a end)
                                  ; we didn't remove duplicates here.
                                  (set! dfa-trans
                                    (cons (list (get-state-tag start)
                                                a
                                                (get-state-tag end))
                                          dfa-trans)))]
                [dfs! (lambda (start-state)
                        ; start is seteq, states is set of seteq
                        ; for state start: traverse every nfa state in it,
                        ; get its non-sigma nfa transition, make a dfa-trans,
                        ; if dfa transition is a new transition, then add it.
                        (for-each
                          (lambda (nfa-state)
                            (for-each
                              (match-lambda
                                [`(,a-nfa-start sigma ,a-nfa-end)
                                  (void)
                                  ; sigma trans. ignore!
                                  ]
                                [`(,a-nfa-start ,a ,a-nfa-end)
                                  ; non sigma trans.
                                  (let ([end-state (sigma-closure a-nfa-end)])
                                    ; add the state if it's a new state
                                    (when (is-new-state? end-state)
                                      (add-state! end-state)
                                      (dfs! end-state))
                                    ; Note we should add trans whether this
                                    ; is a new state or not. Since for a start
                                    ; state, it's possible to have multi transition
                                    ; for two different input.(DFA: single transition for per input per state)
                                    (add-dfa-trans! start-state a end-state))]
                                [_ (error 'dfa-dfs "not a nfa trans")])
                              (get-nfa-trans nfa-state)))
                          (set->list start-state)))]
                [merge-dfa-trans (lambda ()
                                   ; remove duplicates, create a hash table for the transitions
                                   (let ([dfa-trans (remove-duplicates dfa-trans equal?)]
                                         [trans-tbl (make-hasheq)])
                                     (for-each
                                       (lambda (dfa-tran)
                                         (hash-add-trans! trans-tbl dfa-tran))
                                       dfa-trans)
                                     trans-tbl))]
                )
         (dfs! dfa-start)
         (list
           dfa-start-tag
           dfa-end-tags
           ;dfa-trans))
           (merge-dfa-trans))))]))

(module+ test
  (let ([dfa (make-dfa '(arbno (or letter digit whitespace)))])
    (match-dfa dfa "a b c" 0 (lambda (match-res)
                                (print match-res)(newline)))))
