#lang racket

(require "nfa.rkt")

(provide make-dfa)
; end of str and we are in end state.
; s is the matched str.
(struct FullMatch (s) #:transparent)
; next char can't be matched, but we are in end state
(struct PartialMatch (s next) #:transparent)
; the final state can't be accepted
(struct NotMatch (next))

(define (make-dfa reg)
  (let ([the-dfa 'uninitialized])
    (define gen-state
      (let ([n -1])
        (lambda ()
          (set! n (add1 n))
          n)))
    (define (match-dfa str next k)
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
                 (k (FullMatch (string-reverse-append matched-strs)))]
                [(end-of-str? next)
                 ; not in end state
                 (k (NotMatch next))]
                [else
                  (letrec
                    ([match-trans (lambda (trans)
                                    (cond
                                      [(null? trans)
                                       ; no trans matched for current condition
                                       (cond
                                         [(is-end-state? cur-state)
                                          (if (null? matched-strs)
                                            ; Nothing has ever been matched
                                            (k (NotMatch next))
                                            ; currently, we are in end state,
                                            ; the match for current automation is finished.
                                            ; go on with the next one.
                                            (k (PartialMatch (string-reverse-append matched-strs) next)))]
                                         [else
                                           ; we are not in end state, and we can't match
                                           (k (NotMatch next))])]
                                      [else
                                        (match-a-transition
                                          (car trans)
                                          (lambda (matched? . rest)
                                            (if matched?
                                              (match rest
                                                [(list new-state new-next str)
                                                 (loop new-state new-next (cons str matched-strs))])
                                              (match-trans (cdr trans)))))]))]
                     [match-a-transition (lambda (tran k)
                                           #|(printf "match ~a with ~a\n"
                                                   tran (string-ref str next))|#
                                           (match tran
                                             [`(,start ,a ,end)
                                               (match a
                                                 ['whitespace
                                                  (match-whitespace a end k)]
                                                 ['letter
                                                  (match-letter a end k)]
                                                 ['any
                                                  (match-any a end k)]
                                                 ['digit
                                                  (match-digit a end k)]
                                                 [`(not ,c)
                                                   (match-negate-char c end k)]
                                                 [(? string? s)
                                                  (match-string s end k)]
                                                 [(? char? c)
                                                  (match-char c end k)]
                                                 [_ (error 'match-transition "not supported regular expression unit:~a" a)]
                                                 )]))]
                     [match-string (lambda (to-match to-state k)
                                     (let str-loop ([i 0]
                                                    [next next])
                                       (cond
                                         [(>= i (string-length to-match))
                                          (k #t to-state next to-match)]
                                         [(end-of-str? next)
                                          (k #f)]
                                         [(char=? (string-ref str next)
                                                  (string-ref to-match i))
                                          (str-loop (add1 i)
                                                    (add1 next))]
                                         [else
                                           (k #f)])))]
                     [match-char (lambda (to-match to-state k)
                                   (cond
                                     [(end-of-str? next)
                                      (k #f)]
                                     [(char=? (string-ref str next)
                                              to-match)
                                      (k #t to-state (add1 next) (string to-match))]
                                     [else
                                       (k #f)]))]
                     [match-whitespace (lambda (to-match to-state k)
                                         ; longest match
                                         (let whitespace-loop ([i 0]
                                                               [next next]
                                                               [white-spaces '()])
                                           (cond
                                             [(and (end-of-str? next)
                                                   (zero? i))
                                              ; end of str at the beginning
                                              (k #f)]
                                             [(end-of-str? next)
                                              ; end of str, we have loggest white-spaces
                                              (k #t to-state next (string-reverse-append white-spaces))]
                                             [(char-whitespace? (string-ref str next))
                                              (whitespace-loop (add1 i)
                                                               (add1 next)
                                                               (cons (string (string-ref str next))
                                                                     white-spaces))]
                                             [(zero? i)
                                              ; the first match failed
                                              (k #f)]
                                             [else
                                               ; current match failed, we have loggest white-spaces
                                               (k #t to-state next (string-reverse-append white-spaces))]
                                             )))]
                     [match-letter (lambda (to-match to-state k)
                                     ; match a letter
                                     (cond
                                       [(end-of-str? next)
                                        (k #f)]
                                       [(char-alphabetic? (string-ref str next))
                                        (k #t to-state (add1 next) (string (string-ref str next)))]
                                       [else
                                         (k #f)]))]
                     [match-digit (lambda (to-match to-state k)
                                    ; match a digit
                                    (cond
                                      [(end-of-str? next)
                                       (k #f)]
                                      [(char-numeric? (string-ref str next))
                                       (k #t to-state (add1 next) (string (string-ref str next)))]
                                      [else
                                        (k #f)]))]
                     [match-any (lambda (to-match to-state k)
                                  (k #t to-state (add1 next) (string (string-ref str next))))]
                     [match-negate-char (lambda (to-match to-state k)
                                          (cond
                                            [(end-of-str? next)
                                             (k #f)]
                                            [(char=? (string-ref str next)
                                                     to-match)
                                             (k #f)]
                                            [else
                                              (k #t to-state (add1 next) (string (string-ref str next)))]))]
                     )
                    (match-trans
                      (hash-ref dfa-trans cur-state '())))])))]))
    (define (self m)
      (case m
        [(match) (lambda (str k)
                   (match-dfa str 0 k))]
        [(fetch) the-dfa]))
    (match (merge-nfa (make-nfa reg))
      [(list nfa-start nfa-end nfa-trans)
       (letrec
         ([sigma-closure
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
                                ;(printf "add state:~a\n" state)
                                ; The smallest state number should be the
                                ; start state.
                                (let ([state-tag (gen-state)])
                                  (when (set-member? state nfa-end)
                                    (add-dfa-end-tags state-tag))
                                  (hash-ref! dfa-states state state-tag)
                                  state-tag))]
                  [dfa-start-tag (add-state! dfa-start)]
                  [get-state-tag (lambda (state)
                                   ;(print state)(newline)
                                   (hash-ref dfa-states state (lambda ()
                                                                (error 'get-state-tag "state not found"))))]
                  [add-dfa-trans! (lambda (start a end)
                                    ; we didn't remove duplicates here
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
                                      (when (is-new-state? end-state)
                                        (add-state! end-state)
                                        (dfs! end-state))
                                      ; Note we should add trans whether this
                                      ; is a new state or not
                                      (add-dfa-trans! start-state a end-state))]
                                  [_ (error 'dfa-dfs "not a nfa trans")])
                                (get-nfa-trans nfa-state)))
                            (set->list start-state)))]
                  [merge-dfa-trans (lambda ()
                                     (let ([dfa-trans (remove-duplicates dfa-trans equal?)]
                                           [trans-tbl (make-hasheq)])
                                       (for-each
                                         (lambda (dfa-tran)
                                           (hash-add-trans! trans-tbl dfa-tran))
                                         dfa-trans)
                                       trans-tbl))]
                  )
           (dfs! dfa-start)
           (set! the-dfa
             (list
               dfa-start-tag
               dfa-end-tags
               ;dfa-trans))
               (merge-dfa-trans)))
    self))])))

(module+ test
  (let ([dfa (make-dfa '(arbno (concat (or letter digit whitespace) (not #\c))))])
    ((dfa 'match) "a b 1 -" (lambda (match-res)
                           (print match-res)(newline)))))
