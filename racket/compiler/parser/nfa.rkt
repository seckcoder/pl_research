#lang racket

(require "../../base/utils.rkt")

(provide make-nfa
         merge-nfa)

; Regexp ::= String | letter | digit | whitespace | any
;        ::= (not Character) | (or {Regexp}!)
;        :: = (arbno Regexp) | ({Regexp}!)


; NFA ::= (start end (i a j) ...)
; Several assumtions for nfa to simplify the code
;   - only sigma trans for multi trans
;   - only one end state(If there are multi end states, we'll construct sigma trans for each end state to our ultimate end state)
;   - We don't accept sigma. sigma is only introduced during the nfa construction
; Character ::= String | char | letter | digit | whitespace | any 
; a ::= Character | (not Character)

(define gen-state
  (let ([n -1])
    (lambda ()
      (set! n (add1 n))
      n)))

(define nfa-start car)
(define nfa-end cadr)
(define nfa-trans caddr)
(define (sigma-trans start end)
  `(,start sigma ,end))

(define (concat-two-nfa nfa1 nfa2)
  (list
    (nfa-start nfa1)
    (nfa-end nfa2)
    (append
      (nfa-trans nfa1)
      (nfa-trans nfa2)
      ; nfa1-end ---> nfa2-start
      (list
        (sigma-trans (nfa-end nfa1)
                     (nfa-start nfa2))))))
(define (concat-nfas nfas)
  (cond
    [(null? nfas)
     (error 'concat-nfas "bad nfas")]
    [(null? (cdr nfas))
     (car nfas)]
    [else
      (concat-nfas (cons
                     (concat-two-nfa (car nfas) (cadr nfas))
                     (cddr nfas)))]))

(define (make-nfa reg)
  (let ([start (gen-state)]
        [end (gen-state)]
        [character? (lambda (v)
                      (or (string? v)
                          (char? v)
                          (memq v '(letter digit whitespace any))))])
    (match reg
      [(? character? c)
       `(,start ,end ((,start ,c ,end)))]
      [`(not ,(? character? c))
        `(,start ,end ((,start ,c ,end)))]
      [`(or ,reg0 ,reg* ...)
        (let ([nfas
                (map
                  make-nfa
                  (cons reg0 reg*))])
          (list start
                end
                (flatmap
                  (match-lambda
                    [(list a-start a-end a-trans)
                     (append a-trans
                             (list
                               (sigma-trans start a-start)
                               (sigma-trans a-end end)))])
                  nfas)))]
      [`(concat ,reg0 ,reg* ...)
        (let ([nfas (map make-nfa (cons reg0 reg*))])
          (concat-nfas nfas))]
      [`(arbno ,reg)
        (let* ([unit-nfa (make-nfa reg)]
               [unit-start (nfa-start unit-nfa)]
               [unit-end (nfa-end unit-nfa)])
          (list start
                end
                (append
                  (nfa-trans unit-nfa)
                  (list
                    (sigma-trans unit-end start)
                    (sigma-trans unit-end end)
                    (sigma-trans start unit-start)
                    (sigma-trans start end))
                  )))]
      [_ (error 'nfa "the format of regular expression:~a is not right" reg)]
      )))

; merge nfa's trans into a hashtable.
; arrange the keys from 0
(define (merge-nfa nfa)
  (let* ([htb (make-hasheq)]
         [hash-add! (lambda (trans)
                      ;(print trans)(newline)
                      (match trans
                        [`(,start ,a ,end)
                          (if (not (hash-has-key? htb start))
                            (hash-set! htb start (list trans))
                            (hash-set!
                              htb
                              start
                              (cons
                                trans
                                (hash-ref htb start))))]))])
    (for-each
      hash-add!
      (nfa-trans nfa))
    (match (re-hash (nfa-start nfa)
                    (nfa-end nfa)
                    htb)
      [(list start end htb)
       (list start end htb)])))

(define (re-hash start end htb)
  (let* ([keys (hash-keys htb)]
         [values (flatmap
                   (lambda (k)
                     (flatmap
                       (lambda (trans)
                         (match trans
                           [`(,start ,a ,end)
                             (list start end)]
                           [_ (error 'test1 "b")]
                           ))
                       (hash-ref htb k)))
                   keys)]
         [min-k (apply min values)]
         [max-k (apply max values)]
         [hash-fun (lambda (k)
                     (cond
                       [(= k start) 0]
                       [(= k end) (- max-k min-k)]
                       [(= k min-k) (- start min-k)]
                       [(= k max-k) (- end min-k)]
                       [else (- k min-k)]))]
         [new-htb (make-hasheq)])
    ;(printf "~a ~a\n" min-k max-k)
    (for-each
      (lambda (k)
        (hash-set!
          new-htb
          (hash-fun k)
          (map
            (lambda (trans)
              (match trans
                [`(,start ,a ,end)
                  `(,(hash-fun start) ,a ,(hash-fun end))]
                [_ (error 'test "aab")]
                ))
            (hash-ref htb k))))
      keys)
    (list (hash-fun start)
          (hash-fun end)
          new-htb)
    ))

(module+ test
  (merge-nfa (make-nfa "a"))
  (merge-nfa (make-nfa '(arbno (or digit whitespace))))
  (merge-nfa (make-nfa '(concat ";" (arbno (not #\newline)))))
  )
