(library
  (c311 a1)
  (export a1
          fact
          ) 

  (import (rnrs)
          (rnrs r5rs)
          (elegant-weapons compat)
          (elegant-weapons tester)
          (stream)
          (utils)
          )

  (define countdown
    (lambda (n)
      (if (= n 0)
        '(0)
        (cons n (countdown (sub1 n))))))

  (define insertR
    (lambda (old new lat)
      (cond ((null? lat) '())
            ((eq? old (car lat))
             (cons old
                   (cons new
                         (insertR old new (cdr lat)))))
            (else
              (cons (car lat)
                    (insertR old new (cdr lat)))))))

  (define remv-lst
    (lambda (sym lat)
      (cond ((null? lat) '())
            ((eq? sym (car lat))
             (cdr lat))
            (else
              (cons (car lat)
                    (remv-lst sym (cdr lat)))))))

  (define occurs-s-num
    (lambda (sym lat)
      (cond ((null? lat) 0)
            ((eq? sym (car lat))
             (add1 (occurs-s-num sym (cdr lat))))
            (else
              (occurs-s-num sym (cdr lat))))))

  (define occurs-?s
    (lambda (lat)
      (occurs-s-num '? lat)))

  ; filter ...

  (define zip
    (lambda (lst1 lst2)
      (cond ((and (null? lst1)
                  (null? lst2))
             '())
            ((or (null? lst1)
                 (null? lst2))
             (error 'zip "list args should have the same length"))
            (else
              (cons (cons (car lst1)
                          (car lst2))
                    (zip (cdr lst1)
                         (cdr lst2)))))))


  ; map ...

  ; append ...

  ; reverse ...

  ; fact ...

  (define member*
    (lambda (sym lst)
      (cond ((null? lst) #f)
            ((atom? (car lst))
             (or (eq? sym
                      (car lst))
                 (member* sym (cdr lst))))
            (else
              (or (member* sym (car lst))
                  (member* sym (cdr lst)))))))

  (define member-?*
    (lambda (lst)
      (member* '? lst)))

  (define fib
    (lambda (n)
      (let loop ((a 0)
                 (b 1)
                 (i 0))
        (if (= i n)
          a
          (loop b (+ a b) (add1 i))))))

  (define cons-cell-count
    (lambda (exp)
      (cond ((null? exp) 0)
            ((atom? exp) 0)
            (else
              (add1 (+ (cons-cell-count (car exp))
                       (cons-cell-count (cdr exp))))))))

  ; 14. ((w x) y (z)) -> ((w x) . (y (z))) -> ((w . (x)) . (y . ((z)))) -> ((w . (x . ())) . (y . ((z) . ()))) -> ((w . (x . ())) . (y . ((z . ()) . ())))

  (define a1-14
    (equal? '((w x) y (z))
            '((w . (x . ())) . (y . ((z . ()) . ())))))

  (define binary->natural
    (lambda (binary-digits-rev)
      (let loop ((rest-digits binary-digits-rev)
                 (accum 0)
                 (digit-multiplier 1))
        (if (null? rest-digits)
          accum
          (if (= (car rest-digits) 0)
            (loop (cdr rest-digits)
                  accum
                  (* 2 digit-multiplier))
            (loop (cdr rest-digits)
                  (+ accum digit-multiplier)
                  (* 2 digit-multiplier)))))))

  (define natural->binary
    (lambda (n)
      (if (= n 0)
        '()
        (cons (remainder n 2)
              (natural->binary (quotient n 2))))))

  (define insertR-fr
    (lambda (old new lat)
      (fold-right (lambda (atom accum)
                    (if (eq? old atom)
                      (cons atom
                            (cons new accum))
                      (cons atom accum)))
                  '()
                  lat)))

  (define occurs-s-num-fr
    (lambda (sym lat)
      (fold-right (lambda (atom accum)
                    (if (eq? sym atom)
                      (add1 accum)
                      accum))
                  0
                  lat)))

  (define occurs-?s-fr
    (lambda (lat)
      (occurs-s-num-fr '? lat)))

  (define filter-fr
    (lambda (predicate lst)
      (fold-right (lambda (exp accum)
                    (if (predicate exp)
                      (cons exp accum)
                      accum))
                  '()
                  lst)))

  (define zip-fr
    (lambda (lst1 lst2)
      (fold-right (lambda (atom1 atom2 accum)
                    (cons (cons atom1 atom2)
                          accum))
                  '()
                  lst1
                  lst2)))


  (define map-fr
    (lambda (proc lst)
      (fold-right (lambda (exp accum)
                    (cons (proc exp) accum))
                  '()
                  lst)))

  (define append-fr
    (lambda (lst1 lst2)
      (fold-right (lambda (exp accum)
                    (cons exp accum))
                  lst2
                  lst1)))

  ; I think it's better to use fold-left here
  (define reverse-fr
    (lambda (lst)
      (fold-left (lambda (accum exp)
                   (cons exp accum))
                 '()
                 lst)))

  (define binary->natural-fr
    (lambda (binary-digits-rev)
      (fold-left (lambda (accum digit multiplier)
                   (if (= digit 0)
                     accum
                     (+ accum multiplier)))
                 0
                 binary-digits-rev
                 (take (length binary-digits-rev)
                       (geometric-series 1 2)))))

  (define fact
    (lambda (n)
      (define (fact-acc n accum)
        (if (= n 0)
          accum
          (fact-acc (sub1 n) (* n accum))))
      (fact-acc n 1)))

  (define power
    (lambda (x n)
      (cond ((= n 0) 1)
            ((odd? n) (* x (power x (sub1 n))))
            (else
              (square (power x (/ n 2)))))))

  (define sub
    (lambda (a b)
      (if (= b 0)
        a
        (sub (sub1 a)
             (sub1 b)))))

  (define mdiv
    (lambda (a b)
      (if (< a b)
        0
        (add1 (mdiv (sub a b)
                    b)))))

  (define base
    (lambda (x)
      (error 'error "Invalid value ~s~n" x)))

  (define odd-case
    (lambda (recur)
      (lambda (x)
        (cond
          ((odd? x) (collatz (add1 (* x 3))))
          (else (recur x))))))

  (define even-case
    (lambda (recur)
      (lambda (x)
        (cond
          ((even? x) (collatz (/ x 2)))
          (else (recur x))))))

  (define one-case
    (lambda (recur)
      (lambda (x)
        (cond
          ((zero? (sub1 x)) 1)
          (else (recur x))))))

  (define collatz (one-case (odd-case (even-case base))))

  (define quine-demo
    ((lambda (x)
       (list x (list 'quote x)))
     '(lambda (x)
        (list x (list 'quote x)))))

  ; http://www.madore.org/~david/computers/quine.html

  (define-test-suite
    a1
    (basic
      (lambda (fail)
        (or (equal? (countdown 5) '(5 4 3 2 1 0)) (fail))
        (or (equal? (insertR 'x 'y '(x z z x y x)) '(x y z z x y y x y)) (fail))
        (or (equal? (remv-lst 'x '(x y z x)) '(y z x)) (fail))
        (or (equal? (remv-lst 'y '(x y z y x)) '(x z y x)) (fail))
        (or (equal? (occurs-?s '(? y z ? ?)) 3) (fail))
        (or (equal? (zip '(1 2 3) '(a b c)) '((1 . a) (2 . b) (3 . c))) (fail))
        (or (equal? (member-?* '(a b c)) #f) (fail))
        (or (equal? (member-?* '(a ? c)) #t) (fail))
        (or (equal? (member-?* '((a ((?)) ((c) b c)))) #t) (fail))
        (or (equal? (fib 0) 0) (fail))
        (or (equal? (fib 1) 1) (fail))
        (or (equal? (fib 7) 13) (fail))
        (or (equal? (cons-cell-count 'a) 0) (fail))
        (or (equal? (cons-cell-count '(3 . 4)) 1) (fail))
        (or (equal? (cons-cell-count '(a b . c)) 2) (fail))
        (or (equal? (cons-cell-count '((a b . c) 3 . 4)) 4) (fail))
        (or (equal? a1-14 #t) (fail))
        (or (equal? (binary->natural '()) 0) (fail))
        (or (equal? (binary->natural '(0 0 1)) 4) (fail))
        (or (equal? (binary->natural '(0 0 1 1)) 12) (fail))
        (or (equal? (binary->natural '(1 1 1 1)) 15) (fail))
        (or (equal? (binary->natural '(1 0 1 0 1)) 21) (fail))
        (or (equal? (binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1)) 8191) (fail))
        (or (equal? (natural->binary 0) '()) (fail))
        (or (equal? (natural->binary 4) '(0 0 1)) (fail))
        (or (equal? (natural->binary 12) '(0 0 1 1)) (fail))
        (or (equal? (natural->binary 15) '(1 1 1 1)) (fail))
        (or (equal? (natural->binary 21) '(1 0 1 0 1)) (fail))
        (or (equal? (natural->binary 8191) '(1 1 1 1 1 1 1 1 1 1 1 1 1)) (fail))
        (or (equal? (insertR-fr 'x 'y '(x z z x y x)) '(x y z z x y y x y)) (fail))
        (or (equal? (occurs-?s-fr '(? y z ? ?)) 3) (fail))
        (or (equal? (zip-fr '(1 2 3) '(a b c)) '((1 . a) (2 . b) (3 . c))) (fail))
        (or (equal? (filter-fr even? '(1 2 3 4 5 6)) '(2 4 6)) (fail))
        (or (equal? (map-fr add1 '(1 2 3 4)) '(2 3 4 5)) (fail))
        (or (equal? (append-fr '(a b c) '(1 2 3)) '(a b c 1 2 3)) (fail))
        (or (equal? (reverse-fr '(a 3 x)) '(x 3 a)) (fail))
        (or (equal? (binary->natural-fr '()) 0) (fail))
        (or (equal? (binary->natural-fr '(0 0 1)) 4) (fail))
        (or (equal? (binary->natural-fr '(0 0 1 1)) 12) (fail))
        (or (equal? (binary->natural-fr '(1 1 1 1)) 15) (fail))
        (or (equal? (binary->natural-fr '(1 0 1 0 1)) 21) (fail))
        (or (equal? (binary->natural-fr '(1 1 1 1 1 1 1 1 1 1 1 1 1)) 8191) (fail))
        (or (equal? (fact 5) 120) (fail))
        (or (equal? (power 2 0) 1) (fail))
        (or (equal? (power 2 2) 4) (fail))
        (or (equal? (power 2 10) 1024) (fail))
        (or (equal? (power 10 5) 100000) (fail))
        (or (equal? (power 3 31) 617673396283947) (fail))
        (or (equal? (power 3 32) 1853020188851841) (fail))
        (or (equal? (sub 5 3) 2) (fail))
        (or (equal? (sub 100 50) 50) (fail))
        (or (equal? (mdiv 25 5) 5) (fail))
        (or (equal? (mdiv 27 5) 5) (fail))
        (or (equal? (collatz 12) 1) (fail))
        (or (equal? (collatz 120) 1) (fail))
        (or (equal? (collatz 9999) 1) (fail))
        )))
  )
