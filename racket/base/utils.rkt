#lang racket

(require racket/match)
(require racket/pretty)

(provide anything?
         list-of
         index-of
         check
         println
         while
         add1
         sub1
         caddddr
         find
         mfindf
         mapn
         v->lst
         atom?
         sexp?
         tail
         flatmap
         safe-take
         apply-base-ns
         sym-append
         combine
         allf
         anyf
         number->symbol
         remove-nth
         filteri
         mapi
         group
         groupf
         Some
         Some?
         None
         None?
         ref
         ref?
         set-ref-v!
         ref-v
         ~>
         range
         range-len
         range-of-i
         )

(define anything?
  (lambda (v)
    #t))

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

(define index-of
  (lambda (lst v)
    (define (iter lst v idx)
      (cond ((null? lst) -1)
            ((eq? (car lst) v) idx)
            (else
              (iter (cdr lst) v (+ 1 idx)))))
    (iter lst v 0)))

(define-syntax check
  (syntax-rules ()
    [(_ pred a b)
     (let ((va a)
           (vb b))
       (if (not (pred va vb))
         (error 'check "~a is not ~a to ~a\n\t~a not ~a ~a\n" `a `pred `b va `pred vb)
         'ok))]))

(define list-n
  (lambda (n v)
    (if (= n 0)
      '()
      (cons v (list-n (- n 1) v)))))

(define-syntax while
  (syntax-rules ()
    [(_ pred body body* ...)
     (let loop ((v 'unintialized))
       (if pred
         (let ((v (let ()
                    body body* ...)))
           (loop v))
         v))]))

(define caddddr
  (lambda (lst)
    (car (cddddr lst))))

(define find
  (lambda (handle lst)
    (let loop ((lst lst)
               (idx 0))
      (cond ((null? lst) (list #f '() idx))
            ((handle (car lst)) (list #t (car lst) idx))
            (else
              (loop (cdr lst) (add1 idx)))))))

; map n times
(define mapn
  (lambda (handle n)
    (let loop ((i 0))
      (if (>= i n)
        '()
        (cons (handle i)
              (loop (add1 i)))))))

(define (v->lst n v)
  (mapn (lambda (_) v) n))

(define atom?
  (lambda (v)
    (and (not (pair? v))
         (not (null? v)))))


(define sexp?
  (lambda (s)
    (or (atom? s)
        (list? s))))

(define println
  (lambda args
    (apply print args)(newline)))

(define tail
  (lambda (lst)
    (cond ((null? lst)
           (error 'tail "list is null"))
          ((null? (cdr lst))
           (car lst))
          (else
            (tail (cdr lst))))))
(define mfindf
  (lambda (handle lst)
    (match (find handle lst)
      [(list finded? rest ...)
       finded?])))

(define flatmap
  (lambda (handle . rest)
    (apply foldl `(,(lambda args
                      (match args
                        [(list handle-params ... acc)
                         (append acc
                                 (apply handle handle-params))]))
                    ()
                    ,@rest))))

(define safe-take
  (lambda (lst n)
    (if (or (= n 0)
            (null? lst))
      '()
      (cons (car lst)
            (safe-take (cdr lst)
                       (- n 1))))))


(define (apply-base-ns op rands)
  (apply (eval op (make-base-namespace))
         rands))

(define sym-append
  (lambda syms
    (string->symbol
      (foldl
        (lambda (sym s)
          (string-append
            s
            (symbol->string sym)))
        ""
        syms))))

(define number->symbol
  (lambda (n)
    (string->symbol (number->string n))))

; deprecated!
; use racket's compose instead
(define combine
  (match-lambda*
    [(list f) f]
    [(list f f* ...)
     (lambda (v)
       ((apply combine f*) (f v)))]))


(define allf
  (match-lambda*
    [(list) (lambda (v) #t)]
    [(list f0 f* ...)
     (lambda (v)
       (and (f0 v)
            ((apply allf f*) v)))]))

(define anyf
  (match-lambda*
    [(list) (lambda (v) #f)]
    [(list f0 f* ...)
     (lambda (v)
       (or (f0 v)
           ((apply anyf f*) v)))]))

(define (filteri pred lst)
  (let loop ((i 0)
             (lst lst))
    (cond
      ((null? lst) '())
      (else
        (if (pred i (car lst))
          (loop (add1 i)
                (cdr lst))
          (cons (car lst)
                (loop (add1 i)
                      (cdr lst))))))))

(define (mapi proc lst)
  (let loop ((i 0)
             (lst lst))
    (cond
      ((null? lst) '())
      (else
        (cons (proc i (car lst))
              (loop (add1 i)
                    (cdr lst)))))))

(define (remove-nth lst i)
  (filteri
    (lambda (j v)
      (= i j))
    lst))

(define (group lst n)
  (cond
    ((null? lst) '())
    (else
      (cons (take lst n)
            (group (drop lst n) n)))))
(define groupf1
  (lambda (lst pred-fns split-fns)
    (cond
      [(null? lst) '()]
      [else
        (let-values ([(v rest)
                      (let loop ((pred-fns pred-fns)
                                 (split-fns split-fns))
                        (cond
                          ((null? pred-fns)
                           (error 'groupf "no function applied for list:~a" lst))
                          (else
                            (match* ((car pred-fns)
                                     (car split-fns))
                              [(pred-fn split-fn)
                               (if (pred-fn lst)
                                 (split-fn lst)
                                 (loop (cdr pred-fns)
                                       (cdr split-fns)))]))))])
          (cons v (groupf1 rest pred-fns split-fns)))])))

(define-syntax groupf
  (syntax-rules()
    [(_ lst (pred-fn0 split-fn0) (pred-fn* split-fn*) ...)
     (groupf1 lst
              `(,pred-fn0 ,pred-fn* ...)
              `(,split-fn0 ,split-fn* ...))]))

; ML style struct
(struct Some (v) #:transparent)
(struct None () #:transparent)
(struct ref (v) #:mutable #:transparent)

; like Clojure's ->
(define (~> v f . rest)
  ((apply compose1 (reverse (cons f rest))) v))

; my powerful range tools
(define (range-param-check start step end)
  (unless (or (and (<= start end) (> step 0))
             (and (>= start end) (< step 0)))
       (error 'range "range parameters:[~a...[+~a]...~a] is not right\n"
              start step end))
  (if (> step 0)
    >
    <))

; (range start step end) -> [start ... end]
(define range
  (match-lambda*
    [(list start end)
     (if (> start end)
       (range start -1 end)
       (range start 1 end))]
    [(list start step end)
     (let ((cmp (range-param-check start step end)))
       (let loop ([i start]
                  [acc '()])
         (if (cmp i end)
           (reverse acc)
           (loop (+ i step)
                 (cons i acc)))))]))

(define range-len
  (match-lambda*
    [(list start end)
     (if (> start end)
       (range-len start -1 end)
       (range-len start 1 end))]
    [(list start step end)
     (add1 (quotient (- end start) step))]))

(define range-of-i
  (match-lambda*
    [(list start end)
     (if (> start end)
       (range-of-i start -1 end)
       (range-of-i start 1 end))]
    [(list start step end)
     (lambda (i)
       (+ start (* i step)))]))

(module+ test
  ; test without check
  ((combine (lambda (v) v)
            (lambda (v) v)
            (lambda (v) v))
   3)
  ((allf (lambda (v) (> v 0))
         (lambda (v) (< v 10))
         (lambda (v) (even? v))) 4)
  ((anyf (lambda (v) (= v 1))
         (lambda (v) (= v 2))
         (lambda (v) (= v 3))) 3)
  ((anyf (lambda (v) (= v 1))
         (lambda (v) (= v 2))
         (lambda (v) (= v 3))) 0)
  )

(module+ test
  (require rackunit)
  ; group
  (check-equal? (group '(1 2 3 4) 2)
                '((1 2)
                  (3 4)))

  (check-equal?
    (groupf
      '(1 2 3 4 5 6 7 8 9)
      [(compose odd? car)
       (lambda (lst)
         (split-at lst 1))]
      [(compose even? car)
       (lambda (lst)
         (split-at lst 3))]
      )
    '((1) (2 3 4) (5) (6 7 8) (9)))

  (check-equal?
    (~> 3
        (lambda (v) (* v v))
        (lambda (v)
          (- v 3)))
    6)
  
  (check-equal?
    (range 3 3) '(3))
  (check-equal? (range 3 4) '(3 4))
  (check-equal? (range 3 2 8) '(3 5 7))
  (check-equal? (range 3 8) '(3 4 5 6 7 8))
  (check-equal? (range 8 3) '(8 7 6 5 4 3))

  (check-equal?
    (range-len 3 3) 1)
  (check-equal? (range-len 3 4) 2)
  (check-equal? (range-len 3 2 8) 3)
  (check-equal? (range-len 3 8) 6)
  (check-equal? (range-len 8 3) 6)

  (check-equal?
    ((range-of-i 3 3) 0) 3)
  (check-equal?
    ((range-of-i 3 4) 1) 4)
  (check-equal?
    ((range-of-i 3 2 8) 2) 7)
  (check-equal?
    ((range-of-i 3 2 8) 1) 5)
  (check-equal?
    ((range-of-i 8 3) 5) 3)
  (check-equal?
    ((range-of-i 8 3) 2) 6)

  ;(check-equal? 
  )
