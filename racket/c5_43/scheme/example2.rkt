#lang racket
; Implementations and examples are inspired by Matt Might's blog posts:
; http://matt.might.net/articles/programming-with-continuations--exceptions-backtracking-search-threads-generators-coroutines/

(define (current-continuation)
  (call/cc (lambda (k)
             (k k))))

(define (exception)
  ; a demo implementation of exception using continuation
  (define stack '())
  (define (push! id cc)
    (set! stack (cons (cons id cc) stack)))
  (define pop!
    (match-lambda*
      [(list)
       (if (null? stack)
         (error 'exception "cannot pop empty stack")
         (set! stack (cdr stack)))]
      [(list id)
       (cond
         [(null? stack)
          (error 'exception "cannot handle exception:~a" id)]
         [(eq? id (except-id (top)))
          (let ([cc (except-cc (top))])
            (set! stack (cdr stack))
            cc)]
         [else
           (set! stack (cdr stack))
           (pop! id)])]))
  (define (top)
    (car stack))
  (define except-id car)
  (define except-cc cdr)


  (define-syntax try
    (syntax-rules (catch)
      [(_ exp* ... (catch id handle))
       (let ([cc (current-continuation)])
         (cond
           [(continuation? cc)
            (begin
              (push! id cc)
              exp* ...
              (pop!))]
           [(pair? cc)
            (handle (car cc) (cdr cc))]))]))

  (define (throw id msg)
    (let ([cc (pop! id)])
      (cc (cons id msg))))

  (try
    (throw 'RuntimeError "a runtime error")
    (catch 'RuntimeError
           (lambda (id msg)
             (printf "~a\n" msg))))

  (try
    (try
      (throw 'Except1 "except1")
      (display "should not be printed")
      (catch 'Except1
             (lambda (id msg)
               (printf "~a\n" msg)
               (throw id msg)
               (display "should not be printed")
               )))
    (catch 'Except1
           (lambda (id msg)
             (printf "~a\n" msg))))
  )

(define (generator)
  ; demo implementation of generator
  (define (list-iter lst)
    (lambda (yield)
      (for-each
        (lambda (v)
          (yield v))
        lst)))

  (define (make-yield for-cc)
    (lambda (v)
      (let ([cc (current-continuation)])
        (cond
          [(continuation? cc)
           ; jump back to for loops
           (for-cc (list cc v))]
          [else
            ; one loop end
            (void)]))))
  
  ; implementation of for iterator
  (define-syntax mfor
    (syntax-rules (in)
      [(_ v in iter exp* ...)
       (let ([cc (current-continuation)])
         (match cc
           [(? continuation?)
            ; make-yield pass the continuation to generator,
            ; so that it can jump back to for loops.
            (iter (make-yield cc))]
           [(list iter-cc val)
            (let ([v val])
              exp* ...)
            (iter-cc (void))]))]))

  ; tree-iter : tree -> generator
  (define (tree-iter tree)
    (lambda (yield)

      ;; Walk the tree, yielding the leaves.

      (define (walk tree)
        (if (not (pair? tree))
          (yield tree)
          (begin
            (walk (car tree))
            (walk (cdr tree)))))

      (walk tree)))

  (mfor v in (list-iter '(1 2 3))
    (printf "~a " v))
  (printf "\n")

  (mfor v in (tree-iter '(3 . ( ( 4 . 5 ) . 6 ) ))
    (printf "~a " v))
  (printf "\n")
  )

(require data/queue)
(define (coroutines)
  ; demo implementation of coroutines
  (define thd-queue (make-queue))
  (define (spawn thunk)
    (let ([cc (current-continuation)])
      (cond
        [(continuation? cc)
         (enqueue! thd-queue cc)]
        [(eq? cc 'start)
         (thunk)
         ; if the current thread finished, we just
         ; transfer to next thread
         (start)])))
  (define (yield)
    (let ([cc (current-continuation)])
      (cond
        [(continuation? cc)
         ; switch context
         (enqueue! thd-queue cc)
         (start)]
        [(eq? cc 'start)
         ; thread resumed
         (void)])))
  (define (start)
    ; if we have a thread, we start it,
    (when (not (queue-empty? thd-queue))
      (let ([thd (dequeue! thd-queue)])
        (thd 'start))))

  (spawn (lambda ()
           (for ([i (range 1 5)])
             (printf "thd1:~a "i)
             (yield))))

  (spawn (lambda ()
           (for ([i (range 1 5)])
             (printf "thd2:~a " i)
             (yield))))
  (start)
  (newline)
  )

(exception)

(generator)

(coroutines)
