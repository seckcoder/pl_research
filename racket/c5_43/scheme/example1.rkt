#lang racket
(require racket/pretty
         "interp.rkt"
         "../../parser.rkt"
         "../../base/utils.rkt")


(define (example1)
  ; Q: what's the value of cc?
  ; A: cc is the continuation of the `then` expression in `if`,
  ;    it also equals the continuation of `if` expression,
  ;    therefore it's the continuation after if
  (meval '(let ((start #f))
            (if (not start)
              (call/cc (lambda (cc)
                         (set! start cc))))
            (display "Going to invoke (start)\n")
            (start)))
  )

(define (example2)
  ; Q: what's the value of cc?
  ; A: cc = k because in call/cc the argument pass to `k`
  ;    is regarded as the result of evaluating call/cc
  (meval '((lambda (cc)
             cc)
           (call/cc (lambda (k)
                      (k k)))))
  )

(define (example3)
  ; Q: how the code runs?
  ; A: cc send the argument as the result of evaluating call/cc,
  ;    therefore, after call: `(cc 1) 1 is returned as the result of call/cc,
  ;    ie, the result of current-continuation
  (define (current-continuation)
    (call/cc (lambda (cc) (cc cc))))

  (let ((cc (current-continuation)))
    (printf "~v\n" cc)
    (cond ((continuation? cc)
           (cc 1))
          ((= cc 1) (println "bingo"))
          (else (println "contract violation")))
    ))

(define (example4)
  (meval
    '(let ((current-continuation (lambda ()
                                   (call/cc (lambda (cc)
                                              (cc cc))))))
       (let ((cc (current-continuation)))
         (printf "~v\n" cc)
         (cond ((number? cc) (print "bingo"))
               (else
                 (cc 1))))
       )
    ))

(define (example5)
  ; go-when
  (let ((right-now (lambda ()
                     (call/cc (lambda (cc)
                                (cc cc)))))
        (go-when (lambda (then)
                   (then then))))
    ; Q: How the code runs?
    ; A: It implements an infinite loop. right-now returns the current continuation,
    ;    go-when makes the continuation returned as the result of continuation.
    ;    So when go-when called, it returns to the point of assigning result of `(right-now)
    ;    to `moment`. At the same time, moment is always continuation since it passes
    ;    continuation as argument(ie, return continuation as result of call/cc)
    (let ((moment (right-now)))
      (display "hello, world")
      (newline)
      (go-when moment))
    ))

(define (demo-dynamic-wind)
  (define (demo1)
    ; Q: What's dynamic-wind?
    ; A: syntax: (dynamic-wind pre thunk after) with pre, thunk, after all
    ;   being a procedure with no args. After calling (dynamic-wind ...),
    ;   pre is first called, then thunk called, then after called, with thunk's return
    ;   value as return of dynamic-wind. But it makes
    ;   sure that when delimited control jumps into/out of the thunk(continuation
    ;   invocation or other examples): the pre/after will be called correspondingly.
    ; Q: explain the example
    ; A: out is a continuation that assigns a value to v and call body of let.
    ;    At first, after we call call/cc, we enter into dynamic wind, so pre and
    ;    thunk are invoked. In body of thunk, we invoke another call/cc, with continuation:k
    ;   of the rest thunk body as input of out, so v = k, and we then enter into let's body
    ;   and call: (k "post"). As we leave the thunk, we have to call after. After that,
    ;   we pass "post" to display, and return #f. At the same time, we enter into the thunk,
    ;   so we have to call pre. As we leave thunk the second time, we call after.
    ;   The final result is:"in pre out in post out"
    ;   
    (let ([v (call/cc (lambda (out)
                        (dynamic-wind
                          (lambda ()
                            (display "in "))
                          (lambda ()
                            (display "pre ")
                            (display (call/cc out)) ; (out k)
                            #f)
                          (lambda ()
                            (display "out ")))))])
      (when v
        (v "post "))))
  ;(demo1)
  (define (demo2)
    ; Why?
    (call/cc (lambda (k0)
               (call/cc (lambda (k1)
                          (dynamic-wind
                            (lambda () (display "in "))
                            (lambda ()
                              (k1 'cancel)
                              )
                            (lambda ()
                              (k0 'cancel-canceled)))))))
    )
  (demo2)
  )

  (define (current-continuation)
    (call/cc (lambda (k)
               (k k))))
(define (exception-right)
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

  ; tree-iterator : tree -> generator
  (define (tree-iterator tree)
    (lambda (yield)

      ;; Walk the tree, yielding the leaves.

      (define (walk tree)
        (if (not (pair? tree))
          (yield tree)
          (begin
            (walk (car tree))
            (walk (cdr tree)))))

      (walk tree)))

  (let ([iter (list-iter '(1 2 3))])
    (mfor v in iter
      (printf "~a " v))
    (printf "\n")

    (mfor v in iter
      (printf "~a " v)))
  )

(require data/queue)
(define (coroutines)
  #|The API for cooperative multithreading has five functions:
  (spawn thunk) puts a thread for thunk into the thread queue.
  (quit) kills the current thread and removes it from the thread queue.
  (yield) hands control from the current thread to another thread.
  (start-threads) starts executing threads in the thread queue.
  (halt) exits all threads.|#
  
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

(coroutines)
