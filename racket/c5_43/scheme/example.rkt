#lang racket
(require racket/pretty
         "interp.rkt"
         "../../parser.rkt"
         "../../base/utils.rkt")

; Fact:
; 1. (call/cc f) evaluates to (f k) with k being the current continuation
; of expression: (call/cc f).  So call/cc also means call f with current
; continuation.
; 2. when k is called in f, it returns immediately, with the value passed to
; k as the return value

; Another view of continuation:
; Think of the interpreters as cpu, and expressions are instructions. Then continuation
; is Program Counter, which points to the next instruction.

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
  ;    The function current-continuation directly returns the current continuation
  ;    as value
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



(define (exception-wrong)
  ; The wrong implementation of exception(without using continuation).
  ; Note even after the exception is caught, the expressions after throw
  ; will still be executed!
  (define except-stack '())
  (define (push-except id handler)
    (set! except-stack (cons (cons id handler) except-stack)))
  (define (except-id except)
    (car except))
  (define (except-handler except)
    (cdr except))
  (define (top-except)
    (car except-stack))
  (define (pop-except id)
    (cond
      [(null? except-stack) (error 'exception "cannot catch exception:~s" id)]
      [(eq? (except-id (top-except)) id)
       (let ([handler (except-handler (top-except))])
         (set! except-stack (cdr except-stack))
         handler)]
      [else
        (set! except-stack (cdr except-stack))
        (pop-except id)]))
  (define-syntax try
    (syntax-rules (catch)
      ((_ exp* ... (catch id handler))
       (begin
         (push-except id handler)
         exp* ...
         ))))
  (define (throw id except-val)
    (let ([handler (pop-except id)])
      (handler except-val)))
  (try
    (throw 'runtimeerror "this is a runtime error")
    (printf "here\n")
    (catch 'runtimeerror (lambda (msg)
                           (printf "~s\n" msg))))
  )

(define (exception-right)
  ; exception implementation based on continuation
  '())

(define (demo-dynamic-wind)
  (define c
    (dynamic-wind
      (lambda () (display 'IN)(newline))
      (lambda () (call/cc
                   (lambda (k)
                     (display 'X)(newline)
                     k)))
      (lambda () (display 'OUT)(newline))))
  (c 1))
;(demo-dynamic-wind)
