#lang eopl

(require "interp.rkt")
(require "grammar.rkt")
(require "../base/utils.rkt")

(define test-prog
  (lambda (prog)
    (interp (scan&parse prog))))

(define test-prog-eqv 
  (lambda (prog v)
    (check eq? (interp (scan&parse prog)) v)))

(define test-prog-equalv
  (lambda (prog v)
    (check equal? (interp (scan&parse prog)) v)))


(define (test)
  (test-prog-eqv "let f = proc (x) -(x,11)
                 in (f (f 77))"
                 55)
  (test-prog-eqv "(proc (f) (f (f 77))
                   proc (x) -(x,11))"
                 55)
  
  ; the following example is used to identify scoping of the proc-lang.
  ; if it's dynamic scoping, then the result will be 1, else result will be
  ; 2 with lexical scoping
  (test-prog-eqv "let f = let x = 3
                  in proc (y) -(y,x)
                    in let x = 4
                        in (f 5)"
                 2)
  (test-prog-eqv "letrec double(x) = if zero?(x)
                                     then 0
                                     else -((double -(x,1)), -2)
                  in (double 4)"
                  8)
  
  ; test list
  (test-prog-equalv "let x = 4
                     in cons(x, 
                             cons(cons(-(x,1),
                                       emptyList),
                                  emptyList))"
                  '(4 (3)))
  (test-prog-equalv "list()"
                    '())
  (test-prog-equalv "list(1 2 3 list(2 3) 4 5)"
                    '(1 2 3 (2 3) 4 5))
  ; test implicit ref and call by value
  (test-prog-eqv "let p = proc(x) set x = 4
                  in let a = 3
                     in {
                      (p a);
                      a;
                  }"
                  3)

   ; fact
   (test-prog-eqv "letrec fact(n) = if zero?(n)
                                    then 1
                                    else *(n, (fact -(n, 1)))
                   in (fact 4)"
                   24)
  )
