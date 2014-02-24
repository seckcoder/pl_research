#lang racket

#|(print-gensym #f)
(print-gensym 'pretty/suffix)|#

; Note there should be only one exception in syntax-transformer's body

(define-syntax let1
  (syntax-rules ()
    [(_ ((x* e*) ...) body body* ...)
     ((lambda (x* ...)
        body body* ...)
      e* ...)]))


(define-syntax my-let*
  (syntax-rules ()
    [(_ (() ...) body body* ...)
     (let1 ()
        body body* ...)]
    [(_ ((x* e1*) (y* e2*) ...) body body* ...)
     (let1 ((x* e1*))
       (my-let* ((y* e2*) ...) body body* ...))]))

(define-syntax my-and
  (syntax-rules ()
    [(_) #t] ; so empty evaluate to true
    [(_ exp) exp]  ; return last values if all true
    [(_ exp exp* ...)
     (if exp
       (my-and exp* ...)
       #f)]))

(define-syntax my-or
  (syntax-rules ()
    [(_) #f]
    [(_ exp) exp]  ; this line is not helpful funtionally. But it's suggested to be kept here  ; this line is not helpful funtionally.
                   ; But it's suggested to be kept here.
                   ; Reason: see the difference of the expanded macro for the code:
                   ; (if (zero? 5) 3 (or (foo 4)))
                   ; in both conditions.
                   ; : Actually, it's an tail call optimization for scheme compilers that are not clever enough.
    [(_ exp exp* ...)
     (let ((v exp))  ; avoid exp evaluated twice
       (if v
         v
         (my-or exp* ...)))]))

(define-syntax check
  (syntax-rules ()
    [(_ pred a b)
     (if (not (pred a b))
       (error 'check "~s not ~s ~s" `a `pred `b)
       'ok)]))


(check eq? (let1 ((x 3))
                 x
                 (+ x 1))
           4)


(check eq? (my-let* () 
                    (+ 3 1))
           4)

(check eq? (my-let* ((x 1)
                     (y x))
                    (+ 3 y))
           4)

(check eq? (my-and 1 2 #f 3) #f)
(check eq? (my-and 1 2 3) 3)

(check eq? (my-and (display "hi") (zero? 3) (display "there"))
           #f)

(check eq? (my-or 1 #f) 1)
(check eq? (my-or #f #f 1) 1)
(check eq? (my-or #f #f #f) #f)
(check eq? (my-or 1) 1)
(check eq? (my-or #f) #f)

(check eq? (let ([t 5])
             (or #f t)) 5) ; why? the power of hygenic macro

; this case is interesting as we change the binding of if in the let.
; for tranditional gensym way in other macro system
; this case can't be handled.
(check eq? (let ([if (lambda (x y z) #f)])
             (or #f 4)) 4)

; introduce variables in macro

(define-syntax fresh
  (syntax-rules ()
    ((_ (x) g)
     (let ((x 'x))
       (g)))))

(fresh (x)
  (lambda ()
    (check eq? x 'x)))

; what's the result of the following program?

#|(fresh (v)
  (lambda ()
    (check eq? x 'x)))|#


; transform a ... to list
(define-syntax foo
  (syntax-rules ()
    [(_ (a b) ...)
     (list
       (list a ...)
       (list b ...))]))

(foo (1 2))


; transform a pair to hash
(define-syntax gen-pairs
  (syntax-rules ()
    [(_)
     (list)]
    [(_ (op0 p* ...) pair* ...)
     (cons
       ; quasiquote makes op0 not evalauted(return as symbol)
       (cons `op0 (lambda ()
                    p* ...))
       (gen-pairs pair* ...))]))

(define-syntax biop-emit-pairs
  (syntax-rules ()
    [(_ p0 p* ...)
     (make-hasheq
       (gen-pairs p0 p* ...))]))

(biop-emit-pairs
  [* (display "*")]
  [- (display "-")])

(define-syntax for
  (syntax-rules (in)
    ((for element in list body ...)
     (map (lambda (element)
            body ...)
          list))))

(for i in '(0 1 2 3 4) (print i))
; syntax-case...
