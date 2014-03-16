; a scheme interpreter that supports oo
#lang racket

(require eopl/datatype
         (prefix-in env: "../compiler/scheme/env.rkt")
         "../c5_43/store.rkt"
         "../base/utils.rkt"
         "global.rkt"
         "parser.rkt")


(define (interp-exps-return-list exps env)
  (map
    (lambda (exp)
      (interp-exp exp env))
    exps))

(define (interp-exps-return-last exps env)
  (foldl
    (lambda (exp _)
      (interp-exp exp env))
    '()
    exps))

(define-datatype
  procedure procedure?
  (closure
    (vars (list-of symbol?))
    (body expression?)
    (env env:env?)
    )
  (method
    (vars (list-of symbol?))
    (body expression?)
    (super-name super-class-name?) ; super class name of host class
    ))

(define (apply-proc proc rands)
  (cases
    procedure proc
    (closure
      (vars body env)
      (interp-exp
        body
        (env:exts env vars (map newref rands))))
    (method
      (vars body super-name)
      (match rands
        [(list self rands ...)
         (interp-exp
           body
           (~> (env:empty)
               (lambda (env)
                 ; rands
                 (env:exts env vars (map newref rands)))
               (lambda (env)
                 ; self and super
                 (if (null? super-name)
                   (env:ext env 'self (newref self)) ; no super class
                   (env:exts env
                             (list 'self 'super)
                             (list (newref self)
                                   (newref (find-class super-name))))))
               (lambda (env)
                 ; fields
                 (env:ext-env
                   env
                   (object-fields-env self)))))]))))

(define (interp-exp exp env)
  (cases expression exp
    (const-exp
      (cst) cst)
    (var-exp
      (var)
      (deref (env:app env var)))
    (quote-exp
      (sexp) sexp)
    (op-exp
      (op rands)
      (apply (eval op (make-base-namespace)) (interp-exps-return-list rands env)))
    (if-exp
      (test then else)
      (if (interp-exp test env)
        (interp-exp exp then env)
        (interp-exp exp else env)))
    (lambda-exp
      (params body)
      (closure params body env))
    (compound-exp
      (exps)
      (interp-exps-return-last exps env))
    (set-exp
      (var val)
      (setref!
        (env:app env var)
        (interp-exp val env)))
    (method-call-exp
      (obj-exp method-name rands)
      (let ([obj (interp-exp obj-exp env)])
        (apply-proc
          (find-method
            (object-class-name obj)
            method-name)
          (cons
            obj
            (interp-exps-return-list rands env)))))
    (super-call-exp
      (method-name rands)
      (let ([super-class (deref (env:app env 'super))]
            [self (deref (env:app env 'self))])
        (apply-proc
          (find-method
            (class-name super-class)
            method-name)
          (cons
            self
            (interp-exps-return-list rands env)))))
    (new-exp
      (class-name rands)
      (let* ([cls (find-class class-name)]
             [obj (new-object cls)])
        (apply-proc
          (find-method-from-class cls 'initialize)
          (cons obj rands))
        obj))
    (call-exp
      (rator rands)
      (apply-proc
        (interp-exp rator env)
        (interp-exps-return-list rands env)))
    ))

(define-datatype
  class class?
  (a-class
    (class-name symbol?)
    (super-name super-class-name?)
    (field-names (list-of symbol?))
    (method-decls (list-of method-binding?)))
  (root-class ; class that has no parent
    (class-name symbol?)
    (field-names (list-of symbol?))
    (method-decls (list-of method-binding?)))
  )

(define (class-method-decls cls)
  (cases class cls
    (a-class
      (class-name super-name field-names method-decls)
      method-decls)
    (root-class
      (class-name fieldnames method-decls)
      method-decls)
    ))
(define (class-super-name cls)
  (cases class cls
    (a-class
      (class-name super-name field-names method-decls)
      super-name)
    (root-class
      (class-name fieldnames method-decls)
      (error 'class-super-name "no super class for class:~a" class-name))
    ))
(define (class-name cls)
  (cases class cls
    (a-class
      (class-name super-name field-names method-decls)
      class-name)
    (root-class
      (class-name fieldnames method-decls)
      class-name)
    ))

(define-datatype
  object object?
  (a-object
    (class-name symbol?)
    (fields (list-of reference?))))

(define (object-class-name obj)
  (cases object obj
    (a-object
      (class-name fields)
      class-name)))

(define (new-object cls)
  (a-object
    (class-name cls)
    (map
      (lambda (_)
        (newref 'uninitialized-field))
      (class-field-names-all cls))))

(define (object-fields-env obj)
  (cases
    object obj
    (a-object
      (class-name fields)
      (env:init
        (class-field-names-all
          (find-class class-name))
        fields))))

; super-fields ... fields
(define (class-field-names-all cls)
  (cases class cls
    (a-class
      (class-name super-name field-names method-decls)
      (append (class-field-names-all (find-class super-name))
              field-names))
    (root-class
      (class-name field-names method-decls)
      field-names)))


(define (method-binding? l)
  (and (pair? l)
       (symbol? (car l))
       (procedure? (cadr l))))

(define (interp-decls decls)
  (for-each
    (lambda (decl)
      (interp-decl decl))
    decls))

(define (interp-decl decl)
  (cases declaration decl
    (class-decl
      (cls-name super-name field-names method-decls)
      (ext-class-env! cls-name
                      (newref
                        (let ([method-decls
                                (map
                                  (lambda (meth-decl)
                                    (cases
                                      declaration meth-decl
                                      (method-decl
                                        (method-name vars body)
                                        (list method-name
                                              (method vars body super-name)))
                                      (else
                                        (error 'declaration "not a method declaration"))))
                                  method-decls)])
                          (if (eq? cls-name 'object)
                            (root-class cls-name
                                        field-names
                                        method-decls)
                            (a-class cls-name
                                     super-name
                                     field-names
                                     method-decls))))))
    (else
      (error 'declaration "not a class declaration"))))

(define (interp-prog prog)
  (initialize-store!)
  (cases program prog
    (a-program
      (class-decls body)
      (interp-decls class-decls)
      (interp-exp body (env:empty)))))

(define (find-class name)
  (deref (app-class-env name)))

(define (find-method-from-class cls method-name)
  (let* ([method-pair
           (assq 
             method-name
             (class-method-decls cls))]) 
    (if method-pair
      (cadr method-pair)
      (if (eq? class-name 'object)
        (error 'find-method "class:~a has not method:~a" class-name method-name)
        ; inheritance
        (find-method (class-super-name cls) method-name)
        ))))

(define (find-method class-name method-name)
  (find-method-from-class (find-class class-name) method-name))

(module+ test
  (require rackunit)
  (define (demo-interp class-decls exp)
    (interp-prog (parse-prog
                   (cons '(class object extends ()
                            (method initialize ()
                                    (void)))
                         class-decls)
                   exp)))
  (define (test-interp class-decls exp val msg)
    (check-equal? (demo-interp class-decls exp) val msg))
  (test-interp
    (list
      '(class c extends object
         (method m () 1)))
    '(let ([o (new c)])
       (send o m))
    1
    "basic class declaration"
    )

  (test-interp (list
                 '(class c1 extends object
                    (method initialize ()
                            1)
                    (method m1 ()
                            11)
                    (method m2 ()
                            (send self m1)))
                 '(class c2 extends c1
                    (method m1 ()
                            22)))
               '(let ([o1 (new c1)]
                      [o2 (new c2)])
                  (list
                    (send o1 m1)
                    (send o2 m1)
                    (send o2 m2)))
               '(11 22 22)
               "dynamic dispatch for self"
               )

  (test-interp (list
                 '(class c1 extends object
                    (method m1 ()
                            (send self m2))
                    (method m2 ()
                            'c1:m2))
                 '(class c2 extends c1
                    (method m1 ()
                            'c2:m1)
                    (method m2 ()
                            'c2:m2)
                    (method m3 ()
                            (super m1))
                    (method m4 ()
                            (super m2))
                    )
                 '(class c3 extends c2
                    (method m1 ()
                            'c3:m1)
                    (method m2 ()
                            'c3:m2)))
               '(let ([o3 (new c3)])
                  (list (send o3 m3)
                        (send o3 m4)))
               (list 'c3:m2 'c1:m2)
               "static dispatch for super"
               )
  )
