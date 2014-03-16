#lang racket

(require eopl/datatype
         "../base/utils.rkt"
         "../cps/builtin.rkt")

(provide (all-defined-out))

(define (const? v)
  (or (number? v)
      (string? v)
      (char? v)
      (boolean? v)))

(define-datatype
  expression expression?
  (const-exp
    (cst const?))
  (var-exp
    (var symbol?))
  (quote-exp
    (sexp sexp?))
  (op-exp
    (op op?)
    (params (list-of expression?)))
  (call-exp
    (rator expression?)
    (rands (list-of expression?)))
  (if-exp
    (test expression?)
    (then expression?)
    (else expression?))
  (lambda-exp
    (vars (list-of symbol?))
    (body expression?))
  (compound-exp
    (exps (list-of expression?)))
  (set-exp
    (var symbol?)
    (val expression?))
  (new-exp
    (class-name symbol?)
    (rands (list-of expression?)))
  (method-call-exp
    (obj-exp expression?)
    (method-name symbol?)
    (rands (list-of expression?)))
  (super-call-exp
    (method-name symbol?)
    (rands (list-of expression?)))
  )

(define (super-class-name? v)
  (or (symbol? v)
      (null? v)))

(define-datatype
  declaration declaration?
  (class-decl
    (cls-name symbol?)
    (super-name super-class-name?)
    (field-names (list-of symbol?))
    (method-decls (list-of declaration?)))
  (method-decl
    (method-name symbol?)
    (vars (list-of symbol?))
    (body expression?)))

(define-datatype
  program program?
  (a-program
    (decls (list-of declaration?))
    (exp expression?)))


(define (single-or-compound exps)
  (if (null? (cdr exps))
    (car exps)
    `(begin
       ,@exps)))

(define (parse-exp sexp)
  (match sexp
    [(? const? x) (const-exp x)]
    [(? symbol? x) (var-exp x)]
    ; symbol
    [`(quote ,x) (quote-exp x)]
    ; builtin ops
    [(list (? op? op) params* ...)
     (op-exp op (map parse-exp params*))]
    ; if 
    [`(if ,test ,then)
      (parse-exp `(if ,test
                ,then
                (void)))]
    [`(if ,test ,then ,else)
      (if-exp (parse-exp test)
              (parse-exp then)
              (parse-exp else))]
    ; lambda
    [`(lambda (,params ...) ,body ,bodies* ...)
      (lambda-exp params
                  (parse-exp (single-or-compound (cons body bodies*))))]
    [`(begin ,body ,bodies* ...)
      (compound-exp (map parse-exp (cons body bodies*)))]
    [`(let ((,var ,val) ...) ,body ,bodies* ...)
      (parse-exp `((lambda (,@var)
                 ,@(cons body bodies*))
               ,@val))]
    [`(set! ,var ,val)
      (set-exp var
               (parse-exp val))]
    [`(new ,class-name ,rand* ...)
      (new-exp class-name
               (map parse-exp rand*))]
    [`(send ,obj-exp ,method-name ,rand* ...)
      (method-call-exp (parse-exp obj-exp)
                       method-name
                       (map parse-exp rand*))]
    [`(super ,method-name ,rand* ...)
      (super-call-exp method-name
                      (map parse-exp rand*))]
    ; procedure call
    [(list rand rators ...)
     (call-exp (parse-exp rand)
               (map (lambda (rator)
                      (parse-exp rator))
                    rators))]
    ))

(define (parse-decl decl)
  (match decl
    [`(class ,cls-name extends ,super-name
        ,attr-decl* ...)
      (let ([attr-decls (map parse-decl attr-decl*)])
        (class-decl cls-name super-name
                    (map cadr
                         (filter (lambda (decl)
                                   (tagged-list? decl 'field))
                                 attr-decls))
                    (map
                      (match-lambda
                        [`(method ,method-name ,vars ,body)
                          (method-decl method-name vars body)])
                      (filter (lambda (decl)
                                (tagged-list? decl 'method))
                              attr-decls))))]
    [`(field ,v)
      `(field, v)]
    [`(method ,method-name (,vars ...) ,body* ...)
      `(method ,method-name ,vars
               ,(parse-exp (single-or-compound body*)))]
    ))

(define (parse-prog decls exp)
  (a-program (map parse-decl decls)
             (parse-exp exp)))
