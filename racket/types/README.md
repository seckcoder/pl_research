The general idea for type checker:
--------

Type checker takes a top-down approach. To get the
type of current expression, we need the type of sub expressions and
then infer the type of current expression. 

The instinct behind the type checker is:
when we find it hard or impossible to infer, we require an explicit type
specification; when we find we have redundant rules, we use them to verify
the specified type by programmer.

In the type-checker language, we ask you to specify the type for lambda expression
and letrec.

```
; It's hard and sometimes imporssible to infer type for var
(lambda ((var var-type) ...) body)

; It's impossible to infer type for nonterminating
; recursive function.
;(letrec (((funcname rettype) lambda-exp)) body)

```

For an improved type inferer(with less bugs), refer to `module`
