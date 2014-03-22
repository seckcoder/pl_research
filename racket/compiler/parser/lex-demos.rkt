#lang racket

(provide (all-defined-out))

; scanner spec
#|
Scanner-spec ::= ({Regexp-and-action}!)
Regexp-and-action ::= (Name ({Regexp}!) Action)
Name ::= Symbol
Regexp ::= String | letter | digit | whitespace | any
       ::=(not Character)|(or {Regexp}!)
       ::=(arbno Regexp)|(concat {Regexp}!)
Action ::= skip | symbol | number | string
|#
(define general-spec
  '((white-sp (whitespace) skip)
    (comment (";" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)
    ))
