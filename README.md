Experiments for programming language.

It contains or will contain:

- interpreters
  * call-by-name/call-by-value/call-by-need
  * type check and type inference(HM style)
  * thread simulation based on interpreters
  * erlang's actor model simulation based on interpreters
  * cps style interpreter and cps transformation for lambda expression
  * exception
  * fast interpreter based pre static analysis
  * a minikanren interpreter
- compiler
  * seck-scheme: a scheme compiler that compiles to x86
    - basic data type: fixnumber, char, bool, string, vector
    - closure support
    - tail call optimization
    - TODO: GC, Optimization based on cps transformation
  * a scheme compiler compiles to C
- fp and others
  * monad experiments
  * macro experiments
  * pure functional implementation of various data structures

elegant-weapons
---------
elegant-weapons in `./lib` is by [eholk](https://github.com/eholk/elegant-weapons)

copyright (c) 2011-2013 the trustees of indiana university and indiana
university research and technology corporation.  all rights reserved.

minikanren
----------
The minikanren interpreter is based on the code of [The Reasoned Schemer](http://mitpress.mit.edu/books/reasoned-schemer)
