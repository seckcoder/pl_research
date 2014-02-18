#lang eopl

(require "interp.rkt")
(require "grammar.rkt")
(require "../base/utils.rkt")

(define test-prog
  (lambda (prog)
    (interp (scan&parse prog))))

(define eval-prog
  (lambda (prog)
    (display (test-prog prog))))

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

   ; non-cooperating threads
   (test-prog-eqv "letrec
                    noisy (l) = if null?(l)
                                then 0
                                else {
                                  print(car(l));
                                  (noisy cdr(l));
                                }
                   in {
                    spawn(proc (d) (noisy list(1 2 3 4 5)));
                    spawn(proc (d) (noisy list(6 7 8 9 10)));
                    print(100);
                    33;
                   }"
                  33)

   ; producer-consumer
   (test-prog-eqv "let buffer = 0
                   in let producer = proc(n)
                                      letrec
                                        busywait(k) = if zero?(k)
                                                  then set buffer = n
                                                  else {
                                                    print(-(k,-200));
                                                    (busywait -(k,1));
                                                  }
                                      in (busywait 5)
                      in let consumer = proc(id)
                                          letrec busywait(k) = if zero?(buffer)
                                                               then {
                                                                print(-(k,-100));
                                                                (busywait -(k,-1));
                                                               }
                                                               else buffer
                                          in (busywait 0)
                         in {
                          spawn(proc(d) (producer 44));
                          print(300);
                          (consumer 86);
                         }"
                    44)
   
   ; 5.51 producer-consumer without busy-wait.
   (test-prog-eqv 
"
   let buffer = 0 
   in let mtx = mutex()
      in let producer = proc(n) {
                          wait(mtx);
                          letrec
                            busywait(k) = if zero?(k)
                                           then  {
                                              set buffer = n;
                                              signal(mtx);
                                           } else {
                                              print(-(k, -200));
                                              (busywait -(k,1));
                                           }
                          in (busywait 5);
                       }
         in let consumer = proc(id) 
                            letrec sleepwait(k) = if zero?(buffer)
                                                  then {
                                                    print(-(k,-100));
                                                    wait(mtx);
                                                    % keep releasing the mutex
                                                    % if the producer hasn't gotten it. 
                                                    if zero?(buffer)
                                                    then {
                                                      signal(mtx);
                                                      (sleepwait -(k,-1));
                                                    } else {
                                                      signal(mtx);
                                                      buffer;
                                                    };
                                                  } else buffer
                             in (sleepwait 0)
             in {
              spawn(proc (d) (producer 44));
              print(300);
              (consumer 86);
             }
"
   44)
                            
   ; a test of shared variable problem
   (test-prog "let x = 0
               in let incr = proc(id)
                      proc (dummy) {
                        set x = -(x,-1);
                        print(id);
                        print(x);
                        set x = -(x,-1);
                        print(id);
                        print(x);
                        set x = -(x,-1);
                        print(id);
                        print(x);
                        set x = -(x,-1);
                        print(id);
                        print(x);
                        set x = -(x,-1);
                        print(id);
                        print(x);
                        set x = -(x,-1);
                        print(id);
                        print(x);
                        set x = -(x,-1);
                        print(id);
                        print(x);
                        set x = -(x,-1);
                        print(id);
                        print(x);
                        set x = -(x,-1);
                        print(id);
                        print(x);
                        set x = -(x,-1);
                        print(id);
                        print(x);
                      }
                  in {
                    spawn((incr 100));
                    spawn((incr 200));
                    spawn((incr 300));
                  }")
      
    (test-prog "let mtx = mutex()
               in let x = 0
                 in let incr = proc(id)
                        proc (dummy) {
                          wait(mtx);
                          set x = -(x,-1);
                          print(id);
                          print(x);
                          set x = -(x,-1);
                          print(id);
                          print(x);
                          set x = -(x,-1);
                          print(id);
                          print(x);
                          set x = -(x,-1);
                          print(id);
                          print(x);
                          set x = -(x,-1);
                          print(id);
                          print(x);
                          set x = -(x,-1);
                          print(id);
                          print(x);
                          set x = -(x,-1);
                          print(id);
                          print(x);
                          set x = -(x,-1);
                          print(id);
                          print(x);
                          set x = -(x,-1);
                          print(id);
                          print(x);
                          set x = -(x,-1);
                          print(id);
                          print(x);
                          signal(mtx);
                        }
                    in {
                      spawn((incr 100));
                      spawn((incr 200));
                      spawn((incr 300));
                    }")
  ; 5.52; There are better methods to be this. But it needs modify 
  ; the interpreter
  (test-prog-eqv
"
let x = 0
in let threadnum = 3
   in let threadcount = 0
        in let incmtx = mutex()
             in let mainmtx = mutex()
                 in let incrx = proc(id)
                                  proc(dummy) {
                                    wait(incmtx);
                                    set x = -(x,-1);
                                    set x = -(x,-1);
                                    set x = -(x,-1);
                                    set x = -(x,-1);
                                    set x = -(x,-1);
                                    set x = -(x,-1);
                                    set x = -(x,-1);
                                    set x = -(x,-1);
                                    set x = -(x,-1);
                                    set x = -(x,-1);
                                    set threadcount = -(threadcount,-1);
                                    print(id);
                                    print(x);
                                    print(threadcount);
                                    signal(incmtx);
                                    if zero?(-(threadcount,threadnum))
                                    then signal(mainmtx)
                                    else x;
                                  }
                    in {
                      wait(mainmtx);
                      print(111111111);
                      spawn((incrx 100));
                      spawn((incrx 200));
                      spawn((incrx 300));
                      wait(mainmtx);
                      signal(mainmtx);
                      print(111111111);
                      x;
                    }"
  30)
)
