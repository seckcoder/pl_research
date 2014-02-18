
#|(add-tests-with-string-output "proc"
  [(labels ([f (proc (v) v)])
     (app-proc f 3)) => "3\n"]
  [(labels ([f (proc (a b) (+ a b))])
     (app-proc f 3 4)) => "7\n"]
  [(labels ([f1 (proc (a) a)]
            [f2 (proc (a b) (+ a (app-proc f1 b)))])
     (app-proc f2 3 4)) => "7\n"])|#

(add-tests-with-string-output "closure"
  [(let ((u 3))
     (app (lambda (v)
            (+ v u)) u)) => "6\n"]
  [(let* ((u 3)
          (f (lambda (v)
              (+ v u))))
     (f 3)) => "6\n"]
  [((lambda (v)
      (app
        (lambda (x)
          (+ x v))
        v)) 3) => "6\n"]
  )
