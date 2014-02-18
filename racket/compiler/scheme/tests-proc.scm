
#|(add-tests-with-string-output "proc"
  [(labels ([f (proc (v) v)])
     (app-proc f 3)) => "3\n"]
  [(labels ([f (proc (a b) (+ a b))])
     (app-proc f 3 4)) => "7\n"]
  [(labels ([f1 (proc (a) a)]
            [f2 (proc (a b) (+ a (app-proc f1 b)))])
     (app-proc f2 3 4)) => "7\n"])|#

#|(add-tests-with-string-output "closure"
  [(let ([v 3])
     ((lambda (u)
        (+ v u)) v)) => "6\n"])|#


(add-tests-with-string-output "closure"
  [(labels
     ((f (proc (env u) u)))
     (app (closure f (vec)) 3))
   => "3\n"]
  [(let ((u 3))
     (app (lambda (v)
            (+ v u)) u)) => "6\n"])

#|(add-tests-with-string-output "closure->proc"
  [(labels
    ((proc_162 (proc (env163 u)
                     (+ (vec-ref env163 0) u))))
    (let ((v 3))
      (app-proc proc_162 (vec v) v)))
      ;(app (closure proc_162 (vec v)) v)))
   => "6\n"])|#
