
#|(add-tests-with-string-output "proc"
  [(labels ([f (proc (v) v)])
     (app-proc f 3)) => "3\n"]
  [(labels ([f (proc (a b) (+ a b))])
     (app-proc f 3 4)) => "7\n"]
  [(labels ([f1 (proc (a) a)]
            [f2 (proc (a b) (+ a (app-proc f1 b)))])
     (app-proc f2 3 4)) => "7\n"])|#

#|(add-tests-with-string-output "closure"
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
  )|#

#|(add-tests-with-string-output "non-tail-call"
  [(+ 2 ((lambda (v) v) 3)) => "5\n"]
  [(let ((f (lambda (v) v)))
     (+ (f 3) 2)) => "5\n"]
  )|#

(add-tests-with-string-output "tail-call"
  ;[(+ 1 2) => "3\n"])
  ;[((lambda (v) v) 3) => "3\n"])
  #|[(let ((f (lambda (v) v)))
     (f 3)) => "3\n"])|#
  #|[(let* ((f (lambda (v) v))
          (g (lambda (v)
               (f v))))
     (g 3)) => "3\n"]|#
  [(let ((f (lambda (v)
              (
