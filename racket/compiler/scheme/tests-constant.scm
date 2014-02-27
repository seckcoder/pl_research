(add-tests-with-string-output "constant"
  ;['(1 . 2) => "(1 . 2)\n"]
  [(let ([f (lambda ()
              '(1 . 2))])
     (eq? (f) (f))) => "#t\n"]
  )
