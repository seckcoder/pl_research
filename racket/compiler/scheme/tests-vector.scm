(add-tests-with-string-output "vector"
  [(let ([vec (make-vec 5)])
     (vec-set! vec 0 1)
     (vec-ref vec 0)
     ) => "1\n"]
  [(let ([vec (make-vec 5)])
     (vec-set! vec 2 3)
     (vec-ref vec 2)
     ) => "3\n"]
  [(let ([vec (make-vec 5)])
     (vec-set! vec 4 5)
     (vec-ref vec 4)
     ) => "5\n"]
  [(let ([vec (vec 1 2 3 4 5)])
     (vec-ref vec 0)) => "1\n"]
  [(vec-ref (vec 1 2) 1) => "2\n"]
  )
