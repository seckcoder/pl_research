(add-tests-with-string-output "constant"
  ;['(1 . 2) => "(1 . 2)\n"]
  #|[(let ([f (lambda ()
              '(1 . 2))])
     (eq? (f) (f))) => "#t\n"]|#
  ;[''1 => "1\n"]
  )

(add-tests-with-string-output "string"
  ["abc" => "abc\n"]
  [(make-string 5) => "\n"]
  [(string #\a #\b #\c) => "abc\n"]
  [(string-ref "ab" 0) => "#\\a\n"]
  [(let ([s "abc"])
     (string-set! s 1 #\d)
     s) => "adc\n"]
  )
