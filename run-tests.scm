(import
  (elegant-weapons print-c)
  (elegant-weapons parse-c)
  (elegant-weapons tester)

  (elegant-weapons helpers-tests)
  (elegant-weapons sets-tests)
  (elegant-weapons record-case-tests)
  (c311 a1)
  (eopl c1)
  (eopl c2)
  (eopl c2-stack)
  (eopl c2-16-lambda-exp)
  (eopl c2-20)
  )

(define (print-parse-roundtrip e)
  (display "Round trip testing for...\n")
  (display e)(newline)(newline)
  (let ((s (format-c e)))
    (display "Formatted C...\n")
    (display s)(newline)(newline)
    (let ((tokens (tokenize-string s)))
      (display "Tokens...\n")
      (display tokens)(newline)(newline)
      (let ((parsed (parse-c tokens)))
        (display "Parsed...\n")
        (display parsed)(newline)(newline)
        (if (equal? e parsed)
          (display "Success!\n")
          (error 'print-parse-roundtrip
                 "Parser round trip test failed."))))))

(define (parse-roundtrip e error)
  (let ((s (format-c e)))
    (let ((tokens (tokenize-string s)))
      (let ((parsed (parse-c tokens)))
        (unless (equal? e parsed)
          (error))))))

(define-test-suite
  basic
  (simple (lambda (error) #t))
  (parser-roundtrip-1 (lambda (error)
                        (parse-roundtrip '((func int main ()
                                                 (return (int 0))))
                                         error)))
  (parser-roundtrip-2 (lambda (error)
                        (parse-roundtrip
                          '((func int main ()
                                  (let x int (int 0))
                                  (return (var x))))
                          error))))

(run-tests basic helpers sets record-case-tests
           a1 eopl-c1 eopl-c2 eopl-stack eopl-c2-16
           eopl-c2-20)
