(add-tests-with-string-output "gc"
  [(let ([v1 (vec 1 1)])
     ; v1 is 16 bytes
     (let ([v2 (vec 2)])
       ; v2 is 8 bytes
       v2)
     (vec 3)) => "1: [3]\n"]
  #|[(let ([v1 (vec 7 2 3 10 43)]
         [heap-size (fx* 4 (fx* 16 4096))])
     (let ([v2 (make-vec
                 (- (quotient heap-size 2)
                    (* 2 (length v1))))])
       v2)
     (let ([v3 (make-vec (length v1))])
       (+ (length v1)
          (length v3)))) => "10\n"]|#
  )
