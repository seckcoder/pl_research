(load "tests-driver.scm")
#|(load "tests-1.1-req.scm")
(load "tests-1.2-req.scm")|#

(require racket/match
         (prefix-in env: "env.rkt")
         "../../base/utils.rkt"
         "base.rkt"
         "parser.rkt"
         "closure-conversion.rkt"
         "global.rkt")

(define (compile-program x)
  (init-global!)
  (~> x
      parse
      (lambda (e)
        (closure-conversion e 'bottom-up))
      emit-program))

(define (emit-global n code)
  (match code
    [`(proc (,v* ...) ,body)
      (emit "~s:" n)
      (let* ([v*-num (length v*)]
             [env (env:init v*
                            (range (- wordsize)
                                   (- wordsize)
                                   (- (* v*-num wordsize))))])
        ((emit-exp (- (- (* v*-num wordsize))
                      wordsize)
                   env
                   #t) body)
        (emit "   ret")
        )]
    ))

(define (emit-global*)
  (for-each
    (match-lambda
      [(list n code)
       (emit-global n code)])
    *global*))
; lift lambda
(define-syntax def-lifted-lambda
  (syntax-rules ()
    [(_ (lambda (v* ...) body))
     (env!:ext! *proc*
                (gensym 'proc)
                `(proc (v* ...) body))]))

(define (emit-program x)
  (emit "   .text")
  ; We need L_scheme_entry since we need to make sure that when starting
  ; to emit-exp, the value above %esp is a return address. Otherwise,
  ; the tail-optimization will not work.
  (emit-fn-header 'L_scheme_entry)
  ((emit-exp (- wordsize) (env:empty) #t) x)
  (emit "   ret # ret from L_scheme_entry") ; if program is tail-optimized, ret will be ignored
  ;(emit "   ret")
  (emit-fn-header 'scheme_entry)
  (emit-preserve-regs)
  (emit "   movl %esp, %ecx # store esp temporarily")
  ; heap : low->high
  ; stack : high->low
  (emit "   movl 12(%ecx), %ebp # set mem pointer")
  (emit "   movl 8(%ecx), %esp # set stack pointer")
  (emit "   pushl 4(%ecx) # store ctx")
  ; It's an assumption that physical addresses on Intel's
  ; 32bit processors have 8-byte boundaries. So we don't
  ; need to aligh the heap address when start.
  ;(emit-align-heap) ; aligh the start address of heap
  (emit "   call L_scheme_entry")
  (emit-restore-regs)
  (emit "   ret # return from scheme_entry")
  ;(emit-global-proc*)
  )
(define (emit-preserve-regs)
  (define (si-of-i i)
    (* wordsize i))
  (emit "   movl 4(%esp), %ecx") ; ctx ptr
  (let loop ([regs registers]
             [i 0])
    (cond [(null? regs)
           'ok]
          [(scratch? (car regs))
           (loop (cdr regs)
                 (add1 i))]
          [else
            (match (car regs)
              [(cons n _)
               (emit "   movl %~a, ~a(%ecx)" n (si-of-i i))
               (loop (cdr regs)
                     (add1 i))])])))


(define (emit-restore-regs)
  (define (si-of-i i)
    (* wordsize i))
  (emit "   popl %ecx") ; get ctx ptr
  (let loop ([regs registers]
             [i 0])
    (cond [(null? regs)
           'ok]
          [(scratch? (car regs))
           (loop (cdr regs)
                 (add1 i))]
          [else
            (match (car regs)
              [(cons n _)
               (emit "   movl ~a(%ecx), %~a" (si-of-i i) n)
               (loop (cdr regs)
                     (add1 i))])])))

(define (emit-fn-header lbl)
  (emit "   .globl ~a" lbl)
  (emit "   .type ~a, @function" lbl)
  (emit "~a:" lbl))
(define-syntax gen-pairs
  (syntax-rules ()
    [(_)
     (list)]
    [(_ (op0 p* ...) pair* ...)
     (cons
       (cons `op0
             (lambda ()
               p* ...))
       (gen-pairs pair* ...))]))
(define-syntax biop-emit-pairs
  (syntax-rules ()
    [(_ p0 p* ...)
     (make-hasheq
       (gen-pairs p0 p* ...))]))

(define emit-exp
  (lambda (si env tail?)
    (define (emit-unop op v)
      ((emit-exp si env #f) v)
      (match op
        ['add1 (emit "   addl $~s, %eax" (immediate-rep 1))]
        ['$fxadd1 (emit-exp1 `(add1 ,v))]
        ['sub1 (emit "   subl $~s, %eax" (immediate-rep 1))]
        ['$fxsub1 (emit-exp1 `(sub1 ,v))]
        ['number->char
         ; shift left
         (emit "   shll $~s, %eax" (- charshift fxshift))
         ; change the shifted to char tag
         (emit "   orl $~s, %eax" chartag)]
        ['char->number
         (emit "   sarl $~s, %eax" (- charshift fxshift))]
        ['fixnum?
         (emit-exp1 `(number? ,v))]
        ['number?
         (emit "   andb $~s, %al" fxmask)
         (emit "   cmpb $~s, %al" fxtag)
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['null?
         (emit "   cmpw $~s, %ax" null_v)
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['boolean?
         (emit "   andb $~s, %al" boolmask)
         (emit "   cmpb $~s, %al" booltag)
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['char?
         (emit "   andb $~s, %al" charmask)
         (emit "   cmpb $~s, %al" chartag)
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['not
         (emit "   cmpw $~s, %ax" bool-f)
         ; if equal=#f, we set al to 1, then we transform it to #t.
         ; if equal to other value, we set al to 0, then transformed to #f.
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['zero?
         (emit "   cmpl $~s, %eax" (immediate-rep 0))
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['car
         (emit "   movl -1(%eax), %eax")]
        ['cdr
         (emit "   movl ~s(%eax), %eax" (sub1 wordsize))]
        ['pair?
         (emit "   andb $~s, %al" pairmask)
         (emit "   cmpb $~s, %al" pairtag)
         (emit "   sete %al")
         (emit-eax-0/1->bool)]
        ['print
         (emit "   addl $~s, %esp" (- si wordsize))
         (emit "   movl %ebp, ~s(%esp)" wordsize) ; preserve %ebp on stack
         (emit "   movl %eax, (%esp)") ; move eax value to stack
         (emit "   call print_ptr")
         (emit "   movl ~s(%esp), %ebp" wordsize) ; restore %ebp
         (emit "   subl $~s, %esp" (- si wordsize))
         (emit "   movl $~s, %eax" (immediate-rep 0))]
        [_ (error 'emit-unop "~a is not an unary operator" op)]))
    (define (emit-biop op a b)
      (define (emit-*)
        (emit-exp1 a)
        (emit-remove-fxtag 'eax) ; remove fxtag so that it can be used in imull
        (emit "  movl %eax, ~s(%esp)" si)
        ((emit-exp (- si wordsize) env #f) b)
        (emit-remove-fxtag 'eax)
        (emit "  imull ~s(%esp), %eax" si) ; multiply two number
        (emit-add-fxtag 'eax))
      (define (emit-biv)
        (emit-exps-leave-last si env (list a b)))
      #|((emit-exp si env #f) a)
      (emit "   movl %eax, ~s(%esp)" si); store a to stack
      ((emit-exp (- si wordsize) env #f) b))|#
    (define (emit-cmp op)
      (emit-biv)
      ;(printf "emit-cmp:~s\n" op)
      (emit "  cmpl ~s(%esp), %eax" si)
      (case op
        ['= (emit "   sete %al")]
        ['< (emit "   setg %al")]
        ['<= (emit "  setge %al")]
        ['> (emit "   setl %al")]
        ['>= (emit "  setle %al")]
        [else (report-not-found)])
      ;(printf "end of emit-cmp\n")
      (emit-eax-0/1->bool))
    (define op->emitter
      (biop-emit-pairs
        [* (emit-*)]
        [fx* (emit-exp1 `(* ,a ,b))]
        [+ (emit-biv)
           ; b = a + b
           (emit "   addl ~s(%esp), %eax" si)]
        [fx+ (emit-exp1 `(+ ,a ,b))]
        [- (emit-biv) 
           ; b = a - b
           (emit "   movl %eax, %ecx")
           (emit "   movl ~s(%esp), %eax" si)
           (emit "   subl %ecx, %eax")]
        [fx- (emit-exp1 `(- ,a ,b))]
        [= (emit-cmp '=)]
        [fx= (emit-exp1 `(= ,a ,b))]
        [< (emit-cmp '<)]
        [fx< (emit-exp1 `(< ,a ,b))]
        [<= (emit-cmp '<=)]
        [fx<= (emit-exp1 `(<= ,a ,b))]
        [> (emit-cmp '>)]
        [fx> (emit-exp1 `(> ,a ,b))]
        [>= (emit-cmp '>=)]
        [fx>= (emit-exp1 `(>= ,a ,b))]
        [cons (emit-biv)
              (emit "   movl %eax, ~s(%ebp)" wordsize) ; copy b to heap
              (emit "   movl ~s(%esp), %ecx" si) ; copy a to temporary
              (emit "   movl %ecx, (%ebp)") ; copy a to heap
              (emit "   movl %ebp, %eax")
              (emit "   orl $~s, %eax" pairtag)
              ; assert 2*wordsize = n * heap-align
              (emit "   addl $~s, %ebp" (* 2 wordsize))
              ; baseptr + 2 * wordsize is 8-byte aligned.
              ]
        ))
    (define (report-not-found)
      (error 'emit-biop "~a is not a binary operator" op))
    ;(printf "emit-op ~a ~a\n" op (hash-ref op->emitter op))
    ((hash-ref op->emitter op report-not-found)))
  (define (emit-rands rands)
    (let ([new-si (emit-exps-push-all si env rands)])
      ; swap the rands order
      ; rand-0 : si(%esp)
      ; rand-n : (+ new-si wordsize)(%esp)
      ;(printf "~a ~a ~a\n" si new-si (length rands))
      (unless (= (+ new-si
                    (* (length rands) wordsize))
                 si)
        ; for debuggin purpose
        (error 'emit-rands "some error happened"))
      (emit-swap-rands-range si new-si)
      new-si))
  (define (emit-call-proc rator rands)
    (let ([new-si (emit-rands rands)])
      ;(printf "~a ~a\n" si new-si)
      ; it's (+ wordsize new-si)!!!
      (emit "   addl $~s, %esp" (+ wordsize new-si))
      (emit "   call ~a" rator)
      (emit "   subl $~s, %esp" (+ wordsize new-si))))
  (define (emit-make-vec n)
    ((emit-exp si env #f) n)
    (emit-remove-fxtag 'eax) ; %eax store length
    (emit "   movl %eax, ~s(%esp) #store length to stack" si)
    (emit-calc-vector-size)
    (emit-alloc-heap (- si wordsize) #f) ; vec length on stack
    (emit-stack->heap si 0)
    (emit-add-vectag 'eax))
  (define (emit-vec-ref v i)
    ; TODO: out-of-bounds check?
    (emit-exps-leave-last si env (list v i))
    ; remove i's flag
    (emit-remove-fxtag 'eax)
    (emit "   movl ~s(%esp), %ecx # transfer vec to ecx" si) ; v
    (emit-remove-vectag 'ecx)
    ; %ecx + %eax*wordsize + wordsize. extra wordsize is for vector length
    (emit "   movl ~s(%ecx, %eax, ~s), %eax # get ith(in eax) value of vec" wordsize wordsize)
    )
  (define (emit-vec-set! v i val)
    (emit-exps-leave-last si env (list v i val))
    (emit "   movl ~s(%esp), %ecx" si) ; v
    (emit-remove-vectag 'ecx)
    (emit "   movl ~s(%esp), %edx" (- si wordsize)) ; i
    ; i should be fxnum, we should remove its flags
    (emit-remove-fxtag 'edx)
    ; %ecx + %edx*wordsize + wordsize. extra wordsize is for vector length
    (emit "   movl %eax, ~s(%ecx, %edx, ~s)" wordsize wordsize)
    )
  (define (emit-vec-from-values vs)
    ; why we shouldn't evaluate one value and move it to heap?
    ; Since when evaluate the value, it may change ebp
    (emit "# emit-vec-from-values")
    (let* ([len (length vs)]
           [new-si (emit-exps-push-all si env vs)])
      (emit-alloc-heap new-si
                       (* (add1 len) wordsize))
      (emit "   movl $~s, (%eax) # move length to vector" len)
      (let loop ([i 0])
        (unless (>= i len)
          ; move the ith item from stack to heap
          (emit-stack->heap (- si (* i wordsize))
                            (* (add1 i) wordsize))
          (loop (add1 i))))
      (emit-add-vectag 'eax)
      )
    (emit "# emit-vec-from-values end")
    )
  (define (emit-closure f rv)
    (emit "# emit-closure")
    ;(print rv)(newline)
    ;(emit-exp1 rv)
    ((emit-exp si env #f) rv)
    (emit "   leal ~s, %ecx" f)
    (emit "   movl %ecx, (%ebp)") ; move label address to heap
    (emit "   movl %eax, ~s(%ebp)" wordsize) ; move vector env to heap
    (emit "   movl %ebp, %eax") ; return as pointer
    (emit-add-cljtag 'eax)
    (emit "   addl $~s, %ebp" (* 2 wordsize))
    (emit "# emit-closure end")
    ) ; step ebp
  (define (emit-app rator rands)
    (emit "# emit-app")
    ;(printf "emit-app: is tail call? ~a" tail?)
    (if tail?
      (begin
      (emit "# tail-optimization")
      (let ([new-si
              (emit-exps-push-all (- si wordsize)
                                  env
                                  rands)])
        ((emit-exp new-si env #f) rator)
        (emit-remove-cljtag 'eax)
        (emit "   movl (%eax), %ecx # move label to stack")
        (emit "   movl %ecx, ~s(%esp)" new-si)
        (emit "   movl ~s(%eax), %edx # move clojure env to stack" wordsize)
        (emit "   movl %edx, ~s(%esp)" si)
        (emit-stack-move-range
          si
          (- wordsize)
          (+ new-si wordsize)
          (- wordsize))
        (emit "   jmp *~s(%esp) # tail jump" new-si)
        ))
      (begin
        (emit "#no tail optmization")
        (let ([new-si
                (emit-exps-push-all (- si (* 2 wordsize))
                                    env
                                    rands)])
          ((emit-exp new-si env #f) rator) ; get closure
          (emit-remove-cljtag 'eax)
          (emit "   movl (%eax), %ecx") ; move label to ecx
          (emit "   movl ~s(%eax), %edx" wordsize) ; movel clojure env to stack
          (emit "   movl %edx, ~s(%esp)" (- si wordsize))
          (emit "   addl $~s, %esp" (+ si wordsize))
          (emit "   call *%ecx")
          (emit "   subl $~s, %esp" (+ si wordsize)))))
    (emit "# emit-app end"))
  (define (emit-let vs es body)
    (match (emit-decls si env #f vs es)
      [(list si env)
       ((emit-exp si env tail?)
        body)]))
  ; ## emit-exp1 ##
  (define emit-exp1
    (lambda (exp)
      (match exp
        [(? immediate? x)
         (emit "   movl $~s, %eax # emit immediate:~a" (immediate-rep x) x)]
        [(? symbol? v)
         ; variable
         (let ([pos (env:app env v)])
           ;(printf "emit-var: ~a  ~a\n" v pos)
           (emit "   movl ~s(%esp), %eax # var:~s" pos v))]
        [`(make-vec ,n)
          (emit-make-vec n)]
        [`(vec-ref ,v ,i)
          (emit-vec-ref v i)]
        [`(vec-set! ,v ,i ,val)
          (emit-vec-set! v i val)]
        [`(vec ,v* ...)
          (emit-vec-from-values v*)]
        [(list (? unop? op) v)
         (emit-unop op v)]
        [(list (? biop? op) a b)
         (emit-biop op a b)]
        [`(if ,test ,then ,else)
          ((emit-exp si env #f) test)
          (let ((else-lbl (gen-label))
                (endif-lbl (gen-label)))
            ; jump to else if equal to false
            ; Que: how to optimize this?
            (emit "   cmpl $~s, %eax" bool-f)
            (emit "   je ~a" else-lbl)
            (emit-exp1 then)
            (emit "   jmp ~s" endif-lbl)
            (emit "~s:" else-lbl)
            (emit-exp1 else)
            (emit "~s:" endif-lbl))]
        [`(let ((,v* ,e*) ...) ,body)
          (emit-let v* e* body)]
        [`(lambda (,v* ...) ,body)
          (error 'emit-exp "lambda should be converted to procedure")]
        [`(begin ,exp* ...)
          (let loop ([exps exp*])
            (cond
              [(null? exps)
               (error 'begin "empty body")]
              [(null? (cdr exps))
               (emit-exp1 (car exps))]
              [else
                ((emit-exp si env #f) (car exps))
                (loop (cdr exps))]))]
        [`(labels ([,f* ,proc*] ...) ,exp)
          (for-each
            (lambda (f proc)
              (add-global! f proc))
            f*
            proc*)
          (emit-exp1 exp)]
        [`(app-proc ,rator ,rand* ...)
          (emit-call-proc rator rand*)]
        [`(closure ,f ,rv)
          (emit-closure f rv)]
        [`(app ,rator ,rand* ...)
          (emit-app rator rand*)]
        [_ (error 'emit-exp "~a not matched" exp)]
        )))
  emit-exp1))

; eval(e) could be an address or label
(define (emit-decl si env v e)
  ((emit-exp si env) e)
  (emit "   movl %eax, ~s(%esp)" si)
  (list (- si wordsize)
        (env:ext env v si)))

; for let
(define (emit-decls si env tail? vs es)
  (let loop [(si si)
             (cur-vs vs)
             (cur-es es)
             (si-acc '())]
    (cond
      [(and (null? cur-vs)
            (null? cur-es))
       (list si
             (env:exts env vs (reverse si-acc)))]
      [(or (null? cur-vs)
           (null? cur-es))
       (error 'emit-decls "vs and es have different length")]
      [else
        ((emit-exp si env tail?) (car cur-es))
        (emit "   movl %eax, ~s(%esp) # move declared value to stack" si)
        (loop (- si wordsize)
              (cdr cur-vs)
              (cdr cur-es)
              (cons si si-acc))])))

; for let*
(define (emit-decl* si env vs es)
  (foldl
    (match-lambda*
      [(list v e (list si env))
       (emit-decl si env v e)])
    (list si env)
    vs
    es))

(define (emit-remove-fxtag reg)
  (emit "   sar $~s, %~a # remove fx tag" fxshift reg))
(define (emit-add-fxtag reg)
  ; sign extension
  (emit "   sal $~s, %~a # add fxtag" fxshift reg))
(define (emit-remove-cljtag reg)
  (emit "   subl $~s, %~a # remove cljtag" cljtag reg))
(define (emit-add-cljtag reg)
  ; we don't need to shift
  (emit "   orl $~s, %~a # add cljtag" cljtag reg))
(define (emit-remove-vectag reg)
  (emit "   subl $~s, %~a" vectag reg))
(define (emit-add-vectag reg)
  (emit "   orl $~s, %~a # add vectag" vectag reg))

(define gen-label
  (let ([count 0])
    (lambda ()
      (let ([L (format "L_~s" count)])
        (set! count (add1 count))
        (string->symbol L)))))

; after cmp operation, we can set eax to bool value according
; to the flags
(define (emit-eax-0/1->bool)
  (emit "   movzbl %al, %eax") ; movzbl set eax high-order 24bits to zero
  (emit "   sal $~s, %al" boolshift)  ; transform the result to bool
  (emit "   or $~s, %al" bool-f))

; move eax to stack
(define (emit-eax->stack si)
  (emit "   movl %eax, ~s(%esp) # save eax value to stack" si))

; return si
(define (emit-exps-leave-last si env exps)
  ; emit multi exps, leave the last in %eax)
  (cond
    [(null? exps)
     (error 'emit-exps-leav-last "need at least one exp")]
    [(null? (cdr exps))
     ((emit-exp si env #f) (car exps))
     si]
    [else
      (let-values ([(first rest)
                    (split-at-right exps 1)])
        (let ([si (emit-exps-push-all si env first)])
          (emit-exps-leave-last si env rest)
          si))]))

(define (emit-exps-push-all si env exps)
  ; emit mutli exps, all pushed to stack
  (emit "# emit-exps-push-all")
  (let ([new-si
          (foldl
            (match-lambda*
              [(list exp si)
               ((emit-exp si env #f) exp)
               (emit-eax->stack si)
               (- si wordsize)])
            si
            exps)])
    (emit "# emit-exps-push-all end")
    new-si)
  )

; swap i(%base) j(%base)
(define (emit-swap i j [base 'esp])
  (emit "   movl ~s(%~s), %ecx" i base)
  (emit "   movl ~s(%~s), %edx" j base)
  (emit "   movl %ecx, ~s(%~s)" j base)
  (emit "   movl %edx, ~s(%~s)" i base))

; swap pos in [start end]
(define (emit-swap-range start end [step wordsize] [base 'esp])
  (emit "# emit-swap-range")
  (let* ([l (range-len start step end)]
         [l-1 (sub1 l)]
         [range-get (range-of-i start step end)]
         [n (quotient l 2)])
    (let loop ([i 0]
               )
      (cond
        [(>= i n) (void)]
        [else
          #|(printf "~a ~a\n"
                  (range-get i)
                  (range-get (- l-1 i)))|#
          (emit-swap (range-get i)
                     (range-get (- l-1 i))
                     base)
          (loop (add1 i))])))
  (emit "# emit-swap-range end")
  )

(define (emit-swap-rands-range si new-si)
  ; swap (+ new-si wordsize)(%esp) -> si(%esp)
  ;(printf "emit-swap-rands-range ~a ~a\n" si new-si)
  (emit-swap-range (+ new-si wordsize)
                   si))

(define (emit-stack->heap stack-pos heap-pos)
  ; heap ptr stored in eax
  (emit "   movl ~s(%esp), %ecx # move stack value to heap" stack-pos)
  (emit "   movl %ecx, ~s(%eax)" heap-pos))

; move [start ... end] to [to_start ...]
(define (emit-stack-move-range start step end to_start)
  (emit "# emit-stack-move-range ~a ~a ~a ~a"
        start step end to_start)
  (let ([cmp (range-param-check start step end)])
    (let loop ([from start]
               [to to_start])
      (cond
        [(cmp from end) (void)]
        [else
          ;(printf "move ~a to ~a\n" from end)
          (emit-swap from to)
          (loop (+ from step)
                (+ to step))])))
  (emit "# emit-stack-move-range end")
  )

(define (emit-calc-vector-size)
  ; length in %eax
  (emit "   addl $1, %eax # calculate vector size with %eax store length")
  (emit "   imull $~s, %eax" wordsize))

(define (emit-foreign-call si funcname)
  (emit "   addl $~s, %esp" (+ si wordsize))
  (emit "   call ~a" funcname)
  (emit "   subl $~s, %esp" (+ si wordsize)))

(define (emit-alloc-heap1 si)
  ; heap-aligned size in eax
  ; heap_alloc(mem, stack, size)
  (emit "   movl %eax, ~s(%esp) # size to stack" si)
  (emit "   leal ~s(%esp), %ecx # stack base pointer" (+ si wordsize))
  (emit "   movl %ecx, ~s(%esp)" (- si wordsize))
  (emit "   movl %ebp, ~s(%esp) # mem to stack" (- si (* 2 wordsize)))
  (emit-foreign-call (- si (* 3 wordsize))
                     'heap_alloc))

(define emit-alloc-heap
  (match-lambda*
    [(list si (? boolean? align?))
     ; %eax store the size,
     (when align?
       (emit "   addl $~s, %eax # heap align calculation" (sub1 heap-align))
       (emit "   andl $~s, %eax" (- heap-align)))
     (emit-alloc-heap1 si)]
    [(list si (? number? size))
     ; size is the bytes to allocate
     (let ([aligned-size (bitwise-and (+ size (sub1 heap-align))
                                      (- heap-align))])
       (emit "   movl $~s, %eax" aligned-size)
       (emit-alloc-heap1 si))]))

#|(load "tests-1.3-req1.scm")
(load "tests-1.4-req.scm")
(load "tests-1.5-req1.scm")
(load "tests-1.6-req.scm")|#
#|(load "tests-1.6-opt.scm")
(load "tests-1.5-req.scm")|#

; (load "tests-1.8-opt.scm")
; (load "tests-sexp.scm")
;(load "tests-print.scm")
;(load "tests-proc.scm")
(load "tests-vector.scm")
