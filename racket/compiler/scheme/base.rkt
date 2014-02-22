#lang racket

(provide (all-defined-out))

(define fxshift 2)
(define fxmask #x03)
(define fxtag #x00)
(define bool-f #x2F)
(define bool-t #x6F)
(define null_v #x3F)
(define charmask #xFF)
(define chartag #x0F)
(define charshift 8)
(define boolshift 6)
(define boolmask #x3F)
(define booltag #x2F)
(define wordsize 4) ; 4byte
(define pairmask #x03)
(define pairtag #x01)
(define cljmask #x03)
(define cljtag #x02)
(define cljshift 3)
; symbol is also atom
(define symmask #x03)
(define symtag #x03)
(define symshift 3)
(define vecmask #x03)
(define vectag #x05)
(define vecshift 3)
(define strmask #x03)
(define strtag #x06)
; for other immediate and objects
#|(define objmask #x03)
(define objtag #x07)|#
(define heap-align 8)

; offset of heap, global
(define heap-offset 0)
(define global-offset 4)

(define registers
  '((eax . scratch)
    (ebx . preserve)
    (ecx . scratch)
    (edx . scratch)
    (esi . preserve)
    (edi . preserve)
    (ebp . preserve)
    (esp . preserve)))

(define (scratch? reg)
  (and (pair? reg)
       (eq? (cdr reg) 'scratch)))

; Data representation:
; Integer: ....00
; Bool: bool-f | bool-t
; Char: ....00001111

(define fixnum-bits (- (* wordsize 8) fxshift))
(define fxlower (- (expt 2 (- fixnum-bits 1))))
(define fxupper (sub1 (expt 2 (- fixnum-bits 1))))
(define (fixnum? x)
  (and (number? x)
       (exact? x)
       (<= fxlower x fxupper)))

(define (immediate? x)
  (or (fixnum? x)
      (boolean? x)
      (char? x)
      (null? x)
      (string? x)
      ))

(define (immediate-rep x)
  (match x
    [(? fixnum?)
     (arithmetic-shift x fxshift)]
    [(? boolean?)
     (if (eq? x #t)
       bool-t
       bool-f)]
    [(? char?)
     (+ (arithmetic-shift (char->integer x) charshift)
        chartag)]
    ['() null_v]
    [_ (error 'immediate-rep "~a is not an immediate" x)]
    ))

(define (unop? op) (memq op '(add1 $fxadd1 sub1 $fxsub1
                                   number->char char->number
                                   fixnum?  number? char? null?
                                   boolean? not zero?
                                   car cdr pair?
                                   print
                                   )))

(define (biop? op)
  (memq op '(cons
              + fx+
              - fx-
              * fx*
              = fx=
              < fx<
              <= fx<=
              > fx>
              >= fx>=
              )))

(define (prim-op? op)
  (or (unop? op)
      (biop? op)
      (memq op '(make-vec vec-ref vec-set! vec
                          print))))

