#lang racket

(require "sicp.rkt")
(require (prefix-in m: racket/main))
(require (submod "e.2.76.rkt" export))
(require (only-in (submod "e.2.73.rkt" export)
                  table-operations^))
(require (rename-in (submod "e.1.06.rkt" export)
                    (square m:square)))

(define-signature GENERIC/add/sub/mul/div^
  (add sub mul div))
(define-signature constructors/int/num/rat/complex-polar/complex-rect^
  (make-integer
   make-scheme-number
   make-rational
   make-complex-from-real-imag
   make-complex-from-mag-ang))
(define-signature tags-definitions^
  (TAG-TYPE TAG-CONTENTS TAG-ATTACH VALID-DATUM))
(define-signature tags^
  (attach-tag type-tag contents))
(define-signature populate-table^
  (populate-table))
(define-signature integer-package-internals^ extends populate-table^
  (tag))
(define-signature scheme-number-package-internals^ extends populate-table^
  (tag))
(define-signature rational-package-internals^ extends populate-table^
  (tag numer denom make-rat add-rat sub-rat mul-rat div-rat))
(define-signature rectangular-package-internals^ extends populate-table^
  (tag
   install-with-ops
   (open GENERIC/mag/ang/real-part/imag-part/conj^)
   (open constructors/complex^)))
(define-signature polar-package-internals^ extends populate-table^
  (tag
   install-with-ops
   (open GENERIC/mag/ang/real-part/imag-part/conj^)
   (open constructors/complex^)))
(define-signature complex-package-internals^ extends populate-table^
  (tag
   install-with-ops
   (open constructors/complex^)))

(define-unit apply-generic@
  (import table-operations^ tags^)
  (export apply-generic^)
  (define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (error
             "No method for these types -- APPLY-GENERIC"
             (list op type-tags)))))))
(define TAG-TYPE (lambda (datum) (car datum)))
(define TAG-CONTENTS (lambda (datum) (cdr datum)))
(define TAG-ATTACH (lambda (type-tag contents) (cons type-tag contents)))
(define VALID-DATUM (lambda (datum) (pair? datum)))
(define-unit install-tags-package@
  (import tags-definitions^)
  (export tags^)
  (define (attach-tag type-tag contents)
    (TAG-ATTACH type-tag contents))
  (define (type-tag datum)
    (if (VALID-DATUM datum)
        (TAG-TYPE datum)
        (error "Bad tagged datum -- TYPE-TAG" datum)))
  (define (contents datum)
    (if (VALID-DATUM datum)
        (TAG-CONTENTS datum)
        (error "Bad tagged datum -- CONTENTS" datum))))

(define-unit install-integer-package@
  (import table-operations^ tags^)
  (export integer-package-internals^)
  (define (tag x)
    (attach-tag 'integer x))
  (define (populate-table)
    (put 'add '(integer integer)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(integer integer)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(integer integer)
         (lambda (x y) (tag (* x y))))
    (put 'div '(integer integer)
         (lambda (x y) ((get 'make 'rational) x y)))
    (put 'make 'integer
         (lambda (x) (tag x))))
  'done)
(define-unit install-scheme-number-package@
  (import table-operations^ tags^)
  (export scheme-number-package-internals^)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (define (populate-table)
    (put 'add '(scheme-number scheme-number)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
         (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
         (lambda (x y) (tag (/ x y))))
    (put 'make 'scheme-number
         (lambda (x) (tag x))))
  'done)
(define-unit install-rational-package@
  (import table-operations^ tags^)
  (export rational-package-internals^)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (define (populate-table)
    (put 'add '(rational rational)
         (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational)
         (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational)
         (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational)
         (lambda (x y) (tag (div-rat x y))))
    (put 'make 'rational
         (lambda (n d) (tag (make-rat n d)))))
  'done)
(define-unit install-rectangular-package@
  (import table-operations^ tags^)
  (export rectangular-package-internals^)
  ;; internal procedures
  (define id (lambda (x) x))
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define magnitude
    (lambda (+ square sqrt)
      (lambda (z)
        (sqrt (+ (square (real-part z))
                 (square (imag-part z)))))))
  (define angle
    (lambda (atan)
      (lambda (z)
        (atan (imag-part z) (real-part z)))))
  (define (conjugate z)
    (cons (real-part z) (- (imag-part z))))
  (define make-from-mag-ang
    (lambda (* cos sin)
      (lambda (r a)
        (cons (* r (cos a)) (* r (sin a))))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (define (install-with-ops * + atan sin cos square sqrt)
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) (magnitude + square sqrt))
    (put 'angle '(rectangular) (angle atan))
    (put 'make-from-real-imag 'rectangular
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
         (lambda (r a) (tag ((make-from-mag-ang * cos sin) r a)))))
  (define (populate-table)
    (install-with-ops * + m:atan m:sin m:cos m:square m:sqrt))
  'done)
(define-unit install-polar-package@
  (import table-operations^ tags^)
  (export polar-package-internals^)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define real-part
    (lambda (* cos)
      (lambda (z)
        (* (magnitude z) (cos (angle z))))))
  (define imag-part
    (lambda (* sin)
      (lambda (z)
        (* (magnitude z) (sin (angle z))))))
  (define make-from-real-imag
    (lambda (+ sqrt square atan)
      (lambda (x y)
        (cons (sqrt (+ (square x) (square y)))
              (atan y x)))))
  (define (conjugate z)
    (lambda (-)
      (lambda (z)
        (cons (magnitude z) (- (angle z))))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (define (install-with-ops + - * sqrt square atan sin cos)
    (put 'real-part '(polar) (real-part * cos))
    (put 'imag-part '(polar) (imag-part * sin))
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar
         (lambda (x y)
           (tag ((make-from-real-imag + sqrt square atan)
                 x y))))
    (put 'make-from-mag-ang 'polar
         (lambda (r a) (tag (make-from-mag-ang r a)))))
  (define (populate-table)
    (install-with-ops + - * m:sqrt m:square m:atan m:sin m:cos))
  'done)
(define-unit install-complex-package@
  (import apply-generic^
          table-operations^
          tags^
          GENERIC/mag/ang/real-part/imag-part/conj^)
  (export complex-package-internals^)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (INVOKE arith-complex/add/sub/mul/div)
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (define (install-with-ops + - * /)
    (put 'add '(complex complex)
         (lambda (z1 z2) (tag ((add-complex +) z1 z2))))
    (put 'sub '(complex complex)
         (lambda (z1 z2) (tag ((sub-complex -) z1 z2))))
    (put 'mul '(complex complex)
         (lambda (z1 z2) (tag ((mul-complex * +) z1 z2))))
    (put 'div '(complex complex)
         (lambda (z1 z2) (tag ((div-complex / -) z1 z2))))
    (put 'make-from-real-imag 'complex
         (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
         (lambda (r a) (tag (make-from-mag-ang r a))))
    ;; this defined the abstraction barrier between complex number and
    ;; the 2 representations of complex numbers.
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle))
  (define (populate-table)
    (install-with-ops + - * /))
  'done)

;;; arithmetics over numbers
(define-unit GENERIC/add/sub/mul/div@
  (import apply-generic^)
  (export GENERIC/add/sub/mul/div^)
  (define (add x y) (apply-generic 'add x y))
  (define (sub x y) (apply-generic 'sub x y))
  (define (mul x y) (apply-generic 'mul x y))
  (define (div x y) (apply-generic 'div x y)))

;;; global constructors
(define-unit constructors/int/num/rat/complex-polar/complex-rect@
  (import table-operations^)
  (export constructors/int/num/rat/complex-polar/complex-rect^)
  (define (make-integer n)
    ((get 'make 'integer) n))
  (define (make-scheme-number n)
    ((get 'make 'scheme-number) n))
  (define (make-rational n d)
    ((get 'make 'rational) n d))
  (define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y))
  (define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a)))

;;; installation in the table -- export only the constructors of
;;; complex, to keep the same procedures like in SICP.
(define-unit install-table-number-packages@
  (import table-operations^
          apply-generic^
          tags^
          GENERIC/mag/ang/real-part/imag-part/conj^)
  (export constructors/complex^)
  (INVOKE (export (prefix sn: populate-table^))
          install-scheme-number-package)
  (INVOKE (export (prefix rat: populate-table^))
          install-rational-package)
  (INVOKE (export (prefix int: populate-table^))
          install-integer-package)
  (INVOKE (export (prefix pol: populate-table^))
          install-polar-package)
  (INVOKE (export (prefix rect: populate-table^))
          install-rectangular-package)
  (INVOKE (export (rename (prefix c: complex-package-internals^)
                          (make-from-real-imag c:make-from-real-imag)
                          (make-from-mag-ang c:make-from-mag-ang)))
          install-complex-package)
  (sn:populate-table)
  (rat:populate-table)
  (pol:populate-table)
  (rect:populate-table)
  (c:populate-table)
  (int:populate-table)
  'done)

;;; export generic operations, that pass in apply-generic
(define-unit install-GENERIC-packages@
  (import table-operations^ apply-generic^ tags^)
  (export GENERIC/add/sub/mul/div^
          GENERIC/mag/ang/real-part/imag-part/conj^)
  (INVOKE-GENERIC mag/ang/real-part/imag-part/conj)
  (INVOKE-GENERIC add/sub/mul/div))

(define-unit install-basic-packages@
  (import table-operations^ apply-generic^ tags^)
  (export constructors/int/num/rat/complex-polar/complex-rect^
          GENERIC/mag/ang/real-part/imag-part/conj^
          constructors/complex^
          GENERIC/add/sub/mul/div^)
  
  (INVOKE install-GENERIC-packages)
  (INVOKE constructors/int/num/rat/complex-polar/complex-rect)
  (INVOKE install-table-number-packages)
  'done)

(module+ test
  (require (submod "e.2.73.rkt" export))
  (require rackunit)
  (INVOKE install-tags-package)
  (INVOKE table-operations)
  (INVOKE apply-generic)
  (INVOKE install-basic-packages)
  (make-complex-from-real-imag 3 4)
  (magnitude (make-complex-from-real-imag 3 4))
  (add (make-rational 1 2) (make-rational 3 2))
  ;; combinations of different types do not work
  (div (make-integer 2)
       (make-integer 3))
  (add (make-integer 2)
       (make-integer 3))
  (test-case "division with interval containing 0"
             (check-exn exn:fail?
                        (lambda ()
                          (add (make-rational 1 2) (make-complex-from-mag-ang 1 0))))
             'cannot-add-numbers-of-different-types)
  (make-scheme-number 10)
  'ok)

(module+ export
  'ok
  (provide
   ;; install-table-number-packages@
   install-GENERIC-packages@
   constructors/int/num/rat/complex-polar/complex-rect^
   constructors/int/num/rat/complex-polar/complex-rect@
   tags^
   populate-table^

   ;; 
   install-tags-package@

   ;;
   install-integer-package@
   install-rectangular-package@
   install-polar-package@
   install-complex-package@
   install-rational-package@
   install-scheme-number-package@
   
   ;; integer-package-internals^
   rectangular-package-internals^
   polar-package-internals^
   complex-package-internals^
   
   TAG-TYPE TAG-CONTENTS TAG-ATTACH VALID-DATUM
   
   apply-generic@
   GENERIC/add/sub/mul/div^
   GENERIC/add/sub/mul/div@
   install-basic-packages@
   ))
