#lang racket

(require "sicp.rkt")
(require (submod "e.2.76.rkt" export))
(require (submod "e.2.81.rkt" export))
(require (submod "e.2.83.rkt" export))
(require (except-in (submod "e.2.77.rkt" export)
                    apply-generic@))
(require (only-in (submod "e.2.73.rkt" export)
                  table-operations^
                  table-operations@))

;;; rational -> scheme-number -> complex

(define-signature apply-generic/raise^ extends apply-generic^
  (type>=?
   compare-argument-types
   count-booleans
   zip
   max-count
   get-hightest-type
   coerce-to-type
   coerce-argument-to-type
   coerce-arguments-to-max-type))

(define-unit apply-generic@
  (import table-operations^ tags^ GENERIC/raise^)
  (export apply-generic/raise^)
  (define (type>=? e1 t2)
    (define (iter e co)
      (cond ((false? e) (co '>))
            ((eq? (type-tag e) t2) (co '=))
            (else (iter ((get 'raise (list (type-tag e)))
                         (contents e))
                        (lambda (x)
                          (co (if (eq? x '>) '> false)))))))
    (iter e1 (lambda (x) (not (not x)))))
  (define (compare-argument-types args)
    "this will return a list of lists containing boolean values of
this form:
  
        ( (E1 (T1>T1 T1>T2 ... T1>Tn))
          (E2 (T2>T1 T2>T2 ... T2>Tn))
          ...
          (En (Tn>T1 Tn>T2 ... Tn>Tn)) )
  
    The predicate Ti>Tj is true when the type of the ith argument Ei is
highter than the type of the jth argument Ej."
    (map (lambda (e0)
           (list e0
                 (map (lambda (t) (type>=? e0 t))
                      (map type-tag args))))
         args))
  (define (count-booleans l)
    "given a list having only `true` and `false` values, it will
return the count of `true` value."
    (cond ((null? l) 0)
          ((false? (car l)) (count-booleans (cdr l)))
          (else (add1 (count-booleans (cdr l))))))
  (define (zip l1 l2)
    "zip the lists"
    (if (null? l1)
        '()
        (cons (cons (car l1) (car l2))
              (zip (cdr l1) (cdr l2)))))
  (define (max-count l)
    "given a list `l` containing entries of the type ( COUNT . TYPE ),
will return the TYPE that has maximum count"
    (define (iter l co)
      (cond ((null? l) (co '(-1 . no-type)))
            (else (iter (cdr l)
                        (lambda (x)
                          (co (if (> (car x) (caar l))
                                  x
                                  (car l))))))))
    (iter l (lambda (x) x)))
  (define (get-hightest-type args)
    "will return a symbol of the higtest type of all arguments."
    (let ((k (compare-argument-types args)))
      (cdr
       (max-count
        (zip
         (map (lambda (x) (count-booleans (cadr x)))
              k)
         (map type-tag (map car k)))))))
  (define (coerce-to-type n t)
    "coerce the number `n` to type `t`. In this function, it is sure
that the coercion is possible."
    (define (iter n co)
      (if (eq? (type-tag n) t)
          (co n)
          (co (iter ((get 'raise (list (type-tag n)))
                     (contents n))
                    (lambda (y)
                      (co y))))))
    (iter n (lambda (x) x)))
  (define (coerce-argument-to-type t args)
    "this will coerce all the arguments of `args` to type and return
a list of coerced arguments."
    (map (lambda (x) (coerce-to-type x t))
         args))
  (define (coerce-arguments-to-max-type args)
    "return a list with all arguments coerced to the hightest type
of all types of the arguments of the list."
    (coerce-argument-to-type (get-hightest-type args)
                             args))
  (define (apply-generic op . args)
    ;; the only change is to re-define args, coercing all arguments to
    ;; the hightest argument type, after that we do what we did before.
    (let ((args (coerce-arguments-to-max-type args)))
      (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
          (if proc
              (apply proc (map contents args))
              (error
               "No method for these types -- APPLY-GENERIC"
               (list op type-tags))))))))

;;; The current apply-generic is related now to raise, and raise to
;;; apply-generic, so we can combine them to call mutual recursivelly
;;; each other.
(define-compound-unit/infer apply-generic/raise@
  (import table-operations^ tags^ )
  (export GENERIC/raise^ apply-generic^)
  (link GENERIC/raise@ apply-generic@))

(module+ test
  (INVOKE table-operations)
  (INVOKE install-tags-package)
  
  (INVOKE apply-generic/raise)
  
  (INVOKE install-basic-packages)
  (INVOKE-GENERIC exp)
  (INVOKE extend-integer-package/raise)
  (INVOKE extend-rational-package/raise)
  (INVOKE extend-scheme-number-package/raise)
  (INVOKE extend-complex-package/raise)
  
  (get-tables table-show (lambda (-) nil))
  
  (with-handlers
      ([exn:fail? (lambda (v) (exn-message v))])
    (apply-generic
     'test
     (make-rational 10 2)
     (make-rational 10 2)
     (make-scheme-number 10)
     (make-complex-from-real-imag 10 10))
    (car 17))

  (make-rational 10 2)
  (raise (make-rational 10 2))
  (raise (raise (make-rational 10 2)))
  (raise (raise (raise (make-rational 10 2))))

  (make-scheme-number 10)
  (raise (make-scheme-number 10))
  (raise (raise (make-scheme-number 10)))

  (define (t>= x t) (format "~a >=? ~a => ~a"
                            (type-tag x)
                            t
                            (type>=? x t)))
  
  (t>= (make-scheme-number 3) 'complex)
  (t>= (make-scheme-number 3) 'rational)
  (t>= (make-scheme-number 3) 'scheme-number)
  (t>= (make-complex-from-mag-ang 3 2) 'complex)
  (t>= (make-complex-from-mag-ang 3 2) 'rational)
  (t>= (make-rational 3 2) 'rational)
  (t>= (make-rational 3 2) 'complex)
  (t>= (make-rational 3 2) 'scheme-number)

  (with-handlers
      ([exn:fail? (lambda (v) (exn-message v))])
    (raise (raise (raise (make-rational 10 2)))))

  (add '(scheme-number . 4.2)
       '(rational 99 . 8))
  (add '(scheme-number . 27)
       '(integer . 23))
  (div '(rational 27 . 3)
       '(integer . 23))
  (div '(complex rectangular 27 . 0)
       '(integer . 23))
  (add (make-rational 10 2)
       (make-scheme-number 20))
  (add (make-complex-from-real-imag 1 0)
       (make-rational 1 2))
  (mul (make-complex-from-mag-ang 2 pi)
       '(scheme-number . 10)))

(module+ export
  (provide apply-generic/raise@
           apply-generic/raise^
           (prefix-out a/g/r: apply-generic@)))
