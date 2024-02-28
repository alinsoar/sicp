#lang racket

(require "sicp.rkt")
(require (submod "e.2.76.rkt" export))
(require (except-in (submod "e.2.77.rkt" export)
                    apply-generic@))
(require (only-in (submod "e.2.73.rkt" export)
                  table-operations^
                  table-operations@))

(define-signature coercion-table-operations^
  (put-coercion get-coercion show-coercion))
(define-signature GENERIC/exp^
  (exp))

(define-unit extend-scheme-number-package/coercions@
  (import tags^
          coercion-table-operations^
          constructors/int/num/rat/complex-polar/complex-rect^)
  (export)
  (define (scheme-number->complex n) (make-complex-from-real-imag (contents n) 0))
  (put-coercion 'scheme-number 'complex scheme-number->complex)
  ;; it is wrong to insert a coercion to itself, because in case that
  ;; this is used, an infinite loop in apply-generic will rise.
  (define (scheme-number->scheme-number n) n)
  (put-coercion 'scheme-number 'scheme-number
                scheme-number->scheme-number)
  'done)
(define-unit extend-rational-package/coercions@
  (import tags^
          table-operations^
          coercion-table-operations^
          constructors/int/num/rat/complex-polar/complex-rect^)
  (export)
  (INVOKE install-rational-package)
  (define (rational->complex x)
    (let ((v (contents x)))
      (make-complex-from-real-imag (/ (numer v) (denom v)) 0)))
  (define (rational->scheme-number x)
    (let ((v (contents x)))
      (make-scheme-number (/ (numer v) (denom v)))))
  (put-coercion 'rational 'complex rational->complex)
  (put-coercion 'rational 'scheme-number rational->scheme-number)
  'done)
(define-unit extend-complex-package/coercions@
  (import tags^
          coercion-table-operations^
          constructors/int/num/rat/complex-polar/complex-rect^)
  (export)
  ;; an infinite loop in apply-generic will rise if one arrives in the
  ;; situation to coerce any type to itself.
  (define (complex->complex z) z)
  (put-coercion 'complex 'complex complex->complex)
  'done)

(define-unit WRONG-apply-generic@
  (import table-operations^
          tags^
          coercion-table-operations^)
  (export apply-generic^)
  (define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (if (= (length args) 2)
                (let ((type1 (car type-tags))
                      (type2 (cadr type-tags))
                      (a1 (car args))
                      (a2 (cadr args)))
                  ;; (o "::") (d2 type1 type2)
                  (let ((t1->t2 (get-coercion type1 type2))
                        (t2->t1 (get-coercion type2 type1)))
                    (cond (t1->t2
                           (apply-generic op (t1->t2 a1) a2))
                          (t2->t1
                           (apply-generic op a1 (t2->t1 a2)))
                          (else
                           (error "No method for these types"
                                  (list op type-tags))))))
                (error "No method for these types"
                       (list op type-tags))))))))
(define-unit apply-generic@
  (import table-operations^
          tags^
          coercion-table-operations^)
  (export apply-generic^)
  (define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (if (= (length args) 2)
                (let ((type1 (car type-tags))
                      (type2 (cadr type-tags))
                      (a1 (car args))
                      (a2 (cadr args)))
                  (if (eq? type1 type2)
                      (error 'e "! cannot coerce a type to itself; op is not found.")
                      (let ((t1->t2 (get-coercion type1 type2))
                            (t2->t1 (get-coercion type2 type1)))
                        (cond (t1->t2
                               (apply-generic op (t1->t2 a1) a2))
                              (t2->t1
                               (apply-generic op a1 (t2->t1 a2)))
                              (else
                               (error "No method for these types"
                                      (list op type-tags)))))))
                (error "! No method for these types"
                       (list op type-tags))))))))

(define-unit GENERIC/exp@
  (import apply-generic^)
  (export GENERIC/exp^)
  (define (exp x y) (apply-generic 'exp x y)))

(define-unit extend-integer-package/exp@
  (import tags^ table-operations^)
  (export)
  (INVOKE install-integer-package)
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  'done)
(define-unit extend-scheme-number-package/exp@
  (import tags^ table-operations^)
  (export)
  (INVOKE install-scheme-number-package)
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  'done)
(define-unit extend-rational-package/exp@
  (import tags^ table-operations^)
  (export)
  (INVOKE install-rational-package)
  (put 'exp '(rational rational)
       (lambda (x y) (tag (expt (/ (numer x) (denom x))
                                (/ (numer y) (denom y))))))
  'done)
(define-unit extend-complex-package/exp@
  (import tags^
          table-operations^
          apply-generic^
          GENERIC/mag/ang/real-part/imag-part/conj^
          constructors/complex^)
  (export)
  (put 'exp '(complex complex)
       (lambda (z1 z2)
         (make-from-mag-ang
          (* (expt (magnitude z1) (real-part z2))
             (exp (- (* (imag-part z2)
                        (angle z1)))))
          (+ (* (real-part z2)
                (angle z1))
             (/ (* (imag-part z2)
                   (log (expt (magnitude z1) 2)))
                2)))))
  'done)

(define get-tables
  (lambda (p0 p1)
    (list (p0 (lambda (x) (format ":GT: ~a" (car x))))
          (p1 (lambda (x) (format ".CT. ~a" (car x)))))))

(module+ test
  (require rackunit)
  (require racket/sandbox)

  (define-unit make-test-environment@
    (import tags^)
    (export constructors/int/num/rat/complex-polar/complex-rect^
            constructors/complex^
            GENERIC/mag/ang/real-part/imag-part/conj^
            GENERIC/add/sub/mul/div^
            GENERIC/exp^
            table-operations^
            apply-generic^
            coercion-table-operations^)
    ;; reset the coercion-table
    (INVOKE table-operations
      (import)
      (export (rename table-operations^
                      (put-coercion put)
                      (get-coercion get)
                      (show-coercion table-show))))
    ;; reset the global-table
    (INVOKE table-operations
      (import)
      (export table-operations^))
    (INVOKE apply-generic)
    (INVOKE-GENERIC exp)
    
    (INVOKE install-basic-packages)

    (INVOKE extend-scheme-number-package/coercions)
    (INVOKE extend-rational-package/coercions)
    (INVOKE extend-complex-package/coercions))
  (define-unit make-test-environment-WRONG-apply@
    (import tags^)
    (export constructors/int/num/rat/complex-polar/complex-rect^
            constructors/complex^
            GENERIC/mag/ang/real-part/imag-part/conj^
            GENERIC/add/sub/mul/div^
            GENERIC/exp^
            table-operations^
            apply-generic^
            coercion-table-operations^)
    ;; reset the coercion-table
    (INVOKE table-operations
      (import)
      (export (rename table-operations^
                      (put-coercion put)
                      (get-coercion get)
                      (show-coercion table-show))))
    ;; reset the global-table
    (INVOKE table-operations
      (import)
      (export table-operations^))
    (INVOKE WRONG-apply-generic)
    (INVOKE-GENERIC exp)
    
    (INVOKE install-basic-packages)
    (INVOKE extend-scheme-number-package/coercions)
    (INVOKE extend-rational-package/coercions)
    (INVOKE extend-complex-package/coercions))
  
  
  ;; the tags module does not depend on anything, so we can install it
  ;; at this point, before to start installing the environment for the tests
  (INVOKE install-tags-package)

  ;; In the first and second test we use `apply-generic` that does
  ;; check if the types are equal and throws an error if they
  ;; are. Before the tests, we reset the `global-table` and the
  ;; `coercion-table`, after that we make a first test, calling `exp`
  ;; with identical types for arguments. This will result in an error,
  ;; because `apply-generic` forbids calling coercions on the same
  ;; type when `op` is not found. In the 2nd test, we use the same
  ;; `apply-generic`, and the same call of `exp`, but this time we add
  ;; the `exp` operation in the global-table. This time,
  ;; `apply-generic` finds the generic operation, and computes the
  ;; result without trying to coerce.
  (define (test12)
    (INVOKE make-test-environment)

    (define (test1/apply-generic/exp-not-installed)
      (list
       "TEST 1 -- APPLY-CHECK_TYPES -- no exp"
       (get-tables table-show show-coercion)
       (add (make-rational 1 2)
            (make-complex-from-real-imag .5 0))
       (add (make-rational 1 2)
            (make-complex-from-mag-ang .5 0))
       (add (make-rational 1 2)
            (make-scheme-number .5))
       (mul (make-rational 1 2)
            (make-scheme-number .5))
       (test-case "exp"
                  (check-exn exn:fail?
                             (lambda ()
                               (exp (make-complex-from-mag-ang 2 0)
                                    (make-complex-from-mag-ang 3 0))))
                  (format "~a -- ~a"
                          "exp called with identical arg types"
                          "exp not installed -> cannot coerce a type to itself"))
       (test-case "exp"
                  (check-exn exn:fail?
                             (lambda ()
                               (exp (make-scheme-number 1)
                                    (make-scheme-number 3))))
                  (format "~a -- ~a"
                          "exp called with identical arg types"
                          "exp not installed -> cannot coerce a type to itself"))
       'ok))
    (define (test2/apply-generic/exp-installed)
      (INVOKE extend-scheme-number-package/exp)
      (INVOKE extend-rational-package/exp)
      (INVOKE extend-complex-package/exp)
      
      (list
       "TEST 2 -- APPLY-CHECK_TYPES -- exp"
       (get-tables table-show show-coercion)
       (add (make-rational 1 2)
            (make-complex-from-real-imag .5 0))
       (add (make-rational 1 2)
            (make-complex-from-mag-ang .5 0))
       (add (make-rational 1 2)
            (make-scheme-number .5))
       (mul (make-rational 1 2)
            (make-scheme-number .5))
       (exp (make-complex-from-mag-ang 2 0)
            (make-complex-from-mag-ang 3 0))
       (exp (make-scheme-number 1)
            (make-scheme-number 3))
       'ok))

    (list (test1/apply-generic/exp-not-installed)
          (test2/apply-generic/exp-installed)))

  ;; In the third and fourth test we start the same as in the first 2
  ;; tests, by resetting the global-table and the coercion-table, but
  ;; this time we use the variant of `apply-generic` that does not
  ;; check whether the types of arguments are identical, and try to
  ;; coerce. In the 3rd test, we do not install the `exp` operation,
  ;; and try to compute `exp` of 2 numbers that have the same
  ;; type. Because apply-generic cannot find the generic operation in
  ;; the global-table, it will try to coerce. Because the types are
  ;; the same, and the coercion operation returns the same types as
  ;; before, apply-generic will call itself with the same types of
  ;; arguments returned by coercions, entering into an infinite
  ;; loop. In the 4th test, we install the `exp` operation in the
  ;; global-table and do the same test. This time, there will be no
  ;; error, because apply-generic finds the op.
  (define (test34)
    (INVOKE make-test-environment-WRONG-apply)

    (define (test3/WRONG-apply-generic/exp-not-installed)
      
      (list
       "TEST 3 -- APPLY-NO_CHECK_TYPES -- no exp"
       (get-tables table-show show-coercion)
       (add (make-rational 1 2)
            (make-complex-from-real-imag .5 0))
       (add (make-rational 1 2)
            (make-complex-from-mag-ang .5 0))
       (add (make-rational 1 2)
            (make-scheme-number .5))
       (mul (make-rational 1 2)
            (make-scheme-number .5))

       ;; if the `exp` function is not defined for a type like
       ;; `scheme-number`, it is wrong to try to coerce to itself,
       ;; because after the coercion one calls
       ;;
       ;;   (apply-generic op (t1->t2 a1) a2)
       ;;
       ;; and this will recursivelly call apply-generic with both
       ;; parameters having the same type as in the previous call,
       ;; thus an infinite loop appears.


       
       (infinite-loop (lambda () (exp (make-complex-from-mag-ang 2 0)
                                 (make-complex-from-mag-ang 3 0)))
                      "exp called with identical arg types -- exp not installed -> infinite loop"
                      .01
                      "exp")

       (infinite-loop (lambda () (exp (make-scheme-number 1)
                                 (make-scheme-number 3)))
                      "exp called with identical arg types -- exp not installed -> infinite loop"
                      .01
                      "exp")

       
       'ok))
    (define (test4/WRONG-apply-generic/exp-installed)
      (INVOKE extend-scheme-number-package/exp)
      (INVOKE extend-rational-package/exp)
      (INVOKE extend-complex-package/exp)
      
      (list
       "TEST 4 -- APPLY-NO_CHECK_TYPES -- exp"
       (get-tables table-show show-coercion)
       (add (make-rational 1 2)
            (make-complex-from-real-imag .5 0))
       (add (make-rational 1 2)
            (make-complex-from-mag-ang .5 0))
       (add (make-rational 1 2)
            (make-scheme-number .5))
       (mul (make-rational 1 2)
            (make-scheme-number .5))
       ;; it works fine at this point. no need to catch some error.
       (exp (make-complex-from-mag-ang 2 0)
            (make-complex-from-mag-ang 3 0))
       (exp (make-scheme-number 1)
            (make-scheme-number 3))
       'ok))

    (list (test3/WRONG-apply-generic/exp-not-installed)
          (test4/WRONG-apply-generic/exp-installed)))

  (list (test12)
        (test34)))

(module+ export
  (provide coercion-table-operations^
           GENERIC/exp@
           get-tables
           extend-scheme-number-package/coercions@
           extend-rational-package/coercions@
           extend-complex-package/coercions@
           extend-scheme-number-package/exp@
           extend-rational-package/exp@     
           extend-complex-package/exp@))
