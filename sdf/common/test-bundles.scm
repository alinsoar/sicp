
(define-test 'empty
  (lambda ()
    (define foo? (make-bundle-predicate 'foo))
    (define foo (bundle foo?))

    (assert-true (foo? foo))

    (assert-equal '() (bundle-names foo))

    (assert-false (bundle-ref foo 'operator1 #f))

    (assert-error (lambda () (bundle-ref foo 'operator1)))

    (assert-equal '() (bundle-alist foo))
    ))

(define-test 'simple
  (lambda ()

    (define (operator0) 2)
    (define (operator1 a) (+ a 3))
    (define (operator2 a b) (* a b))

    (define foo? (make-bundle-predicate 'foo))
    (define foo (bundle foo? operator0 operator1 operator2))

    (assert-true (foo? foo))

    (assert-lset= eq?
                  '(operator0 operator1 operator2)
                  (bundle-names foo))

    (assert-eqv operator0 (bundle-ref foo 'operator0))
    (assert-eqv operator1 (bundle-ref foo 'operator1))
    (assert-eqv operator2 (bundle-ref foo 'operator2))
    (assert-error (lambda () (bundle-ref foo 'operator3)))

    (assert-lset= equal?
                  (list (cons 'operator0 operator0)
                        (cons 'operator1 operator1)
                        (cons 'operator2 operator2))
                  (bundle-alist foo))

    (assert-eqv 2 (foo 'operator0))
    (assert-eqv 8 (foo 'operator1 5))
    (assert-eqv 10 (foo 'operator1 7))
    (assert-eqv 6 (foo 'operator2 2 3))
    (assert-eqv 35 (foo 'operator2 5 7))
    (assert-error (lambda () (foo 'operator3)))
    (assert-error (lambda () (foo 'operator3 2)))
    (assert-error (lambda () (foo 'operator3 3 5)))
    ))