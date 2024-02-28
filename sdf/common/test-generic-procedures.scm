
(define-test 'generic-basic
  (lambda ()
    (define foo
      (simple-generic-procedure 'foo 0 #f))

    (define-generic-procedure-handler foo (match-args)
      (lambda ()
        'foobar))

    (assert-equal 'foobar (foo))))

(define-test 'generic-example1
  (lambda ()
    (define foo
      (simple-generic-procedure 'foo 2 #f))

    (define-generic-procedure-handler foo (match-args number? number?)
      (lambda (a b)
        (+ a b)))

    (define-generic-procedure-handler foo (any-arg 2 symbol? number?)
      (lambda (a b)
        (list '+ a b)))

    (assert-equal 3 (foo 1 2))
    (assert-equal '(+ 1 a) (foo 1 'a))
    (assert-equal '(+ a 2) (foo 'a 2))
    (assert-equal '(+ a b) (foo 'a 'b))))