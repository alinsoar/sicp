
(define (inline-testing:eval expr env)
  (check-repl-initialized)
  (a:eval expr
          the-global-environment
          (lambda (value fail1)
            (a:advance value
                       (lambda (value2 fail2)
                         (declare (ignore fail2))
                         value2)
                       fail1))
          (lambda ()
            'no-more-alternatives)))

(initialize-repl!)