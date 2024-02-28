
(define (inline-testing:eval expr env)
  (check-repl-initialized)
  (x:advance (x:eval expr the-global-environment)))

(initialize-repl!)