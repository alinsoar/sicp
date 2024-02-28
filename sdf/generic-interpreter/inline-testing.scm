
(define (inline-testing:eval expr env)
  (check-repl-initialized)
  (g:eval expr the-global-environment))

(initialize-repl!)