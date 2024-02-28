
;;; Override for g:advance on result.
(define (inline-testing:eval expr env)
  (check-repl-initialized)
  (g:advance (g:eval expr the-global-environment)))

(initialize-repl!)