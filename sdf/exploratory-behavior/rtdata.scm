
(define (success? x) (procedure? x))
(register-predicate! success? 'success)

(define (failure? x) (procedure? x))
(register-predicate! failure? 'failure)

;;; For backtracking
(define (continue-revert! x pexpr penv)
  (vector-set! x 0 'postponed-memo)
  (vector-set! x 1 pexpr)
  (vector-set! x 2 penv))