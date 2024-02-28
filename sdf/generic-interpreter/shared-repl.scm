
;;;; Shared parts of REPL

;;; Global environment for REPL.
(define the-global-environment
  'not-initialized)

(define (initialize-repl!)
  (set! the-global-environment (make-global-environment))
  'done)

(define (check-repl-initialized)
  (if (eq? the-global-environment 'not-initialized)
      (error "Interpreter not initialized. Run (init) first.")))

(define write
  (simple-generic-procedure 'write 1
    (access write user-initial-environment)))


(define write-line
  (simple-generic-procedure 'write-line 1
    (access write-line user-initial-environment)))

(define pp
  (simple-generic-procedure 'pretty-print 1
    (access pp user-initial-environment)))

(define-generic-procedure-handler write
  (match-args compound-procedure?)
  (compose write procedure-printable-representation))

(define-generic-procedure-handler write-line
  (match-args compound-procedure?)
  (compose write-line procedure-printable-representation))

(define-generic-procedure-handler pp
  (match-args compound-procedure?)
  (compose pp procedure-printable-representation))

(define (g:read)
  (prompt-for-command-expression "eval> "))

(define (init)
  (initialize-repl!)
  (repl))

(define (go)
  (repl))