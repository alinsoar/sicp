
;;;; Read-eval-print loop for extended Scheme interpreter

(define (repl)
  (check-repl-initialized)
  (let ((input (g:read)))
    (write-line (x:eval input the-global-environment))
    (repl)))

(define (load-library filename)
  (check-repl-initialized)
  (call-with-input-file filename
    (lambda (port)
      (let lp ()
        (let ((input (read port)))
          (if (not (eof-object? input))
              (begin
                (write-line
                 (x:eval input the-global-environment))
                (lp))
              'done))))))

;;; Output handlers for special runtime objects

(define-generic-procedure-handler write
  (match-args deferred?)
  (compose write x:advance))

(define-generic-procedure-handler write-line
  (match-args deferred?)
  (compose write-line x:advance))

(define-generic-procedure-handler pp
  (match-args deferred?)
  (compose pp x:advance))

(define-generic-procedure-handler write
  (match-args advanced-memo?)
  (compose write advanced-value))

(define-generic-procedure-handler write-line
  (match-args advanced-memo?)
  (compose write-line advanced-value))

(define-generic-procedure-handler pp
  (match-args advanced-memo?)
  (compose pp advanced-value))