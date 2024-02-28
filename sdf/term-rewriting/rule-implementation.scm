
;;; A rule is a procedure constructed from a pattern and a
;;; consequent procedure (the handler).  A rule takes data that
;;; the rule may apply to and a success and failure continuation.
;;; The consequent procedure binds the variables named in the
;;; pattern.  It produces the result of the rule as a function of
;;; the values of those variables that matched the data
;;; presented.

(define (make-rule pattern handler)
  (let ((match-procedure (match:compile-pattern pattern)))
    (define (the-rule data succeed fail)
      (or (run-matcher match-procedure data
            (lambda (dict)
              (let ((result
                     (apply handler
                            (match:all-values dict))))
                (and result
                     (succeed result
                              (lambda () #f))))))
          (fail)))
    the-rule))

(define-syntax rule
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((pattern (cadr form))
           (handler-body (caddr form))
           (r-make-rule (rename 'make-rule))
           (r-lambda (rename 'lambda)))
       `(,r-make-rule ,pattern
                      (,r-lambda ,(match:pattern-names pattern)
                        ,handler-body))))))

#|
;;; Alternate implementation:

(define-syntax rule
  (sc-macro-transformer
   (lambda (form use-env)
     (let ((pattern (cadr form))
           (handler-body (caddr form)))
       `(make-rule
         ,(close-syntax pattern use-env)
         ,(compile-handler handler-body use-env
                           (match:pattern-names pattern)))))))

(define (compile-handler form env names)
  ;; See magic in utils.scm
  (make-lambda names env
    (lambda (env*) (close-syntax form env*))))
|#
