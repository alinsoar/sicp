
;;; Exposing the continuations: call/ccs as a special form

(define (analyze-call/ccs exp)
  (let ((rproc
         (analyze (call/ccs-receiver exp))))
    (lambda (env succeed fail)
      (let ((succeed-proc
             (lambda (env s f) (s succeed f)))
            (fail-proc
             (lambda (env s f) (s fail f))))
        (rproc env
               (lambda (proc fail-1)
                 (a:advance proc
                            (lambda (procedure fail-2)
                              (a:apply procedure
                                       (list succeed-proc fail-proc)
                                       env
                                       succeed
                                       fail-2))
                            fail-1))
               fail)))))

(define-generic-procedure-handler analyze
  (match-args call/ccs?)
  analyze-call/ccs)


(define (call/ccs? exp)
  (and (pair? exp)
       (eq? (car exp) 'call/ccs)))

(register-predicate! call/ccs? 'call/ccs)

(define (call/ccs-receiver exp)
  (cadr exp))
