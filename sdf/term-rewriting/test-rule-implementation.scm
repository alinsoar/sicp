
(define this-env (the-environment))

(define-test 'rule-syntax
  (lambda ()
    (assert-equal (unsyntax
                   (syntax '(rule '(* (? b) (? a))
                                  (and (expr<? a b)
                                       `(* ,a ,b)))
                           this-env))
                  '(make-rule '(* (? b) (? a))
                              (lambda (b a)
                                (and (expr<? a b)
                                     (list '* a b)))))))