
;;;; Boolean arithmetic

(define boolean-arithmetic
  (make-arithmetic 'boolean boolean? '()
    (lambda (name)
      (case name
        ((additive-identity) #f)
        ((multiplicative-identity) #t)
        (else (default-object))))
    (lambda (operator)
      (let ((procedure
             (case operator
               ((+) (lambda (b1 b2) (or b1 b2)))
               ((*) (lambda (b1 b2) (and b1 b2)))
               ((-) (lambda (b1 b2) 
                      (error "Boolean binary - not defined"
                             b1 b2)))
               ((<) (lambda (b1 b2) (and (not b1) b2)))
               ((<=) (lambda (b1 b2) (or (not b1) b2)))
               ((=) (lambda (b1 b2) (eq? b1 b2)))
               ((>=) (lambda (b1 b2) (or b1 (not b2))))
               ((>) (lambda (b1 b2) (and b1 (not b2))))
               ((positive?) (lambda (b) b))
               ((zero?) (lambda (b) (not b)))
               ((max) (lambda (b1 b2) (or b1 b2)))
               ((min) (lambda (b1 b2) (and b1 b2)))
               ((negate) (lambda (b) (not b)))
               (else (lambda args
                       (error "Operator undefined in Boolean"
                              operator args))))))
        (and procedure
             (simple-operation operator boolean? procedure))))))
