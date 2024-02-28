
;;;; Modular arithmetic

(define get-modulo-predicate
  (memoize-multi-arg-eqv
   (lambda (modulus)
     (guarantee exact-positive-integer? modulus)
     (if (n:< modulus 2)
         (error "Modulus must be 2 or greater:" modulus))
     (make-simple-predicate
      (symbol 'modulo- modulus)
      (lambda (object)
        (and (n:exact-nonnegative-integer? object)
             (n:< object modulus)))
      tagging-strategy:always))))

(define (modulo-arithmetic modulus)
  (let* ((predicate (get-modulo-predicate modulus))
         (constructor (predicate-constructor predicate))
         (get-wrapped (predicate-accessor predicate))
         (convert
          (lambda (value)
            (constructor (modulo value modulus)))))
    (make-arithmetic (symbol 'modulo- modulus)
                     predicate
                     (list numeric-arithmetic)
      (lambda (name base-constant)
        base-constant)
      (lambda (operator numeric-operation)
        (let ((numeric-proc
               (simple-function-procedure numeric-operation))
              (signature
               (call-with-values
                   (lambda ()
                     (operator-signature operator predicate))
                 make-function-predicate)))
          (case operator
            ((negate)
             (make-simple-function operator signature
               (lambda (n)
                 (convert (numeric-proc (get-wrapped n))))))
            ((+ - * max min)
             (make-simple-function operator signature
               (lambda (n1 n2)
                 (convert (numeric-proc (get-wrapped n1)
                                        (get-wrapped n2))))))
            ((< <= = >= >)
             (make-simple-function operator signature
               (lambda (n1 n2)
                 (numeric-proc (get-wrapped n1)
                            (get-wrapped n2)))))
            ((positive? zero? negative?)
             (make-simple-function operator signature
               (lambda (n)
                 (numeric-proc (get-wrapped n)))))
            ((expt)
             (make-simple-function operator signature
               (lambda (n1 n2)
                 (guarantee exact-nonnegative-integer? n2)
                 (convert (numeric-proc (get-wrapped n1) n2)))))
            (else #f)))))))