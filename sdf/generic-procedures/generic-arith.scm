
;;;; Generic arithmetic

(define (make-generic-arithmetic dispatch-store-maker)
  (make-arithmetic 'generic any-object? '()
    constant-union
    (let ((make-generic-procedure
           (generic-procedure-constructor dispatch-store-maker)))
      (lambda (operator)
        (simple-operation operator
                          any-object?
                          (make-generic-procedure
                           operator
                           (operator-arity operator)
                           #f))))))

(define (add-to-generic-arithmetic! generic-arithmetic
                                    arithmetic)
  (add-generic-arith-constants! generic-arithmetic arithmetic)
  (add-generic-arith-operations! generic-arithmetic arithmetic))

(define (add-generic-arith-constants! generic-arithmetic
                                      arithmetic)
  ;; TODO: We have a choice here: do we merge constants with
  ;; non-standard names into the generic arithmetic?  For now, we
  ;; will ignore such constants.
  (for-each
   (lambda (name)
     (let ((binding
            (arithmetic-constant-binding name
                                         generic-arithmetic))
           (element (find-arithmetic-constant name arithmetic)))
       (set-cdr! binding
                 (constant-union name
                                 (cdr binding)
                                 element))))
   (arithmetic-constant-names generic-arithmetic)))

(define (add-generic-arith-operations! generic-arithmetic
                                       arithmetic)
  (for-each
   (lambda (operator)
     (let ((generic-procedure
            (simple-operation-procedure
             (arithmetic-operation operator
                                   generic-arithmetic)))
           (operation
            (arithmetic-operation operator arithmetic)))
       (define-generic-procedure-handler
           generic-procedure
           (operation-applicability operation)
           (operation-procedure operation))))
   (arithmetic-operators arithmetic)))

(define (extend-generic-arithmetic! generic-arithmetic extender)
  (add-to-generic-arithmetic! generic-arithmetic
                              (extender generic-arithmetic)))

(define-generic-procedure-extractor 'arithmetic-overrides
  (lambda (object)
    (and (arithmetic-procedure? object)
         (arithmetic-procedure-metadata object))))

(define-generic-procedure-extractor 'arithmetic-by-name
  (lambda (object)
    (and (symbol? object)
         *current-arithmetic*
         (let ((operation
                (find-arithmetic-operation
                 object
                 *current-arithmetic*)))
           (and operation
                (operation-procedure operation))))))