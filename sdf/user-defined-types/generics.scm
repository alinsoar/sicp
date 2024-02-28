
(define (make-subsetting-dispatch-store-maker choose-handler)
  (lambda ()
    (let ((delegate (make-simple-dispatch-store)))

      (define (get-handler args)
        (let ((matching
               (filter (lambda (rule)
                         (is-generic-handler-applicable?
                          rule args))
                       ((delegate 'get-rules)))))
          (and (n:pair? matching)
               (choose-handler
                (map cdr (sort matching rule<))
                ((delegate 'get-default-handler))))))

      (lambda (message)
        (case message
          ((get-handler) get-handler)
          (else (delegate message)))))))

(define (is-generic-handler-applicable? rule args)
  (if (simple-function? (cdr rule))
      (simple-function-apply-fit (cdr rule) args)
      (predicates-match? (car rule) args)))

(define (rule< r1 r2)
  (let loop ((ps1 (car r1)) (ps2 (car r2)))
    (if (pair? ps1)
        (cond ((eqv? (car ps1) (car ps2))
               (loop (cdr ps1) (cdr ps2)))
              ((predicate<= (car ps1) (car ps2))
               #t)
              ((predicate<= (car ps2) (car ps1))
               #f)
              (else
               (loop (cdr ps1) (cdr ps2))))
        #f)))

(define make-most-specific-dispatch-store
  (make-subsetting-dispatch-store-maker
   (lambda (handlers default-handler)
     (declare (ignore default-handler))
     (car handlers))))

(define make-chaining-dispatch-store
  (make-subsetting-dispatch-store-maker
   (lambda (handlers default-handler)
     (let loop ((handlers handlers))
       (if (pair? handlers)
           (let ((handler (car handlers))
                 (next-handler (loop (cdr handlers))))
             (lambda args
               (apply handler (cons next-handler args))))
           default-handler)))))

(define (make-cached-most-specific-dispatch-store)
  (cache-wrapped-dispatch-store
   (make-most-specific-dispatch-store)
   get-tag))

(define (make-cached-chaining-dispatch-store)
  (cache-wrapped-dispatch-store
   (make-chaining-dispatch-store)
   get-tag))

(define most-specific-generic-procedure
  (generic-procedure-constructor
   make-cached-most-specific-dispatch-store))

(define chaining-generic-procedure
  (generic-procedure-constructor
   make-cached-chaining-dispatch-store))

(set! make-default-dispatch-store
      make-cached-most-specific-dispatch-store)