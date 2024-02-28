
(define (make-pattern-operator . rules)
  (let ((rules
         (cons 'rules
               (if (pair? rules)
                   (except-last-pair rules)
                   '())))
        (default-rule
         (and (pair? rules)
              (last rules))))
    (define (the-operator . data)
      (define (succeed value fail) value)
      (define (fail)
        (error "No applicable operations:" data))
      (try-rules data
                 (cdr rules)
                 succeed
                 (if default-rule
                     (lambda ()
                       (try-rule data
                                 default-rule
                                 succeed
                                 fail))
                     fail)))
    (set-pattern-metadata! the-operator rules)
    the-operator))

(define (attach-rule! operator rule)
  (let ((metadata (pattern-metadata operator)))
    (set-cdr! metadata
              (append (cdr metadata)
                      (list rule)))))

(define (override-rule! operator rule)
  (let ((metadata (pattern-metadata operator)))
    (set-cdr! metadata
              (cons rule (cdr metadata)))))

(define pattern-metadata)
(define set-pattern-metadata!)
(let ((store (make-metadata-association)))
  (set! pattern-metadata (store 'get))
  (set! set-pattern-metadata! (store 'put!)))