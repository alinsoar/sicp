
(define (o . args) (void (map display args)))

(define (__d . args)
  "generic display"
  (define (iter a co)
    (if (null? a)
        (co (lambda () (newline)))
        (iter (cdr a)
              (lambda (x)
                (co (lambda ()
                      (display " ")
                      (display (car a))
                      (x)))))))
  (iter args (lambda (x) (x))))


(define (tagged-list? tag)
  (lambda (exp)
    (if (pair? exp)
        (eq? (car exp) tag)
        false)))


