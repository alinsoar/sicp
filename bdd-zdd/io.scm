

(define __display_generic
  (lambda (out close)
    (lambda args
      (define (iter a co)
        (if (null? a)
            (co (lambda ()
                  (close)))
            (iter (cdr a)
                  (lambda (x)
                    (co (lambda ()
                          (out (car a))
                          (x)))))))
      (iter args (lambda (x) (x) ) ) ) ) )

(define __d
  (__display_generic (lambda (x) (display " ") (display x))
                     (lambda () (newline))))
(define __p
  (__display_generic (lambda (x) (display " ") (display x))
                     (lambda () 'ok)))
(define __w
  (__display_generic (lambda (x) (display " ") (write x))
                     (lambda () (newline))))


(define read-file-by-sexp
  (lambda (file)
    (let ((in (open-input-file file)))
      ((lambda (s) (s s))
       (lambda (s)
         (let ((s/expr (read in)))
           ;; return this reader
           (lambda (return/next finish)
             (if (eof-object? s/expr)
                 (begin
                   (close-port in)
                   (finish))
                 (return/next
                  s/expr
                  (lambda ()
                    (s s)))))))))))


'io
