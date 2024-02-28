
;;; This is the literal function mechanism

(define (literal-function fexp)
  (define (the-function . args)
    (if (any differential? args)
        (let ((n (length args))
              (factor (apply maximal-factor args)))
          (let ((realargs
                 (map (lambda (arg)
                        (finite-part arg factor))
                      args))
                (deltargs
                 (map (lambda (arg)
                        (infinitesimal-part arg factor))
                      args)))
            (let ((fxs (apply the-function realargs))
                  (partials
                   (map (lambda (i)
                          (apply (literal-function
                                  (deriv-expr i n fexp))
                                 realargs))
                        (iota n))))
              (fold d:+ fxs
                (map d:* partials deltargs)))))
        `(,fexp ,@args)))
  the-function)

(define (deriv-expr i n fexp)
  (if (= n 1)
      `(derivative ,fexp)
      `((partial ,i) ,fexp)))
