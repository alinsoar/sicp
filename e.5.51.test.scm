
(define fib
  (named-lambda (fib x y)
    (if (lessp x 2)
        x
        (add (fib (sub x 1) y)
             (fib (sub x 2) y)))))

(fib 10 ())

(define last-digit
  (named-lambda (last-digit n)
    (return (mod n radix))))

radix

(with-dynamic-binding
 (return (named-lambda (ret x) x))
 (with-dynamic-binding
  (radix 10)
  (last-digit 447)))

radix

10

(div 7 2)

(div 7 0)




