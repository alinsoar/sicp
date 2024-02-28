#lang racket

;;; The Church Encoding of Natural Numbers

(define zero  (lambda (f) (lambda (x)             x)))
(define one   (lambda (f) (lambda (x)          (f x))))
(define two   (lambda (f) (lambda (x)       (f (f x)))))
(define three (lambda (f) (lambda (x)    (f (f (f x))))))
(define four  (lambda (f) (lambda (x) (f (f (f (f x)))))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (sub-1 n)
  "2*n operations to get the decrement.
   one can do it with pairs (N N+1) as well."
  (define (iter k co)
    (if (= 0 k)
        co
        (iter (- k 1)
              (lambda (f)
                (lambda (x)
                  (f ((co f) x)))))))
  (let ((k ((n 1+) 0)))
    (cond ((< k 2) zero)
          (else (iter (- k 2) (lambda (x) x))))))

(define (add n m)
  (lambda (f)
    (lambda (x)
      ((n f) ((m f) x)))))

(define (exp n m)
  (lambda (f)
    (lambda (x)
      (((m n) f) x))))

(define (mul n m)
  (lambda (f)
    (lambda (x)
      ((n (m f)) x))))

(define (zero? n)
  (= 0 ((n (lambda (x) (+ x 1))) 0)))

(define (sub n m)
  (if (zero? m)
      n
      (sub (sub-1 n) (sub-1 m))))

(define five (add two three))
(define six (mul two three))
(define seven (add-1 six))
(define eight (exp two three))
(define nine (mul three three))
(define ten (add six four))

(define (1+ k) (+ 1 k))

(module+ test

  (zero? zero)

  (zero? one)

  ((zero 1+) 0)

  ((one 1+) 0)

  ((two 1+) 0)

  ((three 1+) 0)

  (((add-1 zero) 1+) 0)

  (((add-1 three) 1+) 0)

  (((add three two) 1+) 0)

  (((exp two three) 1+) 0)

  (((exp four zero) 1+) 0)

  (((exp four three) 1+) 0)

  (((exp two (add two three)) 1+) 0)

  (((mul three two) 1+) 0)

  (((sub-1 zero) 1+) 0)

  (((sub-1 one) 1+) 0)

  (((sub-1 two) 1+) 0)

  (((sub-1 three) 1+) 0)

  (((sub-1 four) 1+) 0)


  ;; 2^8 - 2^4 = 256 - 16 = 240
  (((sub (exp two (exp two three))
         (exp two (add two two))) 1+) 0)

  (((sub four one) 1+) 0)

  ;; (10^3-10^2) - (5^2*10^2)
  (((sub (sub (exp ten three)
              (exp ten two))
         (mul (exp five two)
              (mul ten two))) 1+) 0)

  ;; (2+((4-1)*2))^(3*(1+2)) = 8^9
  (time
   (((exp (add two
               (mul (sub-1 four) two))
          (mul three
               (add one two))) 1+) 0))

  ;; 8^9 -- faster
  (time
   (((exp (mul two four)
          (mul three three)) 1+) 0))

  ;; 8^9 -- even faster
  (time
   (((exp (lambda (f) (lambda (x) (f (f (f (f (f (f (f (f x))))))))))
          (lambda (f) (lambda (x) (f (f (f (f (f (f (f (f (f x)))))))))))) 1+) 0))

  ;; one--1
  (((add-1 zero) 1+) 0)

  ;; one--2
  ((((lambda (n)
       (lambda (f) (lambda (x) (f ((n f) x)))))
     (lambda (f) (lambda (x) x))) 1+) 0)

  ;; one--3
  (((lambda (f)
      (lambda (x)
        (f (((lambda (f) (lambda (x) x))
             f)
            x)))) 1+) 0)

  ;; one--4
  (((lambda (f)
      (lambda (x)
        (f ((lambda (x) x)
            x)))) 1+) 0)

  ;; one--5
  (((lambda (f)
      (lambda (x)
        (f x))) 1+) 0)

  ;; two--1
  (((add-1 one) 1+) 0)

  ;; two--2
  ((((lambda (n)
       (lambda (f) (lambda (x) (f ((n f) x)))))
     (lambda (f) (lambda (x) (f x)))) 1+) 0)

  ;; two--3
  (((lambda (f)
      (lambda (x)
        (f (((lambda (f) (lambda (x) (f x)))
             f)
            x)))) 1+) 0)

  ;; two--4
  (((lambda (f)
      (lambda (x)
        (f ((lambda (x) (f x))
            x)))) 1+) 0)

  ;; two--5
  (((lambda (f)
      (lambda (x)
        (f (f x)))) 1+) 0)

  ;; three--1
  (((add-1 two) 1+) 0)

  ;; three--2
  ((((lambda (n)
       (lambda (f) (lambda (x) (f ((n f) x)))))
     (lambda (f) (lambda (x) (f (f x))))) 1+) 0)

  ;; three--3
  (((lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f (f x))))
                                 f)
                                x)))) 1+) 0)

  ;; three--4
  (((lambda (f) (lambda (x) (f ((lambda (x) (f (f x)))
                                x)))) 1+) 0)

  ;; three--5
  (((lambda (f) (lambda (x) (f (f (f x))))) 1+) 0)

  )
