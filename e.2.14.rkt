#lang racket

(require (submod "e.2.12.rkt" export))
(require (submod "e.2.13.rkt" export))
(require (submod "e.2.08.rkt" export))
(require (submod "e.2.07.rkt" export))

(module+ test

  (define z1 (make-center-percent 100 50))
  (define z2 (make-center-percent 5 10))

  (add-interval z1 z1)

  (mul-interval z1 z1)

  ;; this should be 1
  (div-interval z1 z1)

  ;; this should be z2
  (mul-interval z1 (div-interval z2 z1))

  (define interval-one (make-center-percent 1 0))

  (define interval-one-biased (make-center-percent 1 1))

  (define (test-par c per1 per2 e)
    (if (<= per1 per2)
        (begin
          (let ((z (make-center-percent c per1)))
            (let ((p1 (par1 z z))
                  (p2 (par2 z z))
                  (a (add-interval z z))
                  (m (mul-interval z z))
                  (1/z (div-interval interval-one z)))
              (display
               (format " z=~a  1/z=~a  z+z=~a  z*z=~a  par1=~a  par2=~a\n"
                       (str-interval z 6 2)
                       (str-interval 1/z 6 2)
                       (str-interval a 6 2)
                       (str-interval m 6 2)
                       (str-interval p1 7 2)
                       (str-interval p2 7 2)))))
          (test-par c (+ per1 e) per2 e))
        'ok))

  ;; the correct answer of parallel resistor (z,z) would be z/2, and
  ;; this is computed by par2. The bias in par1 appears due to
  ;; division of an interval with percent>0 by another interval. Here
  ;; I print the limit cases of percent. percent goes in (0 3) and (97
  ;; 100). When we divide an interval with percent 0 with another
  ;; interval, the interval division has bias 0. This is why computing
  ;; par2 is the best one. The bias of division is proved to be so in
  ;; the next experiment, test-div.
  (begin
    (test-par 10 0 3 .1)
    (newline)
    (test-par 10 97 100 .1))

  ;; divide intervals z/z with center `c` and width from per1 to per2
  (define (test-div c per1 per2 e
                    div-function mul-function
                    inv-function)
    (if (< per1 per2)
        (begin
          (let ((z (make-center-percent c per1))
                (z0 (make-interval 100 200)))
            (let ((1/z0 (inv-function z0))
                  (1/z (inv-function z)))
              (let ((d (div-function z z0 1/z 1/z0))
                    (d0 (mul-function z z0 1/z 1/z0)))
                (display
                 (format "~a ~a : ~a ~a   ~a    ~a\n"
                         (= (lower-bound d) (lower-bound d0))
                         (= (upper-bound d) (upper-bound d0))
                         (str-interval z 5 2)
                         (str-interval z0 5 2)
                         (str-center-percent-interval d 5 2)
                         (str-center-percent-interval d0 4 2))))))
          (test-div c (+ per1 e) per2 e
                    div-function mul-function inv-function))
        'ok))


  ;; the equation z/z0 = (z/1)*(1/z0) holds only when 1 is an interval
  ;; non-biased, as it is proven here. We first vary the numerator,
  ;; then we vary the denominator, having the interval 1 non-biased in
  ;; both experiments, then we make the same experiment with a biased
  ;; interval.

  ;; vary numerator, keep the same denominator, 1 non-biased. Equation
  ;; is true in all cases
  (test-div 100
            0 100 1
            (lambda (z z0 1/z 1/z0) (div-interval z z0))
            (lambda (z z0 1/z 1/z0) (mul-interval z 1/z0))
            (lambda (x) (div-interval interval-one x)))

  ;; vary denominator, keep the same numerator, 1 non-biased. Equation
  ;; is true in all cases
  (test-div 100
            0 100 1
            (lambda (z z0 1/z 1/z0) (div-interval z0 z))
            (lambda (z z0 1/z 1/z0) (mul-interval z0 1/z))
            (lambda (x) (div-interval interval-one x)))

  ;; vary numerator, keep the same denominator, 1 BIASED. Equation is
  ;; false in all cases
  (test-div 100
            0 100 1
            (lambda (z z0 1/z 1/z0) (div-interval z z0))
            (lambda (z z0 1/z 1/z0) (mul-interval
                                     (div-interval z interval-one-biased)
                                     1/z0))
            (lambda (x) (div-interval interval-one-biased x)))

  ;; vary denominator, keep the same numerator, 1 BIASED. Equation is
  ;; false in all cases
  (test-div 100
            0 100 1
            (lambda (z z0 1/z 1/z0) (div-interval z0 z))
            (lambda (z z0 1/z 1/z0) (mul-interval
                                     (div-interval z0 interval-one-biased)
                                     1/z))
            (lambda (x) (div-interval interval-one-biased x)))

  ;; this should be 0
  (sub-interval
   interval-one
   (mul-interval z1 (div-interval interval-one z1)))

  ;; distributivity of multiplication over addition does not work
  (define z3 (make-interval 10 20))
  ;; this should be 0
  (sub-interval
   (mul-interval z1 (add-interval z2 z3))
   (add-interval (mul-interval z1 z2)
                 (mul-interval z1 z3)))
  )
