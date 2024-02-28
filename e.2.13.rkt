#lang racket

(require (submod "e.2.07.rkt" export))
(require (submod "e.2.12.rkt" export))

;;; if the tolerance of x and y are small, they are negligible when
;;; divided with 100*100=1e4.
(define (percentage-tolerance-product x y)
  (/ (+ (percent x) (percent y))
     (+ 1 (/ (* (percent x) (percent y))
             1e4))))

;; parallel resistor formula
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

;; same formula, numerator and denominator divided by R1R2
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(module+ test

  (define z1 (make-center-percent 10. 2.))
  (define z2 (make-center-percent 30. 5.))

  (percent (mul-interval z1 z2))
  (percentage-tolerance-product z1 z2)

  (str-center-percent-interval
   (make-center-percent (center (mul-interval z1 z2))
                        (percentage-tolerance-product z1 z2))
   10 10)

  (str-center-percent-interval
   (make-center-percent (center (mul-interval z1 z2))
                        (percent (mul-interval z1 z2)))
   10 10)

  (define (test-percent-interval-mul z1 z2)
    (- (percent (mul-interval z1 z2))
       (percentage-tolerance-product z1 z2)))

  (test-percent-interval-mul z1 z2)

  ;; par2 returns an interval that is included in the interval
  ;; returned by par1.
  (str-interval (par1 z1 z2) 10 5)
  (str-interval (par2 z1 z2) 10 5)

  (str-interval (par1 z1 z1) 10 5)
  (str-interval (par2 z1 z1) 10 5)

  (str-interval (par1 z2 z2) 10 5)
  (str-interval (par2 z2 z2) 10 5))

(module+ export
  (provide par1 par2))

;;; How to deduce the formula

;; We have the first interval X = [c1 - w1; c1 + w1]

;; We have the second interval Y = [c2 - w2; c2 + w2]

;; Because we suppose they are all positives, we have the product so:

;; [ (c1-w1)*(c2-w2) ; (c1+w1)*(c2+w2) ]

;; This has the center:

;; c = (2*c1*c2 + 2*w1*w2)/2 = c1c2 + w1w2

;; and the width is so:

;; w = (2*c1*w2 + 2*c2*w1)/2 = c1w2 + c2w1

;; The percent tolerance is so:

;; p = 100w/c = 100(c1w2 + c2w1) / (c1c2 + w1w2) =

;;        (100 c1w2 + 100 c2w1)            w2     w1
;;        ---------------------           ---- + ----
;;                c1c2                     c2     c1
;;   =  -------------------------- = 100 ----------------
;;                 w1w2                        w1w2
;;           1+ --------                   1+ ------
;;                 c1c2                        c1c2

;;           t1        t2
;;         ------  + ------
;;          100       100              t1+t2
;;  = 100 --------------------- = ----------------
;;                 w1w2                  t1 t2
;;            1+ --------          1 + ---------
;;                 c1c2                 100*100


