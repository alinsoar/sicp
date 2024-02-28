
;;; The contents of this file is repeated in dependencies/examples.
;;; It is here to make the coderefs work for layers/text.tex.

(define (F m1 m2 r)
  (/ (* G m1 m2)
     (square r)))

(define G
  (layered-datum 6.67408e-11
    unit-layer (unit 'meter 3 'kilogram -1 'second -2)
    support-layer (support-set 'CODATA-2018)))

(define M-Earth
  (layered-datum 5.9722e24
                 unit-layer (unit 'kilogram 1)
                 support-layer (support-set 'Astronomical-Almanac-2016)))

(define M-Moon
  (layered-datum 7.342e22
                 unit-layer (unit 'kilogram 1)
                 support-layer (support-set 'NASA-2006)))

(define a-Moon
  (layered-datum 384399e3
                 unit-layer (unit 'meter 1)
                 support-layer (support-set 'Wieczorek-2006)))

;; coderef: F-result
(pp (F M-earth M-Moon a-moon))
'expect-description:
'((base-layer 1.9805035857209e20)
  (unit-layer (unit kilogram 1 meter 1 second -2))
  (support-layer
   (support-set Wieczorek-2006
                NASA-2006
                Astronomical-Almanac-2016
                CODATA-2018)))
