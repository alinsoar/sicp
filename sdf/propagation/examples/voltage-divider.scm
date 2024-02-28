
(define text-arith
  (extend-arithmetic
   layered-extender
   (extend-arithmetic interval-extender
                      numeric-arithmetic)))

(install-arithmetic! text-arith)
(install-core-propagators!
 merge-value-sets
 text-arith
 layered-propagator-projector)

(initialize-scheduler)

(define-cell vin)
(define-cell vout)

(define-cell r1)
(define-cell r2)

(define-cell rho)                       ;R2/(R1+R2)
(define-cell r1+r2)

(define-cell z)

(c:* rho vin vout)
(c:* r1 rho z)

(c:+ r1 r2 r1+r2)
(c:* rho r1+r2 r2)

#|
(tell! vin
       (make-interval 14.5 15.5)
       'PowerSupplySpec)

(tell! vout
       (make-interval 3.5 4.0)
       'RequiredBias)

(tell! z
       (make-interval 20000 30000)
       'RequiredImpedance)

(inquire r1)
#|
((r1)
 (has-value (interval 7.25e4 1.3286e5))
 (depends-on requiredbias powersupplyspec requiredimpedance)
 (because
  ((p:/ c:*) (z (interval 20000 30000))
             (rho (interval 2.2581e-1 2.7586e-1)))))
|#

(inquire r2)
#| ((r2) (has-value (the-nothing)) (because unknown)) |#

(retract! 'RequiredImpedance)

(tell! r2 47000 'InStock-1)

(inquire r1)
#|
((r1)
 (has-value (interval 1.2338e5 1.6114e5))
 (depends-on requiredbias powersupplyspec instock-1)
 (because
  ((p:- c:+) (r1+r2 (interval 1.7038e5 2.0814e5)) (r2 47000))))
|#

(inquire z)
#|
((z)
 (has-value (interval 2.7859e4 4.4453e4))
 (depends-on requiredbias powersupplyspec instock-1)
 (because
  ((p:* c:*)
   (r1 (interval 1.2338e5 1.6114e5))
   (rho (interval 2.2581e-1 2.7586e-1)))))
|#

(retract! 'Instock-1)

(tell! r2 (+->interval 47000 470) 'InStock-2)

(inquire r1)
#|
((r1)
 (has-value (interval 1.212e5 1.6369e5))
 (depends-on requiredbias powersupplyspec instock-2)
 (because
  ((p:- c:+) (r1+r2 (interval 1.6867e5 2.1022e5))
             (r2 (interval 46530 47470)))))
|#

(inquire z)
#|
((z)
 (has-value (interval 2.7368e4 4.5157e4))
 (depends-on requiredbias powersupplyspec instock-2)
 (because
  ((p:* c:*) (r1 (interval 1.212e5 1.6369e5))
             (rho (interval 2.2581e-1 2.7586e-1)))))
|#

(retract! 'Instock-2)

(assert! 'RequiredImpedance)

(inquire r1)
#|
((r1)
 (has-value (interval 7.25e4 1.3286e5))
 (depends-on requiredbias powersupplyspec requiredimpedance)
 (because
  ((p:/ c:*) (z (interval 20000 30000))
             (rho (interval 2.2581e-1 2.7586e-1)))))
|#

(inquire r2)
#| ((r2) (has-value (the-nothing)) (because unknown)) |#


(tell! r2 (interval 1000 500000) 'trial-1)

(inquire r2)
#|
((r2)
 (has-value (interval 2.1146e4 5.0612e4))
 (depends-on requiredimpedance
             trial-1
             powersupplyspec
             requiredbias)
 (because
  ((p:* c:*) (rho (interval 2.2581e-1 2.7586e-1))
             (r1+r2 (interval 9.3646e4 1.8347e5)))))
|#

(inquire z)
#|
((z) (has-value (interval 20000 30000))
     (depends-on requiredimpedance)
     (because i-told-you-so))
|#
|#
