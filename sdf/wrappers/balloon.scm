
;;; clumsy
;(define (psi-to-nsm psi)
;  (/ 1
;     (inch-to-meter (inch-to-meter
;                    (/ 1
;                       (pound-to-newton psi))))))

;;; wrong
;(define (psi-to-nsm psi)
;  ((/ pound-to-newton (* inch-to-meter inch-to-meter))
;   psi))

;;; correct but a little clumsy
(define psi-to-nsm
  (compose pound-to-newton
           (unit:invert inch-to-meter)
           (unit:invert inch-to-meter)))

(define (gas-law-volume pressure temperature amount)
  (/ (* amount gas-constant temperature) pressure))

(define gas-constant 8.3144621)         ;J/(K*mol)

(define (sphere-radius volume)
  (expt (/ volume (* 4/3 pi)) 1/3))

(define pi (* 4 (atan 1 1)))

#|

(gas-law-volume (psi-to-nsm 14.7)
                ((compose celsius-to-kelvin fahrenheit-to-celsius) 68)
                1)
;Value: .02404848592336704

((unit:invert inch-to-meter)
 (sphere-radius
  (gas-law-volume (psi-to-nsm 14.7)
                  ((compose celsius-to-kelvin fahrenheit-to-celsius) 68)
                  1)))
;Value: 7.049624635839811

((unit:expt (unit:invert inch-to-meter) 3)
 (gas-law-volume (psi-to-nsm 14.7)
                 ((compose celsius-to-kelvin fahrenheit-to-celsius) 68)
                 1))
;Value: 1467.5286508533222
|#

(define make-specialized-gas-law-volume
  (unit-specializer gas-law-volume
                    '(expt meter 3)
                    '(/ newton (expt meter 2))
                    'kelvin
                    'mole))

(define conventional-gas-law-volume
  (make-specialized-gas-law-volume '(expt inch 3)
                                   '(/ pound (expt inch 2))
                                   'fahrenheit
                                   'mole))
#|
(conventional-gas-law-volume 14.7 68 1)
;Value: 1467.5286508533222

(sphere-radius (conventional-gas-law-volume 14.7 68 1))
;Value: 7.04962463583981

|#
