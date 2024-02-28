
(load "aux/msim")

(set! make-stack make-stack2)

(load "aux/eceval")

(define the-global-environment (setup-environment))

(start eceval)

;;; k = a*n+b
;;; k1 = a*n1+b
;;; k2 = a*n2+b
;;; k2-k1 = a(n2-n1)
;;; a = (k2-k1)/(n2-n1)
;;; b = k1-a*n1 = k2-a*n2
(define compute-linear-coeff
  (lambda (n1 k1 n2 k2)
    ((lambda (a) ((lambda (b)
               (display "\na=") (display a)
               (display "\nb=") (display b)
               (display "\n"))
             (- k1 (* a n1))))
     (/ (- k2 k1) (- n2 n1)))))

;;; because we cannot get in the environment of eceval the statistics
;;; we precompute it here and insert the values in `stats`.
(compute-linear-coeff 0 29 50 1779)

(define stats
  (lambda (n)
    (define a 35)
    (define b 29)
    (display "\nexpected pushes = ")
    (display (+ b (* a n)))
    (display "\n")))

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))


;;; because the code of stats influences the stack we cannot combine
;;; the both in the `factorial` function.  eceval resets the stack
;;; after each input.

;; maximum depth required to evaluate n! is 10 for each n.
(factorial 0) (stats 0)
(factorial 1) (stats 1)
(factorial 2) (stats 2)
(factorial 3) (stats 3)
(factorial 4) (stats 4)
(factorial 5) (stats 5)
(factorial 6) (stats 6)
(factorial 7) (stats 7)
(factorial 8) (stats 8)
(factorial 9) (stats 9)
(factorial 10) (stats 10)
(factorial 11) (stats 11)
(factorial 12) (stats 12)
(factorial 13) (stats 13)
(factorial 14) (stats 14)
(factorial 15) (stats 15)
(factorial 16) (stats 16)
(factorial 17) (stats 17)
(factorial 18) (stats 18)
(factorial 19) (stats 19)
(factorial 20) (stats 20)
(factorial 21) (stats 21)
(factorial 22) (stats 22)
(factorial 23) (stats 23)
(factorial 24) (stats 24)
(factorial 25) (stats 25)
(factorial 26) (stats 26)
(factorial 27) (stats 27)
(factorial 28) (stats 28)
(factorial 29) (stats 29)
(factorial 30) (stats 30)
(factorial 31) (stats 31)
(factorial 32) (stats 32)
(factorial 33) (stats 33)
(factorial 34) (stats 34)
(factorial 35) (stats 35)
(factorial 36) (stats 36)
(factorial 37) (stats 37)
(factorial 38) (stats 38)
(factorial 39) (stats 39)
(factorial 40) (stats 40)
(factorial 41) (stats 41)
(factorial 42) (stats 42)
(factorial 43) (stats 43)
(factorial 44) (stats 44)
(factorial 45) (stats 45)
(factorial 46) (stats 46)
(factorial 47) (stats 47)
(factorial 48) (stats 48)
(factorial 49) (stats 49)
(factorial 50) (stats 50)
(factorial 51) (stats 51)
(factorial 52) (stats 52)
(factorial 53) (stats 53)
(factorial 54) (stats 54)
(factorial 55) (stats 55)
(factorial 56) (stats 56)
(factorial 57) (stats 57)
(factorial 58) (stats 58)
(factorial 59) (stats 59)
(factorial 60) (stats 60)
(factorial 61) (stats 61)
(factorial 62) (stats 62)
(factorial 63) (stats 63)
(factorial 64) (stats 64)
(factorial 65) (stats 65)
(factorial 66) (stats 66)
(factorial 67) (stats 67)
(factorial 68) (stats 68)
(factorial 69) (stats 69)
(factorial 70) (stats 70)
(factorial 71) (stats 71)
(factorial 72) (stats 72)
(factorial 73) (stats 73)
(factorial 74) (stats 74)
(factorial 75) (stats 75)
(factorial 76) (stats 76)
(factorial 77) (stats 77)
(factorial 78) (stats 78)
(factorial 79) (stats 79)
(factorial 80) (stats 80)
(factorial 81) (stats 81)
(factorial 82) (stats 82)
(factorial 83) (stats 83)
(factorial 84) (stats 84)
(factorial 85) (stats 85)
(factorial 86) (stats 86)
(factorial 87) (stats 87)
(factorial 88) (stats 88)
(factorial 89) (stats 89)
(factorial 90) (stats 90)
(factorial 91) (stats 91)
(factorial 92) (stats 92)
(factorial 93) (stats 93)
(factorial 94) (stats 94)
(factorial 95) (stats 95)
(factorial 96) (stats 96)
(factorial 97) (stats 97)
(factorial 98) (stats 98)
(factorial 99) (stats 99)
(factorial 100) (stats 100)



