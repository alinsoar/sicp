#lang racket


(define tolerance 0.0001)

(define (cube x) (* x x x))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) tolerance))

(define (square x) (* x x))

(define (improve guess x)
  (/ (+ (* 2 guess)
        (/ x (square guess)))
     3))

(define (cube-root guess x)
;;  (display guess)
;;  (display ":")
;;  (display (cube guess))
;;  (newline)
  (if (good-enough? guess x)
          guess
          (cube-root (improve guess x)
                     x )))

(define root (cube-root 1.0 0.27))

(display root)
(newline)
(display (* (cube root)))

;; f (x) = x^3 - 3x^2 + x -1 => initial guess x0 = 1 one gets
;; x1 = 0, x2 = 1, x3 = 0 and so on

;;;  The method of Newton:
;;;
;;;
;;;
;;;                       \
;;;   |                    \  tan to f in (x0, f(x0))
;;;   |                      \
;;;   |       *                \
;;;   |           *              \
;;;   |               *           \
;;;   |                   *         \
;;;   |                      *        \
;;;   |                         *       \
;;;   |                           *      \
;;;   |                              *     \
;;;   |                                *     \
;;;   |                                   *    \
;;;   |                                      *  \
;;;   |                                        *  \
;;;   |                                          *  \
;;;   |                                             * \
;;;   |                                               *\
;;;   |                                                 *\
;;;   | f(x0)                                             *\
;;;   |----------------------------------------------------X\
;;;   |                                                    | *\
;;;   |                                                    |   *\
;;;   |                                                    |    * \
;;;   |                                                    |     *  \
;;;   |                                                    |      *  \
;;;   |                                                    |       *   \
;;;   |                                                    |        *    \
;;;   |                                                    |         *    \
;;;   |                                                    |          *     \
;;;   |                                                    |           *      \
;;;   |                                                    |           *        \
;;;   |                                                    |            *        \
;;;   |                                                    |             * Z       \
;;;---+---------------------------------------------------+--------------*-----------\---------
;;;   |                                                   x0              *            \ x1
;;;   |                                                                    *             \
;;;   |                                                                     *             \
;;;   |                                                                       *             \
;;;   |                                                                                       \

