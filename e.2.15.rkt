#lang racket


;;; The interval arithmetic should be the arithmetic of the ranges of
;;; functions.

;;; A function like f (x) = x^2 defined on a domain [-1, 1] has the
;;; range [0,1], which is included in [-1,1]*[-1,1] = [-1,1], obtained
;;; by replacing the symbol `x` by the domain of the symbol `x`.

;;; If we define a similar function that uses a different variable for
;;; each dimension, like in f(x,y) = x*y, then the range of this
;;; function, when defined on the domain [-1,1]*[-1,1], is the same as
;;; the interval [-1,1]*[-1,1] = [-1, 1].

;;; It happens that all the time when the function f(.., x, ..) is
;;; continous in each variable `x` we have the range arithmetics to be
;;; identical with interval arithmetics if each symbol is used only
;;; once in definition of f.

;;; In the first formula of Alice, parallel resistor is computed
;;; repeating 2 times the variable R1, and 2 times the variable R2,
;;; and using the same argument the range of this function is included
;;; in the product of the corresponding intervals obtained from the
;;; formula of the function, by replacing each name by corresponding
;;; domain interval, but it is not strictly the same.

