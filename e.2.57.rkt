#lang racket

(require (submod "e.2.56.rkt" export))

(define algebra-rules-extension-shrink
  '(
    ( ((? op) (? e1) ((? op) (? e2) (?* t)))     (@ (: op) (: e1) (: e2) (: t))  )
    ))

(define algebra-rules-extension-expand
  '(
    ( ((? op) (? e1) (? e2) (? e3) (?* t))    ((: op) (: e1)
                                               (@ (: op) (: e2) (: e3) (: t))))
    ))


(define alg-expand  (simplifier algebra-rules-extension-expand))
(define alg-shrink (simplifier algebra-rules-extension-shrink))

(module+ test
  "---"
  (alg-expand '(deriv (+ x 2 3 4 y) x))
  (algsimp (alg-expand '(deriv (+ x 2 3 4 y) x)))
  (alg-shrink (alg-expand '(deriv (+ x 2 3 4 y) x)))
  (alg-shrink (algsimp (alg-expand '(deriv (+ x 2 3 4 y) x))))
  (dsimp (algsimp (alg-expand '(deriv (+ x 2 3 4 y) x))))
  (algsimp (dsimp (algsimp (alg-expand '(deriv (+ x 2 3 4 y) x)))))
  (alg-shrink (algsimp (dsimp (alg-expand '(deriv (+ (** x 3) 2 (* 2 x y) x) x)))))
  (alg-shrink (algsimp (dsimp (alg-expand '(deriv (* 10 (+ x y)) y)))))
  (alg-shrink (algsimp (dsimp (alg-expand '(deriv x x)))))
  (alg-shrink (algsimp (dsimp (alg-expand '(deriv y x)))))
  (alg-shrink (algsimp (dsimp (alg-expand '(deriv (* 10 (+ x z) (+ x (* x y))) x)))))
  (alg-shrink (algsimp (dsimp (alg-expand '(deriv (* 20 (+ x z) (+ x (* x y))) x)))))
  (alg-shrink (algsimp (dsimp (alg-expand '(deriv (* (+ x y z) (+ x z) (+ x (* x y))) x)))))
  )

