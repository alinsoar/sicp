#lang racket

(require (submod "e.2.56.rkt" export))

(define algebra-rules-from-prefix-to-infix
  '(
    ( (+ (? e) (?* t))         (@ (: e) + ((@ + (: t)))))
    ( ((? e) + (+))            (: e))
    ( (* (? e) (?* t))         (@ (: e) * ((@ * (: t)))))
    ( ((? e) * (*))            (: e))
    ))

(define algebra-rules-from-infix-to-prefix
  '(
    ( ((? e) + (?* t))        (@ + (: e) (: t)))
    ( ((? e) * (?* t))        (@ * (: e) (: t)))
    ))


(define alg-pre->in  (simplifier algebra-rules-from-prefix-to-infix))
(define alg-pre<-in  (simplifier algebra-rules-from-infix-to-prefix))

(module+ test
  "---"
  (algsimp (dsimp (alg-pre<-in (alg-pre->in '(deriv (+ 1 x 2 y 3 z 4 w) x)))))
  (algsimp (dsimp (alg-pre<-in (alg-pre->in '(deriv (+ x 2 3 4 y) x)))))
  (algsimp (dsimp (alg-pre<-in (alg-pre->in '(deriv (+ x 2 3 4 y) x)))))
  (algsimp (dsimp (alg-pre<-in (alg-pre->in '(deriv (+ x 2 3 4 y) x)))))
  (algsimp (dsimp (alg-pre<-in (alg-pre->in '(deriv (+ x 2 3 4 y) x)))))
  (algsimp (dsimp (alg-pre<-in (alg-pre->in '(deriv (+ x 2 3 4 y) x)))))
  (algsimp (dsimp (alg-pre<-in (alg-pre->in '(deriv (+ (** x 3) 2 (* 2 x y) x) x)))))
  (algsimp (dsimp (alg-pre<-in (alg-pre->in '(deriv (* 10 (+ x y)) y)))))
  (algsimp (dsimp (alg-pre<-in (alg-pre->in '(deriv x x)))))
  (algsimp (dsimp (alg-pre<-in (alg-pre->in '(deriv y x)))))
  (algsimp (dsimp (alg-pre<-in (alg-pre->in '(deriv (* 10 (+ x z) (+ x (* x y))) x)))))
  (algsimp (dsimp (alg-pre<-in (alg-pre->in '(deriv (* 20 (+ x z) (+ x (* x y))) x)))))
  (algsimp (dsimp (alg-pre<-in (alg-pre->in '(deriv (* (+ x y z) (+ x z) (+ x (* x y))) x)))))
  )

