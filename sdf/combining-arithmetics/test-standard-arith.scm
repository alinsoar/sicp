
(define-arith-test 'pure-numeric-function
  (lambda ()
    (extend-arithmetic pure-function-extender
                       numeric-arithmetic))
  (lambda ()
    (assert-equal -.8488724885405782 ((+ cos sin) 3))
    (assert-equal -1.6977449770811563 (* 2 ((+ cos sin) 3)))
    (assert-error (lambda () (* 2 ((+ 1 cos sin) 3))))))

(define-arith-test 'mixed-numeric-function
  (lambda ()
    (extend-arithmetic function-extender
                       numeric-arithmetic))
  (lambda ()
    (assert-equal -.8488724885405782 ((+ cos sin) 3))
    (assert-equal -1.6977449770811563 (* 2 ((+ cos sin) 3)))
    (assert-equal .3022550229188436 (* 2 ((+ 1 cos sin) 3)))))

(define-arith-test 'mixed-symbolic-function
  (lambda ()
    (extend-arithmetic function-extender
                       combined-arithmetic))
  (lambda ()
    (test-mixed-symbolic-function)))

(define-arith-test 'mixed-symbolic-function-with-coercion
  (lambda ()
    (extend-arithmetic function-extender-with-coercion
                       combined-arithmetic))
  (lambda ()
    (test-mixed-symbolic-function)))

(define (test-mixed-symbolic-function)
  (assert-equal '(* b (+ (+ 4 (cos (+ 3 a))) (sin (+ 3 a))))
                (* 'b ((+ 4 cos sin) (+ 3 'a))))
  (assert-equal '(+ a (+ (cos b) (sin b)))
                (+ 'a ((+ cos sin) 'b)))
  (assert-equal '(+ a -.8488724885405782)
                (+ 'a ((+ cos sin) 3)))
  (assert-equal '(+ a (+ (+ 3 (cos b)) (sin b)))
                (+ 'a ((+ 3 cos sin) 'b)))
  (assert-equal '(+ a (+ (+ c (cos b)) (sin b)))
                (+ 'a ((+ 'c cos sin) 'b)))
  (assert-equal '(+ a (+ (+ (c b) (cos b)) (sin b)))
                (+ 'a ((+ (literal-function 'c) cos sin) 'b))))

#|
;; Most powerful but has issues.  Doesn't work at present.

(define-arith-test 'closed-function
  (lambda ()
    (extend-and-close-arithmetic combined-arithmetic
                                 function-extender))
  (lambda ()
    (assert-equal '(+ a (+ 3 (cos (sin b))))
                  (+ 'a ((+ 3 (cos sin)) 'b)))
    (assert-equal '(+ (3 . 4) (4 . 3))
                  (((+ (lambda (x) (lambda (y) (cons x y)))
                       (lambda (x) (lambda (y) (cons y x))))
                    3)
                   4))
    ;; Works when extend-and-close-environment prioritizes the closure
    ;; over the base, fails otherwise.
    (assert-equal '(+ a (+ c (cos b) (sin b)))
                  (+ 'a ((+ 'c cos sin) 'b)))))
|#