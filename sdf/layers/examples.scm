
(define (generic-symbolic)
  (let ((g (make-generic-arithmetic make-simple-dispatch-store)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    (extend-generic-arithmetic! g symbolic-extender)
    g))

;;; Works

(define generic-with-layers
  (let ((g (generic-symbolic)))
    (extend-generic-arithmetic! g layered-extender)
    g))

(install-arithmetic! generic-with-layers)

(+ 1/2 1/2)
'expect-value: 1

;; coderef: ke-with-units
(define (KE m v)
  (* 1/2 m (square v)))

;; coderef: ke-with-units-1
(pp (KE (layered-datum 'm unit-layer (unit 'kilogram 1))
        (layered-datum 'v
                       unit-layer (unit 'meter 1 'second -1))))
'expect-description:
'((base-layer (* (* 1/2 m) (square v)))
  (unit-layer (unit kilogram 1 meter 2 second -2)))

(install-arithmetic! (generic-symbolic))

;; coderef: square
(define (square x) (* x x))

;; coderef: layered-square
(define layered-square
  (make-layered-procedure 'square 1 square))

;; coderef: layered-square-4
(layered-square 4)
'expect-value: 16

;; coderef: layered-square-x
(layered-square 'm)
'expect-value: '(* m m)

;; coderef: layered-square-m+units
(pp (layered-square
     (layered-datum 'm
                    unit-layer (unit 'kilogram 1))))
'expect-description:
'((base-layer (* m m))
  (unit-layer (unit kilogram 2)))