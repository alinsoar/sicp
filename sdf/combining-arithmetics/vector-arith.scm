
(register-predicate! vector? 'vector)

(define (ensure-vector-lengths-match vecs)
  (let ((first-vec-length (vector-length (car vecs))))
    (if (any (lambda (v)
               (not (n:= (vector-length v)
                         first-vec-length)))
             vecs)
        (error "Vector dimension mismatch:" vecs))))

(define (vector-element-wise element-procedure)
  (lambda vecs    ; Note: this takes multiple vectors
    (ensure-vector-lengths-match vecs)
    (apply vector-map element-procedure vecs)))

(define ((vector-sum-maker +) v)
  (let ((n (vector-length v)))
    (let lp ((i 1) (sum (vector-ref v 0)))
      (if (n:= i n)
          sum
          (lp (n:+ i 1)
              (+ sum (vector-ref v i)))))))

(define (dot-product-maker + *)
  (let ((vector-sum (vector-sum-maker +)))
    (define (dot-product v1 v2)
      (vector-sum
       (vector-map * v1 v2)))
  dot-product))

(define (left-scalar-product-maker *)
  (define (scalar-product c v)
    (vector-map (lambda (x) (* c x))
                v))
  scalar-product)

(define (right-scalar-product-maker *)
  (define (scalar-product v c)
    (vector-map (lambda (x) (* x c))
                v))
  scalar-product)

(define (vector-magnitude-maker + * sqrt)
  (let ((dot-product (dot-product-maker + *)))
    (define (vector-magnitude v)
      (sqrt (dot-product v v)))
    vector-magnitude))

(define (vector-extender component-arithmetic)
  (let ((component-predicate
         (arithmetic-domain-predicate component-arithmetic))
        (component-proc
         (lambda (operator)
           (operation-procedure
            (arithmetic-operation operator component-arithmetic)))))
    (let ((+ (component-proc '+))
          (- (component-proc '-))
          (* (component-proc '*))
          (negate (component-proc 'negate))
          (sqrt (component-proc 'sqrt)))
      (let ((dot-product (dot-product-maker + *))
            (left-scalar-product (left-scalar-product-maker *))
            (right-scalar-product (right-scalar-product-maker *))
            (magnitude (vector-magnitude-maker + * sqrt)))
        (make-arithmetic 'vector
          (disjoin component-predicate vector?)
          (list component-arithmetic)
          (lambda (name component-constant)
            (default-object))
          (lambda (operator component-operation)
            (case operator
              ((+)
               (make-operation operator
                               (all-args (operator-arity operator)
                                         vector?)
                               (vector-element-wise +)))
              ((-)
               (make-operation operator
                               (all-args (operator-arity operator)
                                         vector?)
                               (vector-element-wise -)))
              ((*)
               (operation-union
                operator
                (make-operation operator
                                (match-args component-predicate vector?)
                                left-scalar-product)
                (make-operation operator
                                (match-args vector? component-predicate)
                                right-scalar-product)
                (make-operation operator
                                (all-args (operator-arity operator)
                                          vector?)
                                dot-product)))
              ((magnitude)
               (make-operation operator
                               (all-args (operator-arity operator)
                                         vector?)
                               magnitude))
              ((negate)
               (make-operation operator
                               (all-args (operator-arity operator)
                                         vector?)
                               (vector-element-wise negate)))
              (else #f))))))))

#|
(define combined-arithmetic
  (extend-arithmetic symbolic-extender
                     numeric-arithmetic))

(define combined-with-functions
  (extend-arithmetic function-extender
                     combined-arithmetic))


(install-arithmetic!
 (extend-arithmetic vector-extender
                    combined-with-functions))

(+ #(1 2 3) #(4 5 6))
;Value: #(5 7 9)

(- #(4 5 6) #(1 1 1) #(1 2 3))
;Value: #(2 2 2)

;;; Dot Products and Scalar Products
(* #(1 2 3) #(1 2 3))
;Value: 14


(* 3 #(1 2 3))
;Value: #(3 6 9)

(* #(1 2 3) 3)
;Value: #(3 6 9)


(magnitude #(3 4))
;Value: 5


;;; These should also work with symbols, element-wise:

(+ #(1 2 3) #(4 a 6))
;Value: #(5 (+ 2 a) 9)

(magnitude #(1 2 a))
;Value: (sqrt (+ 5 (* a a)))

(define square (lambda (x) (* x x)))

((magnitude (vector square square)) 'a)
;Value: (sqrt (+ (* (* a a) (* a a)) (* (* a a) (* a a))))

;;; Stormer works!

(define ss0
  (make-initial-history 0 .01
                        (vector (sin 0) (cos 0))
                        (vector (sin -.01) (cos -.01))
                        (vector (sin -.02) (cos -.02))))


(define (F t x) (- x))

(x 0 ((evolver F .01 stormer-2) ss0 100))
;Value: #(.8414709493275624 .540302318220704)
|#