
;;;; Support layer

(define support-layer
  (make-annotation-layer 'support
    (lambda (get-name has-value? get-value)

      (define (get-default-value)
        (support-set))

      (define (get-procedure name arity)
        (declare (ignore arity))
        (case name
          ((*) support:*)
          ((/) support:/)
          ((atan2) support:atan2)
          (else support:default-procedure)))

      (define (summarize-self)
        (list (get-name)))

      (bundle layer?
              get-name has-value? get-value get-default-value
              get-procedure summarize-self))))

(define support-layer-value
  (layer-accessor support-layer))

(define (support:default-procedure base-value . args)
  (declare (ignore base-value))
  (apply support-set-union (map support-layer-value args)))

(define (support:* base-value arg1 arg2)
  (declare (ignore base-value))
  (let ((v1 (base-layer-value arg1))
        (v2 (base-layer-value arg2))
        (s1 (support-layer-value arg1))
        (s2 (support-layer-value arg2)))
    (if (exact-zero? v1)
        (if (exact-zero? v2)
            (if (< (length (support-set-elements s1))
                   (length (support-set-elements s2)))
                s1
                s2)   ;arbitrary
            s1)
        (if (exact-zero? v2)
            s2
            (support-set-union s1 s2)))))

(define (support:/ base-value arg1 arg2)
  (declare (ignore base-value))
  (let ((v1 (base-layer-value arg1))
        (s1 (support-layer-value arg1))
        (s2 (support-layer-value arg2)))
    ;; s2 can never be zero: base layer gets error.
    (if (exact-zero? v1)
        s1
        (support-set-union s1 s2))))

;; Unused but preserved for GJS sanity.
(define (support:atan2 base-value arg1 arg2)
  (declare (ignore base-value))
  (let ((v1 (base-layer-value arg1))
        (v2 (base-layer-value arg2))
        (s1 (support-layer-value arg1))
        (s2 (support-layer-value arg2)))
    ;; s2 and s2 can never both be zero
    (cond ((exact-zero? v1) s1)
          ((exact-zero? v2) s2)
          (else (support-set-union s1 s2)))))

(define (exact-zero? x)
  (and (n:number? x) (exact? x) (n:zero? x)))