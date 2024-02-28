
;;;; Sets of values

;;; The set elements must be distinct according to equivalent?
(define-record-type <value-set>
    (%make-value-set elements)
    value-set?
  (elements value-set-elements))
(register-predicate! value-set? 'value-set)

(define (make-value-set elements)
  (guarantee list? elements 'make-value-set)
  (%make-value-set (remove nothing? elements)))

(define (value-set . elements)
  (%make-value-set (remove nothing? elements)))

(define (->value-set value)
  (if (value-set? value)
      value
      (value-set value)))

(define-generic-procedure-handler get-base-value
  (match-args value-set?)
  (lambda (set) (get-base-value (strongest-consequence set))))

(define-generic-procedure-handler unusable-value?
  (match-args value-set?)
  (lambda (set) (unusable-value? (strongest-consequence set))))

;; coderef: strongest-value-values
(define-generic-procedure-handler strongest-value
  (match-args value-set?)
  (lambda (set) (strongest-consequence set)))

(define (map-value-set procedure . sets)
  (make-value-set
   (apply map
          procedure
          (map value-set-elements sets))))

;;;; Merge

(define (merge-value-sets content increment)
  (if (nothing? increment)
      (->value-set content)
      (value-set-adjoin (->value-set content) increment)))

(define (value-set-adjoin set elt)
  (if (any (lambda (old-elt)
             (element-subsumes? old-elt elt))
           (value-set-elements set))
      set
      (make-value-set
       (lset-adjoin equivalent?
                    ;; Potential optimization:
                    ;; (remove (lambda (old-elt)
                    ;;           (element-subsumes? elt old-elt))
                    ;;         (value-set-elements set))
                    (value-set-elements set)
                    elt))))

(define (element-subsumes? elt1 elt2)
  (and (value-implies? (base-layer-value elt1)
                       (base-layer-value elt2))
       (support-set<= (support-layer-value elt1)
                      (support-layer-value elt2))))

(define (strongest-consequence set)
  (fold (lambda (increment content)
          (merge-layered content increment))
        the-nothing
        (filter (lambda (elt)
                  (all-premises-in? (support-layer-value elt)))
                (value-set-elements set))))