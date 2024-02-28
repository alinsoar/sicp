
;;;; Support sets

(define (%make-support-set elements)
  (cons 'support-set elements))

(define (support-set? object)
  (and (pair? object)
       (eq? 'support-set (car object))
       (list? (cdr object))))
(register-predicate! support-set? 'support-set)

(define (support-set-elements support-set)
  (cdr support-set))

(define (make-support-set elements)
  (guarantee list? elements 'make-support-set)
  (if (null? elements)
      %empty-support-set
      (%make-support-set (delete-duplicates elements))))

(define (support-set . elements)
  (if (null? elements)
      %empty-support-set
      (%make-support-set (delete-duplicates elements))))

(define %empty-support-set
  (%make-support-set '()))

(define (support-set-empty? s)
  (null? (support-set-elements s)))

(define (support-set-adjoin set . elts)
  (make-support-set
   (apply lset-adjoin eqv? (support-set-elements set) elts)))

(define (support-set-union . sets)
  (make-support-set
   (apply lset-union
          eqv?
          (map support-set-elements sets))))

(define (support-set= set1 set2)
  (lset= eqv?
         (support-set-elements set1)
         (support-set-elements set2)))

(define (support-set<= set1 set2)
  (lset<= eqv?
          (support-set-elements set1)
          (support-set-elements set2)))

(define (support-set< set1 set2)
  (and (not (support-set= set1 set2))
       (support-set<= set1 set2)))

(define (support-set-remove set . elts)
  (make-support-set
   (lset-difference eqv?
                    (support-set-elements set)
                    elts)))

(define (support-set-filter predicate . sets)
  (make-support-set
   (apply filter
          predicate
          (map support-set-elements sets))))

(define (support-set-every predicate . sets)
  (apply every
         predicate
         (map support-set-elements sets)))

(define (support-set-any predicate . sets)
  (apply any
         predicate
         (map support-set-elements sets)))