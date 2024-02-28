
(define-test 'built-in-predicates
  (lambda ()
    (test-built-in-predicate exact-integer? 'exact-integer 0)
    (test-built-in-predicate exact-integer? 'exact-integer (expt 2 400))
    (test-built-in-predicate exact-rational? 'exact-rational 1/2)
    (test-built-in-predicate inexact-real? 'inexact-real 1.0)
    (test-built-in-predicate complex? 'complex 1+3i)
    (test-built-in-predicate symbol? 'symbol 'a)
    (test-built-in-predicate symbol? 'symbol (generate-uninterned-symbol "foo"))))

(define (test-built-in-predicate predicate name object)
  (assert-true (predicate? predicate))
  (let ((tag (predicate->tag predicate)))
    (assert-true (tag? tag))
    (assert-eqv predicate (tag->predicate tag))
    (assert-equal name (predicate-name predicate))
    (assert-equal name (tag-name tag))
    (assert-eqv predicate (get-predicate object))
    (assert-eqv tag (get-tag object))
    (assert-eqv object (get-data object))))