
;;;; Generic tag access

(define get-tag
  (simple-generic-procedure 'get-tag 1
    (lambda (object)
      (implementation-tag object))))

(define (get-predicate object)
  (tag->predicate (get-tag object)))

(define get-data
  (simple-generic-procedure 'get-data 1
    (lambda (object) object)))

;;;; Tagged data

(define-record-type <tagged-data>
    (%make-tagged-data tag data)
    tagged-data?
  (tag tagged-data-tag)
  (data tagged-data-data))

(define-generic-procedure-handler get-tag
  (match-args tagged-data?)
  tagged-data-tag)

(define-generic-procedure-handler get-data
  (match-args tagged-data?)
  tagged-data-data)

(define (tagged-data= t1 t2)
  (and (eq? (tagged-data-tag t1) (tagged-data-tag t2))
       (equal*? (tagged-data-data t1) (tagged-data-data t2))))

(define-generic-procedure-handler equal*?
  (match-args tagged-data? tagged-data?)
  tagged-data=)

(define tagged-data-representation
  ((generic-procedure-constructor make-chaining-dispatch-store)
   'tagged-data-representation 1
   (lambda (tagged-data)
     (list (tagged-data-data tagged-data)))))

;;; MIT/GNU Scheme: integrate with printer
(define-print-method tagged-data?
  (standard-print-method
      (lambda (tagged-data)
        (tag-name (tagged-data-tag tagged-data)))
    tagged-data-representation))

(define tagged-data-description
  (most-specific-generic-procedure 'tagged-data-description 1
    (constant-generic-procedure-handler #f)))

;;; MIT/GNU Scheme: integrate with pretty-printer
(define-pp-describer tagged-data?
  tagged-data-description)

;;;; Tagging strategies

(define (tagging-strategy:never name data-test make-tag)

  (define (constructor data)
    (if (not (data-test data))
        (error (string "Ill-formed data for " name ":")
               data))
    data)

  (define tag
    (make-tag data-test constructor (lambda (object) object)))

  tag)

(define (tagging-strategy:always name data-test make-tag)

  (define (constructor data)
    (if (not (data-test data))
        (error (string "Ill-formed data for " name ":")
               data))
    (%make-tagged-data tag data))

  (define (predicate object)
    (and (tagged-data? object)
         (tag<= (tagged-data-tag object) tag)
         (data-test (tagged-data-data object))))

  (define tag
    (make-tag predicate constructor tagged-data-data))

  tag)

(define (tagging-strategy:optional name data-test make-tag)

  (define (constructor data)
    (if (not (data-test data))
        (error (string "Ill-formed data for " name ":")
               data))
    (if (eq? tag (get-tag data))
        data
        (%make-tagged-data tag data)))

  (define (predicate object)
    (or (and (tagged-data? object)
             (tag<= (tagged-data-tag object) tag)
             (data-test (tagged-data-data object)))
        (data-test object)))

  (define (accessor object)
    (if (tagged-data? object)
        (tagged-data-data object)
        object))

  (define tag
    (make-tag predicate constructor accessor))

  tag)