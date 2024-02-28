
;;;; Data modelling

(define get-base-value
  (simple-generic-procedure 'get-base-value 1 base-layer-value))

(define (equivalent? object1 object2)
  (or (eqv? object1 object2)
      (g:equivalent? object1 object2)))

(define g:equivalent?
  (simple-generic-procedure 'equivalent? 2
    (lambda (x y)
      (declare (ignore x y))
      #f)))

(define-generic-procedure-handler g:equivalent?
  (match-args n:number? n:number?)
  ~=?)

(define value-implies?
  (simple-generic-procedure 'value-implies? 2 eqv?))

(define-generic-procedure-handler value-implies?
  (match-args n:number? n:number?)
  equivalent?)

(define unusable-value?
  (simple-generic-procedure 'unusable-value? 1
    (constant-generic-procedure-handler #f)))

(define strongest-value
  (simple-generic-procedure 'strongest-value 1
    (lambda (object) object)))

;; Arguments:
;; * CONTENT is the current content of the cell
;; * INCREMENT is an incremental update to the cell
;;
;; MERGE returns either a contradiction or a merged value.

(define merge
  (simple-generic-procedure 'merge 2
    (lambda (content increment)
      (cond ((nothing? content) increment)
            ((nothing? increment) content)
            ((contradiction? content) content)
            ((contradiction? increment) increment)
            ((equivalent? content increment) content)
            (else the-contradiction)))))

(define merge-layered
  (make-layered-procedure 'merge 2 merge))

;; Like merge but only on added layers.
(define (merge-metadata content increment)
  (declare (ignore increment))
  content)

;; Like merge-layered but only on added layers.
(define merge-metadata-layered
  (make-layered-procedure 'merge-metadata 2 merge-metadata))