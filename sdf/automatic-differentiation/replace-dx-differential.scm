
;;; object is a differential object...

(define (replace-dx-differential new-dx old-dx object)
  (make-differential
   (combine-like-terms
    (sort (append-map
           (lambda (term)
             (let ((factors
                    (sort (substitute new-dx old-dx
                                      (diff-factors term))
                          diff-factor>?))
                   (c (diff-coefficient term)))
               (if (or (and (number? c) (= c 0))
                       (duplicate-factors? (diff-factors term)))
                   '()
                   (list (make-diff-term c factors)))))
           (diff-terms object))
          diff-term>?))))

(define (duplicate-factors? lst)
  (and (not (null? lst))
       (let lp ((lst lst))
         (and (not (null? (cdr lst)))
              (or (eqv? (car lst) (cadr lst))
                  (lp (cdr lst)))))))

(define (combine-like-terms terms)
  (cond ((null? terms) '())
        ((null? (cdr terms)) terms)
        (else
         (let ((current (car terms)) (next (cadr terms)))
           (cond ((equal? (diff-factors current)
                          (diff-factors next))
                  (let ((newcoeff
                         (+ (diff-coefficient current)
                            (diff-coefficient next))))
                    (if (and (number? newcoeff) (= newcoeff 0))
                        (combine-like-terms (cddr terms))
                        (combine-like-terms
                         (cons (make-diff-term newcoeff
                                               (diff-factors current))
                               (cddr terms))))))
                 (else 
                  (cons current
                        (combine-like-terms (cdr terms)))))))))
               
;; coderef: replace-dx:differential
(define-generic-procedure-handler replace-dx
  (match-args diff-factor? diff-factor? differential?)
  replace-dx-differential)
