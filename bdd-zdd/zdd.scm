
(define zdd-empty-set
  'F)
(define zdd-empty-set?
  (lambda (sos)
    (eq? sos zdd-empty-set)))

(define zdd-set-of-empty-set
  'T)
(define zdd-set-of-empty-set?
  (lambda (sos)
    (eq? sos zdd-set-of-empty-set)))

(define zdd-new-node
  (lambda (n high low)
    (list n high low)))

(define zdd-get-root car)
(define zdd-get-high cadr)
(define zdd-get-low  caddr)

(define add-set
  (lambda (sos x)
    ((lambda (s) (s s sos (sort x <)))
     (lambda (s sos set)
       (cond ((null? set)
              (if (or (zdd-empty-set? sos)
                      (zdd-set-of-empty-set? sos))
                  zdd-set-of-empty-set
                  (zdd-new-node (zdd-get-root sos)
                                (zdd-get-high sos)
                                (s s (zdd-get-low sos) '()))))
             ((zdd-empty-set? sos)
              (zdd-new-node (car set)
                            (s s zdd-empty-set (cdr set))
                            zdd-empty-set))
             ((zdd-set-of-empty-set? sos)
              (zdd-new-node (car set)
                            (s s zdd-empty-set (cdr set))
                            zdd-set-of-empty-set))
             ((= (car set) (car sos))
              (zdd-new-node (zdd-get-root sos)
                            (s s (zdd-get-high sos) (cdr set))
                            (zdd-get-low sos)))
             (else
              (zdd-new-node (zdd-get-root sos)
                            (zdd-get-high sos)
                            (s s (zdd-get-low sos) set))))))))

(define union
  (lambda (s1 s2)
    (cond ((zdd-empty-set? s1) s2)
          ((zdd-empty-set? s2) s1)
          ((and (zdd-set-of-empty-set? s2)
                (zdd-set-of-empty-set? s1))
           zdd-set-of-empty-set)
          ((zdd-set-of-empty-set? s2)
           (union zdd-set-of-empty-set s1))
          ((zdd-set-of-empty-set? s1)
           (zdd-new-node (zdd-get-root s2)
                         (zdd-get-high s2)
                         (union zdd-set-of-empty-set
                                (zdd-get-low s2))))
          ((= (zdd-get-root s1) (zdd-get-root s2))
           (zdd-new-node (zdd-get-root s1)
                         (union (zdd-get-high s1)
                                (zdd-get-high s2))
                         (union (zdd-get-low s1)
                                (zdd-get-low s2))))
          ((< (zdd-get-root s1) (zdd-get-root s2))
           (zdd-new-node (zdd-get-root s1)
                         (union (zdd-get-high s1)
                                s2)
                         (union (zdd-get-low s1)
                                s2))))))

(define zdd-get-sets
  (lambda (sos)
    ((lambda (s) (s s sos '() (lambda (a) a)))
     (lambda (s sos set succ)
       (cond ((zdd-empty-set? sos)
              (succ '()))
             ((zdd-set-of-empty-set? sos)
              (succ (list set)))
             (else
              (s s (zdd-get-low sos)
                 set
                 (lambda (acc/l)
                   (s s (zdd-get-high sos)
                      (cons (zdd-get-root sos) set)
                      (lambda (acc/h)
                        (succ (append acc/l acc/h))))))))))))

'zdd


