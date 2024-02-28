#lang racket

(define (fold-left0 op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right0 op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right0 op initial (cdr sequence)))))

(define (fold-left1 op initial sequence)
  (define (iter s co)
    (if (null? s)
        (co initial)
        (iter (cdr s)
              (lambda (x)
                (op (co x) (car s))))))
  (iter sequence (lambda (x) x)))

(define (fold-right1 op initial sequence)
  (define (iter s co)
    (if (null? s)
        (co initial)
        (iter (cdr s)
              (lambda (x)
                (co (op (car s) x))))))
  (iter sequence (lambda (x) x)))

;;; They produce the same result only if the operator is commutative,
;;; id est a op b = b op a

(module+ test

  ;; first
  
  (fold-right0 / 1 (list 1 2 3))           ; (/ 1 (/ 2 (/ 3 1)))

  (fold-left0 / 1 (list 1 2 3))            ; (/ (/ (/ 1 1) 2) 3)

  (fold-right0 list '() (list 1 2 3))      ; (list 1 (list 2 (list 3 '())))

  (fold-left0 list '() (list 1 2 3))       ; (list (list (list '() 1) 2) 3)

  ;; commutative
  (fold-left0 + 1 (list 1 2 3))
  (fold-right0 + 1 (list 1 2 3))

  ;; second
  
  (fold-right1 / 1 (list 1 2 3))           ; (/ 1 (/ 2 (/ 3 1)))

  (fold-left1 / 1 (list 1 2 3))            ; (/ (/ (/ 1 1) 2) 3)

  (fold-right1 list '() (list 1 2 3))      ; (list 1 (list 2 (list 3 '())))

  (fold-left1 list '() (list 1 2 3))       ; (list (list (list '() 1) 2) 3)

  ;; commutative
  (fold-left1 + 1 (list 1 2 3))
  (fold-right1 + 1 (list 1 2 3))

  )


(module+ export
  (provide fold-left0
           fold-right0
           fold-left1
           fold-right1))
