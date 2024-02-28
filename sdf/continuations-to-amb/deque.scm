
;;;; Simple deque Abstraction

(define-record-type <deque>
    (%make-deque front back)
    deque?
  (front deque-front set-deque-front!)
  (back deque-back set-deque-back!))

(define (make-deque)
  (%make-deque '() '()))

(define (reset-deque! stq)
  (set-deque-front! stq '())
  (set-deque-back! stq '()))

(define (deque-empty? stq)
  (not (pair? (deque-front stq))))

(define (dequed? stq item)
  (memq item (deque-front stq)))

(define (push! stq object)
  (if (pair? (deque-front stq))
      (set-deque-front! stq (cons object (deque-front stq)))
      (begin
        (set-deque-front! stq (cons object (deque-front stq)))
        (set-deque-back! stq (deque-front stq)))))

(define (add-to-end! stq object)
  (let ((new (cons object '())))
    (if (pair? (deque-back stq))
        (set-cdr! (deque-back stq) new)
        (set-deque-front! stq new))
    (set-deque-back! stq new)))

(define (pop! stq)
  (let ((next (deque-front stq)))
    (if (not (pair? next))
        (error "Empty deque -- POP"))
    (if (pair? (cdr next))
        (set-deque-front! stq (cdr next))
        (begin
          (set-deque-front! stq '())
          (set-deque-back! stq '())))
    (car next)))