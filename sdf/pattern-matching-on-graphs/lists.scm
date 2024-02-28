
;;; Experimental list generators

(define (list->graph list)
  (if (pair? list)
      (g:cons (car list) (list->graph (cdr list)))
      (g:null)))

(define (list->lazy-graph list)
  (if (pair? list)
      (g:cons (delay (car list))
              (delay (list->lazy-graph (cdr list))))
      (g:null)))

(define (list->extensible-lazy-graph list)
  (if (not (pair? list))
      (error "Can't implement empty extensible list."))
  (let loop ((list list))
    (let ((head (make-graph-node 'pair)))
      (head 'connect! 'car (delay (car list)))
      (if (pair? (cdr list))
          (head 'connect! 'cdr (delay (loop (cdr list)))))
      head)))

(define nil
  (make-graph-node 'nil))

(define (g:null)
  nil)

(define (g:null? object)
  (eqv? object nil))

(define (g:cons car cdr)
  (let ((pair (make-graph-node 'pair)))
    (pair 'connect! 'car car)
    (pair 'connect! 'cdr cdr)
    pair))

(define (g:car pair)
  (pair 'edge-value 'car))

(define (g:cdr pair)
  (pair 'edge-value 'cdr))

(define (g:has-cdr? pair)
  (pair 'has-edge? 'cdr))

(define (g:last-pair list)
  (if (g:has-cdr? list)
      (let ((cdr (g:cdr list)))
        (if (g:null? cdr)
            list
            (g:last-pair cdr)))
      list))

(define (g:last list)
  (g:car (g:last-pair list)))

(define (g:append! l1 l2)
  ((g:last-pair l1) 'connect! 'cdr l2))
