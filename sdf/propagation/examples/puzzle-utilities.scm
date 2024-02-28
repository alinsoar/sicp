
(define (require-distinct cells)
  (compound-propagator cells '()
    (lambda ()
      (for-each-distinct-pair
       (lambda (c1 c2)
         (let-cells (p)
           (p:= c1 c2 p)
           (abhor p)))
       cells))
    'require-distinct))

(define (one-of values output-cell)
  (compound-propagator '() (list output-cell)
    (lambda ()
      (let ((cells
             (map (lambda (value)
                    (let-cells ((cell value))
                      cell))
                  values)))
        (one-of-the-cells cells output-cell)))
    'one-of))

(define (one-of-the-cells input-cells output-cell)
  (cond ((n:= (length input-cells) 2)
         (let-cells (p)
           (p:conditional p
                          (car input-cells)
                          (cadr input-cells)
                          output-cell)
           (binary-amb p)))
        ((n:> (length input-cells) 2)
         (let-cells (link p)
           (one-of-the-cells (cdr input-cells) link)
           (p:conditional p
                          (car input-cells)
                          link
                          output-cell)
           (binary-amb p)))
        (else
         (error "Inadequate choices for one-of-the-cells"
                input-cells output-cell))))