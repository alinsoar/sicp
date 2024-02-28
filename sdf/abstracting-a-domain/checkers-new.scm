
(define checkers
  (make-checkers generate-moves-using-rule-interpreter))

;; coderef: evolution-rule:simple-move
(define-evolution-rule 'simple-move checkers
  (lambda (pmove)
    (if (is-pmove-empty? pmove)
        (get-simple-moves pmove)
        '())))

(define (get-simple-moves pmove)
  (filter-map
   (lambda (direction)
     (let ((landing
            (compute-new-position direction 1 pmove))
           (board (current-board pmove)))
       (and (is-position-on-board? landing board)
            (is-position-unoccupied? landing board)
            (finish-move (new-piece-position landing pmove)))))
   (possible-directions (current-piece pmove))))

;; coderef: evolution-rule:jump
(define-evolution-rule 'jump checkers
  (lambda (pmove)
    (let ((jumps (get-jumps pmove)))
      (cond ((not (null? jumps))
             jumps)
            ((is-pmove-empty? pmove)
             '())
            (else
             (list (finish-move pmove)))))))

(define (get-jumps pmove)
  (filter-map
   (lambda (direction)
     (let ((possible-jump
            (compute-new-position direction 1 pmove))
           (landing (compute-new-position direction 2 pmove))
           (board (current-board pmove)))
       (and (is-position-on-board? landing board)
            (is-position-unoccupied? landing board)
            (is-position-occupied-by-opponent? possible-jump
                                               board)
            (capture-piece-at possible-jump
                              (new-piece-position landing
                                                  pmove)))))
   (possible-directions (current-piece pmove))))

;; coderef: aggregate-rule:coronation
(define-aggregate-rule 'coronation checkers
  (lambda (pmoves)
    (map (lambda (pmove)
           (let ((piece (current-piece pmove)))
             (if (should-be-crowned? piece)
                 (update-piece crown-piece pmove)
                 pmove)))
         pmoves)))

;; coderef: aggregate-rule:require-jumps
(define-aggregate-rule 'require-jumps checkers
  (lambda (pmoves)
    (let ((jumps (filter captures-pieces? pmoves)))
      (if (null? jumps)
          pmoves
          jumps))))