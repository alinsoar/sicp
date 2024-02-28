
;;;;                The Interpreter

(define (generate-moves-using-rule-interpreter board)
  (execute-rules (map (lambda (piece)
                        (initial-pmove board piece))
                      (current-pieces board))
                 (get-evolution-rules (board-game board))
                 (get-aggregate-rules (board-game board))))

(define (execute-rules initial-pmoves evolution-rules
                       aggregate-rules)
  ((reduce compose (lambda (x) x) aggregate-rules)
   (append-map (lambda (pmove)
                 (evolve-pmove pmove evolution-rules))
               initial-pmoves)))

(define (evolve-pmove-from-text pmove evolution-rules)
  (append-map (lambda (new-pmove)
                (if (is-pmove-finished? new-pmove)
                    (list new-pmove)
                    (evolve-pmove new-pmove evolution-rules)))
              (append-map (lambda (evolution-rule)
                            (evolution-rule pmove))
                          evolution-rules)))

(define (evolve-pmove pmove evolution-rules)
  (append-map (lambda (new-pmove)
                (if (is-pmove-finished? new-pmove)
                    (list new-pmove)
                    (evolve-pmove new-pmove evolution-rules)))
              (append-map (lambda (evolution-rule)
                            (apply-evolution-rule evolution-rule
                                                  pmove))
                          evolution-rules)))

(define (apply-evolution-rule evolution-rule pmove)
  (guarantee-list-of (lambda (pmove*)
                       (and (pmove? pmove*)
                            (is-pmove-derived-from? pmove*
                                                    pmove)))
                     (evolution-rule pmove)))