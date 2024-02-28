
(define chess
  (make-game 8 8 '(white black) '(pawn rook knight bishop queen king)
             chess-initial-pieces
             generate-moves-using-rule-interpreter
             chess-piece-summary))

(define (chess-initial-pieces game)
  (let ((choose-type
         (lambda (column)
           (case column
             ((0 7) 'rook)
             ((1 6) 'knight)
             ((2 5) 'bishop)
             ((3) 'king)
             ((4) 'queen)
             (else (error "Incorrect column:" column))))))
    (append-map (lambda (color)
                  (append-map (lambda (column)
                                (let ((make
                                       (lambda (type column row)
                                         (make-piece color type
                                                     (make-coords column
                                                                  row)))))
                                 (list (make 'pawn column 1)
                                       (make (choose-type column)
                                         column
                                         0))))
                              (iota (game-width game))))
                (game-colors game))))

(define (chess-piece-summary piece)
  (if piece
      (string (case (piece-type piece)
                ((pawn) #\P)
                ((rook) #\R)
                ((knight) #\N)
                ((bishop) #\B)
                ((king) #\K)
                ((queen) #\Q)
                (else (error "Unknown type:" piece)))
              (case (piece-color piece)
                ((black) #\b)
                ((white) #\w)
                (else (error "Unknown color:" piece))))
      "  "))