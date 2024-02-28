
;;;; Games

#|
Assumptions:
    two players
    rectangular board
    depth of board is distance between player sides
        width of board is perpendicular to depth
    coordinates are relative to player sides
        (0 0) is lower-left corner for player
        increasing y is forward
        increasing x is right
        a row has a fixed y
        a column has a fixed x
    player's home row is defined as y = 0
    at most one piece per position
    one piece moves per turn
    players alternate turns
    pieces are conserved except for capture
    game is over when no turns are possible;
        there may be additional ending conditions
|#

(define-record-type <game>
    (%make-game width depth colors types initial-pieces-generator
                moves-generator piece-summarizer
                evolution-rules aggregate-rules)
    game?
  (width game-width)
  (depth game-depth)
  (colors game-colors)
  (types game-types)
  (initial-pieces-generator initial-pieces-generator)
  (moves-generator moves-generator)
  (piece-summarizer piece-summarizer)
  (evolution-rules %get-evolution-rules %set-evolution-rules!)
  (aggregate-rules %get-aggregate-rules %set-aggregate-rules!))

(define (make-game width depth colors types initial-pieces-generator
                   moves-generator piece-summarizer)
  (guarantee list-of-unique-symbols? colors)
  (if (not (= (length colors) 2))
      (error "Board supports only two colors:" colors))
  (guarantee list-of-unique-symbols? types)
  (if (null? types)
      (error "Must specify at least one type"))
  (guarantee procedure? initial-pieces-generator)
  (guarantee procedure? moves-generator)
  (guarantee procedure? piece-summarizer)
  (%make-game width depth colors types initial-pieces-generator
              moves-generator piece-summarizer '() '()))

(define (get-evolution-rules game)
  (map cdr (%get-evolution-rules game)))

(define (get-aggregate-rules game)
  (map cdr (%get-aggregate-rules game)))

(define (define-evolution-rule name game procedure)
  (%define-rule %get-evolution-rules %set-evolution-rules!
                name game procedure))

(define (define-aggregate-rule name game procedure)
  (%define-rule %get-aggregate-rules %set-aggregate-rules!
                name game procedure))

(define (%define-rule get-rules set-rules name game procedure)
  (let ((rules (get-rules game)))
    (let ((p (assq name rules)))
      (if p
          (set-cdr! p procedure)
          (set-rules game (cons (cons name procedure) rules))))))