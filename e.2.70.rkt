#lang racket

(require (submod "e.2.69.rkt" export))
(require (submod "e.2.68.rkt" export))
(require (submod "e.2.67.rkt" export))
(require (submod "e.2.24.rkt" export))

(define rock-songs-tree
  (generate-huffman-tree
   '((A 2)
     (BOOM 1)
     (GET 2)
     (JOB 2)
     (NA 16)
     (SHA 3)
     (YIP 9)
     (WAH 1))))

(define rock-song
  '(GET A JOB

        SHA NA NA NA NA NA NA NA NA

        GET A JOB

        SHA NA NA NA NA NA NA NA NA

        WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP

        SHA BOOM ))

(define rock-music
  (encode rock-song rock-songs-tree ))

(module+ test

  (tree-view rock-songs-tree)

  rock-music

  (length rock-music)

  (encode-symbol 'BOOM rock-songs-tree)
  (encode-symbol 'GET rock-songs-tree)
  (encode-symbol 'YIP rock-songs-tree)
  (encode-symbol 'NA rock-songs-tree)

  (equal? rock-song (decode rock-music rock-songs-tree)))

