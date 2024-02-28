#lang racket

(require "sicp.rkt")
(GETMOD 4 35)

(module+ test
  (amb-test
   '(
     (define (memq a l)
       (cond ((null? l) false)
             ((eq? a (car l)) l)
             (else
              (memq a (cdr l)))))
     
     (define nouns '(noun student professor cat class))

     (define verbs '(verb studies lectures eats sleeps))

     (define articles '(article the a))

     (define prepositions '(prep for to in by with))

     (define (parse-word word-list)
       (require (not (null? *unparsed*)))
       (require (memq (car *unparsed*) (cdr word-list)))
       (let ((found-word (car *unparsed*)))
         (set! *unparsed* (cdr *unparsed*))
         (list (car word-list) found-word)))

     (define *unparsed* '())

     (define (parse-sentence0)
       (list 'sentence
             (parse-noun-phrase0)
             (parse-word verbs)))
     
     (define (parse-noun-phrase0)
       "reverse the order of evaluation between articles and nouns."
       ((lambda (parse-nouns)
          ((lambda (parse-articles)
             (list 'noun-phrase
                   parse-articles
                   parse-nouns))
           (parse-word articles)))
        (parse-word nouns)))

     (define (parse0 input)
       (set! *unparsed* input)
       (let ((sent (parse-sentence0)))
         (require (null? *unparsed*))
         sent))))
  (amb-test '("===================="
              (parse0 '(the cat eats))
              try-again
              
              ))
  'done)





