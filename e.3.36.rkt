#lang racket

(require "sicp.rkt")
(GETMOD 3 33)

;;;
;;; +---------------------------------------------------------------------------------------------------------------------+
;;; |     make-connector         set-value!                                     a           GLOBAL ENVIRONMENT            |
;;; |         +                     +                                           +                                         |
;;; +---------|---------------------|-------------------------------------------|-----------------------------------------+
;;;           |  ^                  |  ^                     ^                  |                     ^
;;;           |  |                  |  |                     |                  |                     |
;;;           |  |                  |  |          +------+   |                  |                     |       ENV-***
;;;           v  |                  v  |          |      v   |     ENV-*        |            +--------+----------+
;;;    +------+--|---+       +------+--|---+      | +--------+---------+        |            | connector: a      |
;;;    |  +   |  +   |       |  +   |  +   |      | | name: "a"        |        |            | new-value: 10     |
;;;    +--|---+------+       +--|---+------+      | +------------------+        |            | informant: 'user  |
;;;       |                     |                 | (define a                   |            +-------------------+
;;;       |                     |                 |  (make-connector "a")       |            (set-value a 10 'user)
;;;       +                     +                 |                             |
;;;    PARAM: name           PARAM: connector     +-----------      +---------+-|-------------+------------------+
;;;    CODE: MAKE-CONNECTOR  new-value informant             |      v ENV-**  | |             |                  |
;;;                          CODE: set-value!       +--------+---------+      | |             |                  |
;;;                                                 | value: false     |      | |             |                  |
;;;                                                 | informant: false |      | |             |                  |
;;;  ____________________________________________\  | constraints: '() |      | |             |                  |
;;;   in creating this environment, first        /  | set-my-value ------>..  | |             |                  |
;;;   is created a closure that is connected        | forget-my-value --->..  | |             |                  |
;;;   to the ENV-*, and afterwards that             | connect ----------->..  | |      +------+---------------+  |
;;;   closure is applied on (false false '()).      | str --------------->..  | |      | request: 'set-value! |  |
;;;   we do not represent this intermediary state.  | me ----------------+    | |      +----------------------+  |
;;;   the closure contains the code of (let ...).   +------------------+ |    | |      (connector 'set-value!)   |
;;;                                                 (let ((value ...)))  |    | |      executed from ENV-***     |
;;;                                                                      v    | V      => set-my-value           |
;;;                                                             +--------+----+----+                             |
;;;                                                             |   +    |         |                             |
;;;                                                             +---+----+---------+                             |
;;;                                                                 |                                            |
;;;                                                                 +                                            |
;;;                                                             PARAM: request                                   |
;;;                                                             CODE: make-connector.me                          |
;;;                                                                                                              |
;;;                                                                       +--------------------------------+     |
;;;                                                                       |                                V     |
;;;                                                                       |                           +----------+--------+
;;;                                                                       |                           | newval: new-value |
;;;                                                                       |                           | setter: informant |
;;;                                                                       |                           +-------------------+
;;;                                                         +-------------+-----------------+         (set-my-value new-value
;;;                                                         | exception: setter             |                       informant)
;;;                                                         | procedure: inform-about-value |         executed from ENV-***
;;;                                                         | list: constraints             |
;;;                                                         +-------------------------------+
;;;                                                         (for-each-constraint-except setter
;;;                                                                                     inform-about-value
;;;                                                                                     constraints)
;;;

(module+ test
  (define a (make-connector "a"))
  (define b (make-connector "b"))

  (set-value! a 10 'user)
  'done)
