
(load "aux/msim")

(define (sqrt x)
  (define (square x) (* x x))
  (define (average x y) (/ (+ x y) 2))
  (define (good-enough? guess) (< (abs (- (square guess) x)) 0.001))
  (define (improve guess) (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (__d ":" guess)
    (if (good-enough? guess) guess (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;;(good-enough? ,(lambda (guess) (< (abs (- (square guess) x)) 0.001)))
(define sqrt/machine
  (make-machine
   '(n guess t1 t2 t3 x)
   `((< ,<) (- ,-) (* ,*) (abs ,abs) (+ ,+) (/ ,/) (__d ,__d))
   '(controller
     (assign guess (const 1.0))
     sqrt-iter
     (perform (op __d) (const "-") (reg guess))
     ;; good-enogh?
     (assign t1 (op *) (reg guess) (reg guess))
     (assign t2 (op -) (reg t1) (reg n))
     (assign t3 (op abs) (reg t2))
     (test (op <) (reg t3) (const 0.001))
     (branch (label done))
     ;; not good enough
     (assign t1 (op /) (reg n) (reg guess))
     (assign t1 (op +) (reg t1) (reg guess))
     (assign guess (op /) (reg t1) (const 2))
     (goto (label sqrt-iter))
     done)))

(__d (sqrt 15))

(set-register-contents! sqrt/machine 'n 15)
(start sqrt/machine)
(__d (get-register-contents sqrt/machine 'guess))

;;;;     o-------o
;;;;      \ 1.0 /
;;;;       \   /
;;;;        \ /
;;;;         V       +------------------------+
;;;;         |       |                        |
;;;;         X g<-1  X g<-i                   |
;;;;         |       |                        |
;;;;      +--v-------v--+                     |
;;;;      |    guess    |                     |
;;;;      +------+------+                     |
;;;;             |                            |
;;;;             +-------------------+        |
;;;;             |                   |        |
;;;;     o-------v------o     +------v------+ |
;;;;    (  good-enough?  )    |   improve   | |
;;;;     o--------------o     +------+------+ |
;;;;                                 |        |
;;;;                                 +--------+
;;;;




;;;;                IMPROVE                                         GOOD-ENOUGH?
;;;;
;;;;               +-----------+   +-----------+                    +-------------+  +-----+ o-------o
;;;;               |     x     |   |   guess   |                    |   guess     |  |  x  |  \0.001/
;;;;               +-----+-----+   +-----+-----+                    +------+------+  +--+--+   \   /
;;;;                     |               |                                 |            |       \ /
;;;;                     +-----+    +----+                                 |            |        V
;;;;                           |    |    |                                 +---+        |        |
;;;;                        +--v----v--+ |                                 |   |        |        |
;;;;                        |     /    | |                              +--v---v--+     |        |
;;;;                        +-----+----+ |                              |    *    |     |        |
;;;;                              |      |                              +----+----+     |        |
;;;;                         +----v------v----+                              |          |        |
;;;;                         |    average     |                              |          |        |
;;;;                         +----------------+                           +--v----------v-+      |
;;;;                                                                      |       -       |      |
;;;;                                                                      +-------o-------+      |
;;;;                                                                              |              |
;;;;                                                                              |              |
;;;;                                                                         +----v------+       |
;;;;                                                                         |   abs     |       |
;;;;                                                                         +-----+-----+       |
;;;;                                                                               |             |
;;;;                                                                               |             |
;;;;                                                                             --v-------------v--
;;;;                                                                            (         <         )
;;;;                                                                             -------------------
;;;;
;;;;





;;;;    EXPANDED DATAPATH
;;;;
;;;;    o-------o
;;;;     \ 1.0 /
;;;;      \   /
;;;;       \ /
;;;;        V       +----------------------------------------------+
;;;;        |       |                                              |
;;;;        X g<-1  X g<-i                                         |
;;;;        |       |                                              |
;;;;     +--v-------v--+                                           |
;;;;     |    guess    |                                           |
;;;;     +------+------+                                           |
;;;;            |                                                  |
;;;;            +---+----------------------------------+----+      |
;;;;            |   |                                  |    |      |
;;;;         +--v---v--+  +-----+                      |    |      |
;;;;         |    *    |  |  x  |                      |    |      |
;;;;         +----+----+  +--+--+                      |    |      |
;;;;              |          +--------------------+    |    |      |
;;;;              |          |                    |    |    |      |
;;;;           +--v----------v-+                  |    |    |      |
;;;;           |       -       |               +--v----v--+ |      |
;;;;           +-------o-------+  o-------o    |     /    | |      |
;;;;                   |           \0.001/     +-----+----+ |      |
;;;;              +----v------+     \   /            |      |      |
;;;;              |   abs     |      \ /        +----v------v----+ |
;;;;              +-----+-----+       V         |    average     | |
;;;;                    |             |         +-------+--------+ |
;;;;                  --v-------------v--               |          |
;;;;                 (         <         )              +----------+
;;;;                  -------------------
;;;;
;;;;
;;;;
;;;;
;;;;
