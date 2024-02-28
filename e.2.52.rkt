#lang racket

(require racket/gui)

(require (submod "e.2.50.rkt" export))
(require (submod "e.2.47.rkt" export))
(require (submod "e.2.46.rkt" export))

(define (copy-painter painter)
  (let ((width (send painter get-width))
        (height (send painter get-height)))
    (let ((drawing-context (new bitmap-dc% [bitmap (make-bitmap width height)])))
      (send drawing-context draw-bitmap painter 0 0)
      (send drawing-context get-bitmap))))

(define (set-coordinates drawing-context frame scale-x scale-y)
  "should be used only by picture->painter"
  (send drawing-context set-transformation
        (vector
         ;; logical unit-vectors and logical origin
         (vector (xcor-vect (edge1-frame frame))  ; x-unit
                 (ycor-vect (edge1-frame frame))
                 (xcor-vect (edge2-frame frame))  ; y-unit
                 (ycor-vect (edge2-frame frame))
                 (xcor-vect (origin-frame frame)) ; origin
                 (ycor-vect (origin-frame frame)))
         0 0               ; physical device origin
         scale-x scale-y   ; scaling x and y axis on physical device
         0                 ; rotation on physical device.
         )))

(define (over painter1 painter2)
  (lambda (frame)
    (let ((drawing-context (new bitmap-dc% [bitmap (copy-painter painter1)])))
      (let ((width0 (send painter1 get-width))
            (height0 (send painter1 get-height))
            (width (send painter2 get-width))
            (height (send painter2 get-height)))
        (let ((new-origin (make-vect (* width0 (xcor-vect (origin-frame frame)))
                                     (* height0 (ycor-vect (origin-frame frame))))))
          (let ((e1 (make-vect (xcor-vect (edge1-frame frame))
                               (ycor-vect (edge1-frame frame))))
                (e2 (make-vect (xcor-vect (edge2-frame frame))
                               (ycor-vect (edge2-frame frame)))))
            (let ((f0 (make-frame new-origin e1 e2)))
              (set-coordinates drawing-context f0 1 1))))
        (send drawing-context draw-bitmap painter2 0 0)
        (send (send drawing-context get-bitmap) save-file "pict.png" 'png)
        (send drawing-context get-bitmap)))))

(define (file->painter filename)
  (lambda (frame)
    (let ((o (make-object bitmap% filename)))
      ((over (make-bitmap (send o get-width)
                             (send o get-height))
                o)
       frame))))

(define rogers (file->painter "william-barton-rogers.gif"))

(define identity-frame (make-frame (make-vect 0 0)
                                   (make-vect 1 0)
                                   (make-vect 0 1)))

(module+ test

  
  (define f0 (make-frame (make-vect 0.2 0.2) ; frame origin
                        (make-vect .5 .1)   ; 0x-axis unit vector
                        (make-vect .1 .5)))

  ;; 0y-axis unit vector

  
  ((rotate90 rogers) f0)

  (rogers identity-frame)

  
  
  ((over (rogers identity-frame)
         ((over (rogers identity-frame)
                ((over (rogers identity-frame)
                       ((over (rogers identity-frame)
                              (rogers identity-frame))
                        f0))
                 f0))
          f0))
   f0)

  ((transform-painter (over (rogers identity-frame) (rogers f0))
                      (origin-frame f0)
                      (edge1-frame f0)
                      (edge2-frame f0))
   identity-frame)

  (define (make-logo painter)
    (lambda (frame)
      (define blue-brush (new brush% [color "blue"]))
      (define red-brush (new brush% [color "red"]))
      (define left-lambda-path
        (let ([p (new dc-path%)])
          (send p move-to 153 44)
          (send p line-to 161.5 60)
          (send p curve-to 202.5 49 230 42 245 61)
          (send p curve-to 280.06 105.41 287.5 141 296.5 186)
          (send p curve-to 301.12 209.08 299.11 223.38 293.96 244)
          (send p curve-to 281.34 294.54 259.18 331.61 233.5 375)
          (send p curve-to 198.21 434.63 164.68 505.6 125.5 564)
          (send p line-to 135 572)
          p))

      (define bottom-lambda-path
        (let ([p (new dc-path%)])
          (send p move-to 135 572)
          (send p line-to 188.5 564)
          (send p curve-to 208.5 517 230.91 465.21 251 420)
          (send p curve-to 267 384 278.5 348 296.5 312)
          (send p curve-to 301.01 302.98 318 258 329 274)
          (send p curve-to 338.89 288.39 351 314 358 332)
          (send p curve-to 377.28 381.58 395.57 429.61 414 477)
          (send p curve-to 428 513 436.5 540 449.5 573)
          (send p line-to 465 580)
          (send p line-to 529 545)
          p))

      (define right-lambda-path
        (let ([p (new dc-path%)])
          (send p move-to 153 44)
          (send p curve-to 192.21 30.69 233.21 14.23 275 20)
          (send p curve-to 328.6 27.4 350.23 103.08 364 151)
          (send p curve-to 378.75 202.32 400.5 244 418 294)
          (send p curve-to 446.56 375.6 494.5 456 530.5 537)
          (send p line-to 529 545)
          p))

      (define lambda-path
        (let ([p (new dc-path%)])
          (send p append left-lambda-path)
          (send p append bottom-lambda-path)
          (let ([t (new dc-path%)])
            (send t append right-lambda-path)
            (send t reverse)
            (send p append t))
          (send p close)
          p))

      (define dc (new bitmap-dc% [bitmap (make-bitmap 500 500)]))

      (send dc set-font (make-font #:size 50 #:family 'symbol #:weight 'light
                                   #:smoothing 'smoothed))
      (send dc set-text-foreground "red")
      (send dc draw-text "(Y F) = (F (Y F))" 30 50)

      (send dc translate 0 200)
      (send dc scale 0.4 0.4)
      (send dc set-pen "yellow" 0 'transparent)
      (send dc set-brush "blue" 'solid)
      (send dc draw-path lambda-path)

      ((over painter (send dc get-bitmap))
       frame)
      ))

  (define f (make-frame (make-vect .5 .3)
                        (make-vect .4 0)
                        (make-vect 0 .4)))

  (define f2 (make-frame (make-vect .31 .28)
                         (make-vect .9 0)
                         (make-vect 0 .9)))

  (define l (file->painter "lambda-t.png"))
  (define m (file->painter "mit-logo.jpg"))

  (define mit-scheme-logo
    ((over (m identity-frame)
           ((over (l identity-frame)
                  ((over (l identity-frame)
                         ((over (l identity-frame)
                                ((over (l identity-frame)
                                       ((over (l identity-frame)
                                              (l identity-frame))
                                        f))
                                 f))
                          f))
                   f))
            f))
     f2))

  (define (logo painter n)
    (lambda (frame)
      (define (iter n co)
        (if (zero? n)
            (co (painter identity-frame))
            (iter (sub1 n)
                  (lambda (w)
                    (co ((over (painter identity-frame)
                               w)
                         frame))))))
      (iter n (lambda (x) x))))

  (define f4 (make-frame
              (make-vect .4 .3)
              (make-vect .6 0)
              (make-vect 0 .6)))

  (define scheme-logo (logo (make-logo (make-bitmap 500 500))
                            3))

  (scheme-logo f4)

  ((rotate90 (logo (make-logo (make-bitmap 500 500))
                   10))
   f4)

  ((squash-inwards (logo (make-logo (make-bitmap 500 500)) 10)) f4)

  )

