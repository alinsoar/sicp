
;; x windows mesh: matrix 4 X 4 - filled on (1 1) (2 2) (3 4) (4 3)

;;; first add the directory of this file in preferences/folders/script-fu-folders
;;; instead of opening the console:
;; gimp -b '(script-fu-refresh 1)' -b '(x-windows-mesh "t.png" 100 100)'

(define big-x-windows-mesh-pattern
  (let ((data '((0 . 0) (1 . 1) (2 . 2) (3 . 3) (7 . 4) (6 . 5) (5 . 6) (4 . 7))))
    (lambda (m)
      (case m
        ('x-width 8)
        ('y-width 8)
        ('iter-data (lambda (proc) (for-each proc
                                        (map car data)
                                        (map cdr data))))))))

(define small-x-windows-mesh-pattern
  (let ((data '((0 . 0) (1 . 1) (2 . 2) (2 . 3) (3 . 2))))
    (lambda (m)
      (case m
        ('x-width 4)
        ('y-width 4)
        ('iter-data (lambda (proc)
                      (for-each proc
                                (map car data)
                                (map cdr data))))))))

(define pattern.x-width
  (lambda (p)
    (p 'x-width)))
(define pattern.y-width
  (lambda (p)
    (p 'x-width)))
(define pattern.iter-data
  (lambda (p proc)
    ((p 'iter-data) proc)))

(define make-pattern-image
  (lambda (pattern layer)
    (lambda (orig-x orig-y)
      (pattern.iter-data pattern
                         (lambda (x y)
                           (gimp-pencil layer 2 (vector (+ orig-x x) (+ orig-y y))))))))

(define fill-pattern
  (lambda (layer img fill-x fill-y pattern)

    (define pattern-x-size (pattern.x-width pattern))
    (define pattern-y-size (pattern.y-width pattern))
    
    (define copy
      (lambda (x y)
        (gimp-image-select-rectangle img CHANNEL-OP-REPLACE 0 0 x y)
        (gimp-edit-copy layer)
        (gimp-selection-none img)))

    (define (paste xoff yoff)
      (let ((floating/selection (car (gimp-edit-paste layer 0))))
        (gimp-layer-set-offsets floating/selection xoff yoff)
        (gimp-floating-sel-anchor floating/selection)))

    ((make-pattern-image pattern layer) 0 0)
    
    (gimp-layer-set-offsets layer 0 0)
    
    ((lambda (s) (s s pattern-x-size))
     (lambda (s x)
       (if (< x fill-x)
           (begin
             (copy x pattern-y-size)
             (paste x 0)
             (s s (+ x x))))))

    ((lambda (s) (s s pattern-y-size))
     (lambda (s y)
       (if (< y fill-y)
           (begin
             (copy fill-x y)
             (paste 0 y)
             (s s (+ y y))))))

    (gimp-selection-none img)))

(define x-windows-mesh
  (lambda (filename x-size y-size)
    (define img (car (gimp-image-new x-size y-size RGB)))
    (define layer (car (gimp-layer-new img x-size y-size RGB-IMAGE "layer" 0 LAYER-MODE-NORMAL-LEGACY)))
    (define pattern big-x-windows-mesh-pattern)

    (gimp-image-insert-layer img layer 0 0)
    
    (gimp-context-set-background '(000 000 000))
    (gimp-context-set-foreground '(255 255 255))
    (gimp-drawable-fill layer BACKGROUND-FILL)
    
    (fill-pattern layer img x-size y-size pattern)

    (gimp-file-save RUN-NONINTERACTIVE img layer filename "")

    ))

(script-fu-register
 "x-windows-mesh"                ; func name
 "new console"                   ; menu label
 "repl"                          ; description
 "alin soare"                    ; author
 "gnu"                           ; copyright notice
 "July 19, 2019"                 ; date created
 "GRAY"                          ; image type that the script works on
 SF-STRING "Text" "output file"  ; first parameter
 SF-VALUE "x" "dimension on X axis"     ; second parameter
 SF-VALUE "y" "dimension on Y axis"     ; third parameter
 )

