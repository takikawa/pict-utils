#lang at-exp racket

(require slideshow/pict
         "../paragraph.rkt"
         "../pict.rkt"
         "../npict.rkt")

(define axis-label
  (style #:background-color "white"))
(define sin-style
  (style #:text-color "red"
         #:background-color "white"))
(define cos-style
  (style #:text-color "blue"
         #:background-color "white"))

(define main-diagram
  (npict (node #:name 'origin #:at (coord 0 0))
         ;; grid and axes
         (node #:at 'origin #:pict (colorize (grid 300 300 50 1) "gray"))
         (node #:at 'origin #:pict (hline 300 1))
         (node #:at 'origin #:pict (vline 1 300))
         ;; unit circle
         (node #:at 'origin #:pict (circle 200))
         ;; the interesting stuff
         (node #:at (align 'origin 'lb)
               #:pict
               (colorize
                (path (move-to 0 0)
                      (line-to 30 0)
                      (arc -30 -30 60 60 0 (degrees->radians 30))
                      (close))
                "lightgreen"))
         (node #:at (coord (* 100 (cos (degrees->radians 30))) 0 'cb)
               #:pict (colorize (vline 5 50) "red"))
         (node #:at (coord 0 0 'lc)
               #:pict (colorize (hline (* 100 (cos (degrees->radians 30))) 1)
                                "blue"))
         (node #:at (coord 100 0 'cb)
               #:pict
               (colorize (vline 1 (* 100 (tan (degrees->radians 30))))
                         "orange"))
         (node #:at (align 'origin 'lb)
               #:pict (path (move-to 0 0)
                            (line-to 100 (* -100 (tan (degrees->radians 30))))))
         ;; draw trig text
         (node #:at (coord 50 -10) #:style cos-style #:text "cos α")
         (node #:at (coord 65 20) #:style sin-style #:text "sin α")
         ;; draw labels
         (node #:at (coord -10 -100) #:style axis-label #:text "-1")
         (node #:at (coord -10 100)  #:style axis-label #:text "1")
         (node #:at (coord 100 -10)  #:style axis-label #:text "1")
         (node #:at (coord -100 -10) #:style axis-label #:text "-1")
         (node #:at (coord -10 -50)  #:style axis-label #:text "-½")
         (node #:at (coord -10 50)   #:style axis-label #:text "½")
         (node #:at (coord -50 -10)  #:style axis-label #:text "-½")))

;; you can also compose picts made with `nodes`
(define composed-diagram
  (npict
   (node #:at (coord 250 200) #:pict
         (backdrop
          (paragraph @string-append{The angle α is 30 degrees in
                                    the example. The sine of α,
                                    which is the height of the
                                    red line, is 1/2.}
                     100)))
   (node #:at (coord 0 200) #:pict main-diagram)))

;; this pict should not be drawn out of bounds
(define coord-test
  (npict
   (node #:at (coord 0 15 'cb)  #:pict (rectangle 20 20))
   (node #:at (coord 0 -15 'ct) #:pict (rectangle 20 20))))

;; testing lines
(define line-test
  (npict
   ;; two boxes, arrow line from left to right
   (node #:name 'n1 #:at (coord -20 0)
         #:pict (rectangle 20 20))
   (node #:name 'n2 #:at (coord 20 0)
         #:pict (rectangle 20 20))
   (line #:arrow? #t #:from 'n1 #:to 'n2)
   ;; two boxes, non-arrow line between
   (node #:name 'n3 #:at (coord -20 -25)
         #:pict (rectangle 20 20))
   (node #:name 'n4 #:at (coord 20 -25)
         #:pict (rectangle 20 20))
   (line #:from 'n4 #:to 'n3)))
