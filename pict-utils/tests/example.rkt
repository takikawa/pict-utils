#lang racket

(require slideshow/pict
         pict-utils/constructors
         pict-utils/npict)

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
         (let ([x (* 100 (cos (degrees->radians 30)))])
           (line #:from (coord x 0 'cb) #:to (coord x 50 'cb)
                 #:color "red"))
         (let ([x (* 100 (cos (degrees->radians 30)))])
           (line #:from (coord 0 0 'lc) #:to (coord x 0 'lc)
                 #:color "blue"))
         (let ([y (* 100 (tan (degrees->radians 30)))])
           (line #:from (coord 100 0 'cb) #:to (coord 100 y 'cb)
                 #:color "orange"))
         (line #:from (align 'origin 'cc)
               #:to (coord 100 (* 100 (tan (degrees->radians 30)))))
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

main-diagram