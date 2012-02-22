#lang racket

(require slideshow/pict
         "pict.rkt"
         "node.rkt")

(define axis-label
  (make-style #:background-color "white"))
(define sin-style
  (make-style #:text-color "red"
              #:background-color "white"))
(define cos-style
  (make-style #:text-color "blue"
              #:background-color "white"))

(define main-diagram
  (nodes (node #:at (coord 0 0)
               #:pict (colorize (grid 300 300 50 1) "gray"))
         (node #:at (coord 0 0)
               #:pict (hline 300 1))
         (node #:at (coord 0 0)
               #:pict (vline 1 300))
         (node #:at (coord 0 0)
               #:pict (circle 200))
         (node #:at (coord 0 0 'lb)
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
         (node #:at (coord 0 0 'lb)
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