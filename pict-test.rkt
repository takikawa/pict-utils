#lang racket

(require slideshow/pict
         (except-in unstable/gui/ppict grid)
         "pict.rkt"
         "node.rkt")

(define main-diagram
  (nodes (node #:at (coord 0 0)
               #:pict (colorize (grid 300 300 50 1) "gray"))
         (node #:at (coord 0 0)
               #:pict (hline 300 1))
         (node #:at (coord 0 0)
               #:pict (vline 1 300))
         (node #:at (coord 0 0)
               #:pict (circle 200))
         (node #:at (coord 0 0)
               #:pict
               (colorize
                (path (move-to 0 0)
                      (line-to 30 0)
                      (arc -30 -30 60 60 0 (degrees->radians 30))
                      (close))
                "lightgreen"))
         (node #:at (coord (* 100 (cos (degrees->radians 30))) 25)
               #:pict (colorize (vline 5 50) "red"))
         (node #:at (coord 0 0)
               #:pict (colorize (hline (* 100 (cos (degrees->radians 30))) 1)
                                "blue"))
         (node #:at (coord 100 0)
               #:pict
               (colorize (vline 1 (* 100 (tan (degrees->radians 30))))
                         "orange"))
         (node #:at (coord 0 0)
               #:pict (path (move-to 0 0)
                            (line-to 100 (* -100 (tan (degrees->radians 30))))))
         ;; draw trig text
         (node #:at (coord 50 5)
               #:pict
               (backdrop (colorize (text "cos α") "blue")))
         (node #:at (coord 65 -20)
               #:pict
               (backdrop (colorize (text "sin α") "red")))
         ;; draw labels
         (node #:at (coord -5 -100)
               #:pict (backdrop (text "1")))
         (node #:at (coord -5 100)
               #:pict (backdrop (text "-1")))
         (node #:at (coord 100 5)
               #:pict (backdrop (text "1")))
         (node #:at (coord -100 5)
               #:pict (backdrop (text "-1")))
         (node #:at (coord -5 50)
               #:pict (backdrop (text "-½")))
         (node #:at (coord -5 -50)
               #:pict (backdrop (text "½")))
         (node #:at (coord -50 5)
               #:pict (backdrop (text "-½")))))