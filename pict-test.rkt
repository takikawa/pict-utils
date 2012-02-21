#lang racket

(require slideshow/pict
         (except-in unstable/gui/ppict grid)
         "pict.rkt"
         "node.rkt")

(define main-diagram
  (ppict-do (colorize (grid 300 300 50 1) "gray")
            #:go (coord 0.5 0.5)
            (hline 300 1)
            #:go (coord 0.5 0.5)
            (vline 1 300)
            #:go (coord 0.5 0.5)
            (circle 200)
            #:go (coord 0.5 0.5 'lt)
            (colorize
             (path (move-to 0 0)
                   (line-to 30 0)
                   (arc -30 -30 60 60 0 (degrees->radians 30))
                   (close))
             "lightgreen")
            #:go (coord 1/2 1/2
                        #:abs-x (* 100 (cos (degrees->radians 30)))
                        'cb)
            (colorize (vline 5 50) "red")
            #:go (coord 1/2 1/2 'lc)
            (colorize (hline (* 100 (cos (degrees->radians 30))) 1)
                      "blue")
            #:go (coord 1/2 1/2
                        #:abs-x 100
                        'cb)
            (colorize (vline 1 (* 100 (tan (degrees->radians 30))))
                      "orange")
            #:go (coord 1/2 1/2 'lt)
            (path (move-to 0 0)
                  (line-to 100 (* -100 (tan (degrees->radians 30)))))
            ;; draw trig text
            #:go (coord 1/2 1/2 #:abs-x 50 #:abs-y 5 'ct)
            (backdrop (colorize (text "cos α") "blue"))
            #:go (coord 1/2 1/2 #:abs-x 65 #:abs-y -20)
            (backdrop (colorize (text "sin α") "red"))
            ;; draw labels
            #:go (coord 1/2 1/2 #:abs-x -5 #:abs-y -100 'rc)
            (backdrop (text "1"))
            #:go (coord 1/2 1/2 #:abs-x -5 #:abs-y 100 'rc)
            (backdrop (text "-1"))
            #:go (coord 1/2 1/2 #:abs-x 100 #:abs-y 5 'ct)
            (backdrop (text "1"))
            #:go (coord 1/2 1/2 #:abs-x -100 #:abs-y 5 'ct)
            (backdrop (text "-1"))
            #:go (coord 1/2 1/2 #:abs-x -5 #:abs-y 50 'rc)
            (backdrop (text "-½"))
            #:go (coord 1/2 1/2 #:abs-x -5 #:abs-y -50 'rc)
            (backdrop (text "½"))
            #:go (coord 1/2 1/2 #:abs-x -50 #:abs-y 5 'ct)
            (backdrop (text "-½"))))