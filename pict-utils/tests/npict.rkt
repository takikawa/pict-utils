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

;; same with this one
(define coord-test-2
  (npict
   (node #:at (coord 15 0 'lc)  #:pict (rectangle 20 20))
   (node #:at (coord -15 0 'rc) #:pict (rectangle 20 20))))

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

;; advanced line tests with curves
(define line-test-2
  (npict
   ;; two boxes, curved line from left to right (dashed)
   (node #:name 'n1 #:at (coord -20 0)
         #:pict (rectangle 20 20))
   (node #:name 'n2 #:at (coord 20 0)
         #:pict (rectangle 20 20))
   (line #:arrow? #t #:from 'n1 #:to 'n2
         #:start-angle (/ pi 3)
         #:end-angle (- (/ pi 3))
         #:style 'short-dash)
   ;; two boxes, curved line mirror image (translucent)
   (node #:name 'n3 #:at (coord -20 -25)
         #:pict (rectangle 20 20))
   (node #:name 'n4 #:at (coord 20 -25)
         #:pict (rectangle 20 20))
   (line #:arrow? #t #:from 'n3 #:to 'n4
         #:start-angle (- (/ pi 3))
         #:end-angle (/ pi 3)
         #:style 'hilite)))

;; allow both strings and symbols as names
;; should show a diagonally placed pair of a
;; red box and black box with a line between
(define name-test
  (npict
   (node #:name "origin" #:at (coord 0 0))
   (node #:name 'n1 #:at (coord 50 50)
         #:pict (rectangle 20 20))
   (node #:at "origin"
         #:pict (colorize (rectangle 20 20) "red"))
   (line #:from 'n1 #:to "origin")))

;; tests alignment specifications
(define align-test
  (let ([c (colorize (rectangle 20 20) "red")]
        [l (colorize (rectangle 20 20) "green")]
        [r (colorize (rectangle 20 20) "blue")]
        [hl (colorize (hline 180 1) "lightgray")]
        [vl (colorize (vline 1 180) "lightgray")])
    (npict
     ;; grid lines
     (node #:at (coord 0 -30 'cb) #:pict vl)
     (node #:at (coord 60 -30 'cb) #:pict vl)
     (node #:at (coord 120 -30 'cb) #:pict vl)
     (node #:at (coord -30 0 'lc) #:pict hl)
     (node #:at (coord -30 60 'lc) #:pict hl)
     (node #:at (coord -30 120 'lc) #:pict hl)
     ;; boxes at all alignments
     (node #:at (coord 0 0 'cc) #:pict c #:text "cc")
     (node #:at (coord 60 0 'ct) #:pict c #:text "ct")
     (node #:at (coord 120 0 'cb) #:pict c #:text "cb")
     (node #:at (coord 0 60 'lc) #:pict l #:text "lc")
     (node #:at (coord 60 60 'lt) #:pict l #:text "lt")
     (node #:at (coord 120 60 'lb) #:pict l #:text "lb")
     (node #:at (coord 0 120 'rc) #:pict r #:text "rc")
     (node #:at (coord 60 120 'rt) #:pict r #:text "rt")
     (node #:at (coord 120 120 'rb) #:pict r #:text "rb"))))