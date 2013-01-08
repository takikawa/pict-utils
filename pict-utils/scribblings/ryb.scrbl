#lang scribble/manual

@(require "common.rkt")

@(define the-eval (make-base-eval))
@(the-eval '(require pict-utils/ryb slideshow/pict plot))

@title{RYB Color Model}

@defproc[(make-color/ryb [red (real-in 0 1)]
                         [yellow (real-in 0 1)]
                         [blue (real-in 0 1)])
         color%]{

Produces a @racket[color%] object given a coordinate in the
red-yellow-blue (RYB) color space. The RYB color model is
a subtractive model used for painting and art education.
This function produces an approximation in the RGB color
model.

@examples[#:eval the-eval
  (colorize (disk 50) (make-color/ryb 0 1 1))
  (colorize (filled-rectangle 40 40) (make-color/ryb 1 0.5 1))
]}

@defproc[(ryb-series [red-1 (real-in 0 1)]
                     [yellow-1 (real-in 0 1)]
                     [blue-1 (real-in 0 1)]
                     [red-2 (real-in 0 1)]
                     [yellow-2 (real-in 0 1)]
                     [blue-3 (real-in 0 1)]
                     [#:midpoints midpoints 1])
         (listof color%)]{

Produces a list of @racket[color%] objects that represent
averages (or mixtures) of the two input colors in the RYB
model.

The @racket[midpoints] argument controls how many recursive
averagings to perform. A @racket[midpoints] of 1 produces
three colors, 2 produces five, and so on.

@examples[#:eval the-eval
(map (λ (c) (colorize (filled-rounded-rectangle 50 50) c))
     (ryb-series 0.2 0.1 0.3 0 0 0.8
                 #:midpoints 2))
(map (λ (c) (colorize (filled-rounded-rectangle 50 50) c))
     (ryb-series 0 1 0 0 0 0.8
                 #:midpoints 3))
(plot (contour-intervals (λ (x y) (+ x y)) -2 2 -2 2
                         #:levels 4 #:contour-styles '(transparent)
                         #:colors (ryb-series 0 0.3 0.3 0.5 0.3 0.2
                                              #:midpoints 2)))
]}
