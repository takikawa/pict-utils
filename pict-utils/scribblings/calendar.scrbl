#lang scribble/manual

@(require "common.rkt")

@(define the-eval (make-base-eval))
@(the-eval '(require pict-utils/calendar slideshow/pict
                     racket/format))

@title{Calendar picts}

@defproc[(calendar [month (integer-in 1 12)]
                   [year exact-nonnegative-integer?]
                   [#:draw-day draw-day (-> (option/c (integer-in 1 31)) pict?)])
          pict?]{

Produces a @tech{pict} depicting the given @racket[month] of the Gregorian
calendar for the given @racket[year].

If @racket[draw-day] is provided, it is called with either an integer representing
the day of the month or @racket[#f] for an empty calendar slot. For each day position
on the calendar, the result of @racket[draw-day] is shown in the resulting pict.

@examples[#:eval the-eval
  (calendar 12 1988)
  (calendar 3 2013
   #:draw-day (λ (day)
                (if day
                    (cc-superimpose
                     (hc-append (blank 3) (standard-fish 30 30) (blank 3))
                     (colorize (text (~a day)) "white"))
                    (blank 36 30))))
]}
