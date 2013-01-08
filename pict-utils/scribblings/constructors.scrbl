#lang scribble/manual

@(require "common.rkt")

@(define the-eval (make-base-eval))
@(the-eval '(require pict-utils/constructors slideshow/pict))

@title{Extra pict constructors}

@defproc[(envelope [width real?] [height real?]
                   [#:border-width border-width real? 1]
                   [#:color color string? "beige"]
                   [#:border-color border-color string? "medium goldenrod"]
                   [#:seal-color seal-color string? "red"])
         pict?]{

Produces a @tech{pict} of an envelope. The envelope is drawn with
the given dimensions and colors.

@examples[#:eval the-eval
  (envelope 150 50)
  (envelope 150 50 #:seal-color "blue")
]}

@defproc[(grid [width real?] [height real?] [step real?]
               [line-width real? 1])
         pict?]{

Produces a @tech{pict} of a grid with the given dimensions.

@examples[#:eval the-eval
  (grid 300 300 50)
]}

@defproc[(arc [width real?] [height real?]
              [start-radians real?] [end-radians real?])
         pict?]{

Produces a pict of an arc with the given dimensions. The
arguments are the same as for the @racket[draw-arc] method.
}

