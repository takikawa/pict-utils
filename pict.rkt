#lang racket

(require slideshow/pict
         racket/draw
         (except-in unstable/gui/ppict grid)
         (for-syntax syntax/parse))

;; grid : nat nat nat [nat] -> pict
;; draw a grid
(define (grid width height step [line-width 1])
  (define vlines
    (apply hc-append
           (cons step
                 (build-list (sub1 (floor (/ width step)))
                             (lambda (_)
                               (vline line-width height))))))
  (define hlines
    (apply vc-append
           (cons step
                 (build-list (sub1 (floor (/ width step)))
                             (lambda (_)
                               (hline width line-width))))))
  vlines
  (cc-superimpose vlines hlines))

;; arc : nat nat real real -> pict
;; draw an arc pict
(define (arc width height start-radians end-radians)
  (dc (λ (dc x y)
        (send dc draw-arc
              x y
              width height
              start-radians end-radians))
      width
      height))

;; degrees->radians : real -> real
(define (degrees->radians r)
  (* r (/ (* 2 pi) 360)))

;; path
;; pict macro for drawing paths
;; e.g.
;; (path 100 200
;;       (move-to 0 0)
;;       (arc 30 30 0 (degrees->radians 30))
;;       (close))
(define-syntax (path stx)
  (define-syntax-class path-elem
    (pattern ((~literal move-to) x y)
             #:with expr #'(λ (p) (send p move-to x y)))
    (pattern ((~literal line-to) x y)
             #:with expr #'(λ (p) (send p line-to x y)))
    (pattern ((~literal arc) x y w h sr er)
             #:with expr #'(λ (p) (send p arc x y w h sr er)))
    (pattern ((~literal close))
             #:with expr #'(λ (p) (send p close))))
  
  (syntax-parse stx
    [(_ elem:path-elem ...)
     #'(let ()
         (define p (new dc-path%))
         (elem.expr p) ...
         (define-values (x y w h)
           (send p get-bounding-box))
         (dc (λ (dc x y)
             (send dc draw-path p x y))
           w h))]))

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
           "green")
          #:go (coord 1/2 1/2
                      #:abs-x (* 100 (cos (degrees->radians 30)))
                      'cb)
          (colorize (vline 5 50) "red")
          #:go (coord 1/2 1/2 'lc)
          (colorize (hline (* 100 (cos (degrees->radians 30))) 1)
                    "blue"))