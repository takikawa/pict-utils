#lang racket

(require slideshow/pict
         (except-in unstable/gui/ppict grid))

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
;; (path (move-to 0 0)
;;       (arc 30 30 0 (degrees->radians 30))
;;       (close))
(define-syntax (path stx)
  (define-syntax-class path-elem
    (pattern ...))
  
  (syntax-parse stx
    [(_ elem:path-elem ...)
     #'(let ()
         (define p (new dc-path%))
         (elem.expr p) ...)]))

(ppict-do (colorize (grid 300 300 50 1) "gray")
          #:go (coord 0.5 0.5)
          (hline 300 1)
          #:go (coord 0.5 0.5)
          (vline 1 300)
          #:go (coord 0.5 0.5)
          (circle 200)
          #:go (coord 0.5 0.5 'lc)
          (arc 30 30 0 (degrees->radians 30)))