#lang racket

;; RYB colorspace
;;
;; Idea and original code from:
;;   Nathan Gossett and Baoquan Chen,
;;   "Paint Inspired Color Mixing and Compositing for Visualization"
;;   IEEE InfoVis 2004, Austin, Texas, Oct 10-12, 2004
;;   http://threekings.tk/mirror/ryb_TR.pdf

(require racket/draw)

(provide make-color/ryb
         ryb-series)

;; make a RYB color series from the first color
;; to the second
(define (ryb-series red-1 yellow-1 blue-1
                    red-2 yellow-2 blue-2
                    #:midpoints [midpoints 1])
  (define (average c1 c2)
    (/ (+ c1 c2) 2))
  (define-values (red-mid yellow-mid blue-mid)
    (values (average red-1 red-2)
            (average yellow-1 yellow-2)
            (average blue-1 blue-2)))
  (if (= 1 midpoints)
      (list (make-color/ryb red-1 yellow-1 blue-1)
            (make-color/ryb red-mid yellow-mid blue-mid)
            (make-color/ryb red-2 yellow-2 blue-2))
      (append (ryb-series red-1 yellow-1 blue-1
                          red-mid yellow-mid blue-mid
                          #:midpoints (- midpoints 1))
              ;; avoid duplicating the middle
              (rest (ryb-series red-mid yellow-mid blue-mid
                                red-2 yellow-2 blue-2
                                #:midpoints (- midpoints 1))))))

;; make a color% object, given RYB coordinates
(define (make-color/ryb red-in yellow-in blue-in)
  ;; red
  (define red-out
    (let* ([x0 (cubic-interpolate blue-in 1.0 0.163)]
           [x1 (cubic-interpolate blue-in 1.0 0.0)]
           [x2 (cubic-interpolate blue-in 1.0 0.5)]
           [x3 (cubic-interpolate blue-in 1.0 0.2)]
           [y0 (cubic-interpolate yellow-in x0 x1)]
           [y1 (cubic-interpolate yellow-in x2 x3)])
      (cubic-interpolate red-in y0 y1)))
  ;; green
  (define green-out
    (let* ([x0 (cubic-interpolate blue-in 1.0 0.373)]
           [x1 (cubic-interpolate blue-in 1.0 0.66)]
           [x2 (cubic-interpolate blue-in 0.0 0.0)]
           [x3 (cubic-interpolate blue-in 0.5 0.094)]
           [y0 (cubic-interpolate yellow-in x0 x1)]
           [y1 (cubic-interpolate yellow-in x2 x3)])
      (cubic-interpolate red-in y0 y1)))
  ;; blue
  (define blue-out
    (let* ([x0 (cubic-interpolate blue-in 1.0 0.6)]
           [x1 (cubic-interpolate blue-in 0.0 0.2)]
           [x2 (cubic-interpolate blue-in 0.0 0.5)]
           [x3 (cubic-interpolate blue-in 0.0 0.0)]
           [y0 (cubic-interpolate yellow-in x0 x1)]
           [y1 (cubic-interpolate yellow-in x2 x3)])
      (cubic-interpolate red-in y0 y1)))
  (apply make-object color%
         (map (λ (c) (exact-round (* 255 c)))
              (list red-out green-out blue-out))))

;; t : interpolation factor
;; a, b : values to interpolate
(define (cubic-interpolate t a b)
  (define weight (* t t(- 3 (* 2 t))))
  (+ a (* weight (- b a))))

(module+ samples
  (require slideshow/pict
           plot)
  (map (λ (c) (colorize (filled-rounded-rectangle 50 50) c))
       (ryb-series 0.2 0.1 0.3 0 0 0.8
                   #:midpoints 2))
  (map (λ (c) (colorize (filled-rounded-rectangle 50 50) c))
       (ryb-series 0 1 0 0 0 0.8
                   #:midpoints 3))
  (plot (contour-intervals (λ (x y) (+ x y)) -2 2 -2 2
                           #:levels 4 #:contour-styles '(transparent)
                           #:colors (ryb-series 0 0.3 0.3 0.5 0.3 0.2
                                                #:midpoints 2))))
