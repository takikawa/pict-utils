#lang racket

;; HSV color space operations

(require racket/draw)

(provide
 (contract-out [make-color/hsv
                (-> real? (real-in 0 1) (real-in 0 1) (is-a?/c color%))]))

;; make a color object from HSV values
(define (make-color/hsv hue saturation value)
  (define chroma (* saturation value))
  (define hue* (/ (remainder* hue (* 2 pi)) (/ pi 3)))
  (define X (* chroma (- 1 (abs (- (remainder* hue* 2) 1)))))
  (define-values (r1 g1 b1)
    (cond [(and (<= 0 hue*) (< hue* 1)) (values chroma X 0)]
          [(and (<= 1 hue*) (< hue* 2)) (values X chroma 0)]
          [(and (<= 2 hue*) (< hue* 3)) (values 0 chroma X)]
          [(and (<= 3 hue*) (< hue* 4)) (values 0 X chroma)]
          [(and (<= 4 hue*) (< hue* 5)) (values X 0 chroma)]
          [(and (<= 5 hue*) (< hue* 6)) (values chroma 0 X)]))
  (define m (- value chroma))
  (apply make-color (map (λ (x) (exact-round (* 255 (+ x m))))
                         (list r1 g1 b1))))

;; general remainder
(define (remainder* n1 n2)
  (define num-divides (/ n1 n2))
  (- n1 (* (floor num-divides) n2)))

(module+ samples
  (require slideshow/pict)
  (colorize (disk 50) (make-color/hsv (/ pi 3) 1 0.75))
  (colorize (disk 50) (make-color/hsv (/ pi 1.5) 1 0.50))
  (colorize (disk 50) (make-color/hsv pi 1 1))
  (map (λ (angle)
         (colorize (disk 50) (make-color/hsv angle 1 0.75)))
       (range 0 (* 2 pi) (/ pi 10))))