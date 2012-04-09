#lang racket

(require rackunit)

;; Zeller's congruence
(define (zeller day month year)
  (define K (modulo year 100))
  (define J (floor (/ year 100)))
  (modulo
   (+ day
      (floor (/ (* 13 (+ month 1)) 5))
      K
      (floor (/ K 4))
      (floor (/ J 4))
      (- (* 2 J)))
   7))

;; Alternative starting on Sunday
(define (zeller* day month year)
  (modulo (- (zeller day month year) 1) 7))

(module+ tests
  (check-equal? (zeller 03 12 1988) 0)
  (check-equal? (zeller 24 03 2012) 0)
  (check-equal? (zeller 25 03 2012) 1))

(define days-in-a-month
  #hash((01 . 31) (02 . 28) (03 . 31)
        (04 . 30) (05 . 31) (06 . 30)
        (07 . 31) (08 . 31) (09 . 30)
        (10 . 31) (11 . 30) (12 . 31)))

(require slideshow/pict)

(define (calendar month year)
  ;; for drawing the calendar
  (define header
    (apply hc-append
           (map (λ (str) (text str))
                (list "M" "T" "W" "Tr" "F" "S" "Su"))))

  ;; some numbers we need
  (define start-day (zeller* 1 month year))
  (define month-length (dict-ref days-in-a-month month))
  (define num-weeks (quotient (+ month-length start-day) 7))
  (define last-offset (modulo (+ month-length start-day) 7))
  ;; produce the pict
  (define-values (weeks _)
    (for/fold ([weeks '()]
               [day (+ 2 (- 6 start-day))])
      ([n (- num-weeks 2)])
      (values (append weeks (list (draw-week day)))
              (+ day 7))))
  (apply vc-append
         (append (list (draw-week 1 #:offset start-day))
                 weeks
                 (list (draw-week (- month-length (- last-offset 1))
                                  #:offset (- last-offset))))))

;; base day
(define empty (blank 20 30))

;; draw-day : (integer-in 1 31) -> pict?
;; produces a pict for the given day
(define (draw-day n)
  (rc-superimpose (text (number->string n)) empty))

;; draw-week : (integer-in 1 31) #:offset (integer-in -6 6) -> pict?
;; creates a pict for a week of the month
(define (draw-week start-day #:offset [offset 0])
  (if (>= offset 0) 
      (apply hc-append
             (append (build-list offset (λ (n) empty))
                     (map (λ (n) (draw-day n))
                          (range start-day (+ start-day (- 7 offset))))))
      (apply hc-append
             (append (map (λ (n) (draw-day n))
                          (range start-day (+ start-day (- offset))))
                     (build-list (- 7 (- offset)) (λ (n) empty))))))