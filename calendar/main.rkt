#lang racket

(require slideshow/pict)

;; Zeller's congruence
(define (zeller day month year)
  (define Y (if (<= month 2) (- year 1) year))
  (define m (if (<= month 2) (+ 12 month) month))
  (modulo
   (+ day
      (floor (/ (* 26 (+ m 1)) 10))
      Y
      (floor (/ Y 4))
      (* 6 (floor (/ Y 100)))
      (floor (/ Y 400)))
   7))

;; Alternative starting on Sunday
(define (zeller* day month year)
  (modulo (- (zeller day month year) 1) 7))

(module+ test
  (require rackunit)
  (check-equal? (zeller 01 01 2012) 1)
  (check-equal? (zeller 01 02 2012) 4)
  (check-equal? (zeller 04 05 2011) 4)
  (check-equal? (zeller 03 12 1988) 0)
  (check-equal? (zeller 24 03 2012) 0)
  (check-equal? (zeller 25 03 2012) 1))

(define days-in-a-month
  #hash((01 . 31) (02 . 28) (03 . 31)
        (04 . 30) (05 . 31) (06 . 30)
        (07 . 31) (08 . 31) (09 . 30)
        (10 . 31) (11 . 30) (12 . 31)))

;; calendar : (integer-in 1 12) integer
;;            #:draw-day (-> (U #f (integer-in 1 31)) pict?)
;;            -> pict?
(define (calendar month year
                  #:draw-day [draw-day draw-day])
  
  ;; draw-week : (integer-in 1 31) #:offset (integer-in -6 6) -> pict?
  ;; creates a pict for a week of the month
  (define (draw-week start-day #:offset [offset 0])
    (if (>= offset 0) 
        (apply hc-append
               (append (build-list offset (λ (n) (draw-day #f)))
                       (map (λ (n) (draw-day n))
                            (range start-day (+ start-day (- 7 offset))))))
        (apply hc-append
               (append (map (λ (n) (draw-day n))
                            (range start-day (+ start-day (+ 7 offset))))
                       (build-list (- offset) (λ (n) (draw-day #f)))))))
  
  ;; optional header
  (define header
    (apply hc-append
           (map (λ (str) (text str))
                (list "M" "T" "W" "Tr" "F" "S" "Su"))))

  ;; some numbers we need
  (define start-day (zeller* 1 month year))
  (define month-length (dict-ref days-in-a-month month))
  (define last-offset (modulo (- 7 (modulo (+ month-length start-day) 7)) 7))
  (define num-weeks (quotient (+ month-length start-day last-offset) 7))
  (printf "day: ~a length: ~a offset: ~a weeks: ~a ~n" start-day month-length last-offset num-weeks)
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
                 (list (draw-week (- month-length (- 6 last-offset))
                                  #:offset (- last-offset))))))

;; base day
(define empty (blank 20 30))

;; draw-day : (integer-in 1 31) U #f -> pict?
;; produces a pict for the given day
(define (draw-day d)
  (if d
      (rc-superimpose (text (number->string d)) empty)
      empty))

(module+ test
  (apply ht-append
         (for/list ([n (range 1 13)]
                    [month-name (list "January" "February" "March"
                                      "April" "May" "June"
                                      "July" "August" "September"
                                      "October" "November" "December")])
           (vc-append (text month-name) (calendar n 2012)))))