#lang racket

;; dumb and simple paragraph typesetting

(require slideshow/pict)

(provide paragraph)

;; TODO: doesn't error gracefully when given width is too
;; short to accommodate the words

;; string -> listof string
(define (string-split str) (regexp-match* #px"\\S+" str))

;; string exact-positive-integer? -> pict
(define (paragraph str width)
  
  ;; listof pict -> pict, listof pict
  (define (construct-line picts)
    (let loop ([line (blank 0 0)] [picts picts])
      (cond [(null? picts) (values line picts)]
            [else
             (define new-line (hbl-append line space (car picts)))
             (if (> (pict-width new-line) width)
                 (values line picts)
                 (loop new-line (cdr picts)))])))
  
  (define space (text " "))
  (define words (string-split str))
  (define word-picts (map (λ (w) (text w)) words))
  
  ;; keep constructing lines until all words are consumed
  (define lines
    (let loop ([word-picts word-picts] [par '()])
      (cond [(null? word-picts) par]
            [else
             (define-values (line rst) (construct-line word-picts))
             (loop rst (cons line par))])))
  
  (apply vl-append (reverse lines)))