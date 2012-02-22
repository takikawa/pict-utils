#lang racket

(require slideshow/pict
         (for-syntax syntax/parse))

(provide nodes)

;; a Style is a (style String ...) [to be expanded]
(struct style (color))

(define (make-style #:color [color #f])
  (style color))

;; a Node is a (node Coord Coord Pict String Style)
(struct node (x y pos pict text style))

(define (make-node x y pos
                   #:pict [pict #f]
                   #:style [style #f]
                   #:text [text #f])
  (node x y pos pict text style))

(define-syntax (nodes stx)
  
  (define-splicing-syntax-class node-subclause
    (pattern (~seq (~and #:pict kw) (~and pict-expr val)))
    (pattern (~seq (~and #:style kw) (~and style-expr val)))
    (pattern (~seq (~and #:text kw) (~and text-expr val))))
  
  (define-syntax-class node-clause
    (pattern ((~datum node) #:at c:coord sub:node-subclause ...)
             #:with node #'(keyword-apply make-node
                                          '(sub.kw ...)
                                          (list sub.val ...)
                                          (list c.e1 c.e2 c.pos))))
  
  (define-syntax-class coord
    (pattern ((~datum coord) e1 e2 (~optional pos #:defaults ([pos #''cc])))))
  
  (syntax-parse stx
    [(_ n:node-clause ...)
     #'(draw-nodes (list n.node ...))]))

;; listof<Node> -> Pict
(define (draw-nodes nodes)
  
  ;; to build a pict for each node
  (define (draw-one n)
    (define base-node-pict (node-pict n))
    (cond [(and (node-text n) base-node-pict)
           (cc-superimpose (text (node-text n)) base-node-pict)]
          [(node-text n) (text (node-text n))]
          [else base-node-pict]))
  
  ;; pict-offsets : pict? pos? -> (values int? int?)
  ;; find the offsets used to draw or size the scene
  (define (pict-offsets p pos)
    (define w (pict-width p))
    (define h (pict-height p))
    (case pos
      [(lt) (values 0 0)]
      [(ct) (values (/ w 2) 0)]
      [(rt) (values w 0)]
      [(lc) (values 0 (/ h 2))]
      [(cc) (values (/ w 2) (/ h 2))]
      [(rc) (values w (/ h 2))]
      [(lb) (values 0 h)]
      [(cb) (values (/ w 2) h)]
      [(rb) (values w h)]))
  
  ;; first figure out the size of the base pict
  (define-values (xp xn yp yn)
    (for/fold ([x-pos-max 0] [x-neg-max 0]
               [y-pos-max 0] [y-neg-max 0])
              ([n nodes])
      ;; hack
      (define p (or (draw-one n) (blank 0 0)))
      (define-values (dx dy) (pict-offsets p (node-pos n)))
      (values (max x-pos-max (+ (node-x n) dx))
              (min x-neg-max (- (node-x n) dx))
              (max y-pos-max (+ (node-y n) dy))
              (min y-neg-max (- (node-y n) dy)))))
  
  (define-values (w h) (values (+ xp (- xn)) (+ yp (- yn))))
  
  ;; gets the translated coords for each node
  (define (draw-coords n p)
    (define-values (dx dy) (pict-offsets p (node-pos n)))
    (values (if (> (node-x n) 0)
                (- (+ (- xn) (node-x n)) dx)
                (- (- (- xn) (- (node-x n))) dx))
            (if (> (node-y n) 0)
                (- (- yp (node-y n)) dy)
                (- (+ yp (- (node-y n))) dy))))
  
  ;; then draw on a blank pict of the right size
  (for/fold ([p (blank w h)])
            ([n nodes])
    (define pict-for-node (draw-one n))
    (define-values (x y) (draw-coords n pict-for-node))
    (if pict-for-node
        (pin-over p x y pict-for-node)
        p)))