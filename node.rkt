#lang racket

(require slideshow/pict
         "pict.rkt"
         (for-syntax racket syntax/parse))

(provide nodes make-style
         align
         (rename-out [make-coord coord]))

;;; Data definitions

;; a Location is one of
;;  - Symbol
;;  - (align Symbol Symbol)
;;  - (coord Number Number [Symbol])
(struct align (name align))
(struct coord (x y align))

(define (make-coord x y [align 'cc])
  (coord x y align))

;; a Style is a (style Maybe<String> String Maybe<String>)
(struct style (color text-color background-color))

(define (make-style #:color [color #f]
                    #:text-color [text-color "black"]
                    #:background-color [background-color #f])
  (style color text-color background-color))

;; a Node is a (node Location 
;;                   Maybe<Symbol> Maybe<Pict>
;;                   Maybe<String> Maybe<Style>)
(struct node (loc name pict text style))

(define (make-node #:at [loc (coord 0 0 'cc)]
                   #:name [name #f]
                   #:pict [pict #f]
                   #:style [style #f]
                   #:text [text #f])
  (node loc name pict text style))

;;; Contracts
(define align/c (one-of/c 'lt 'ct 'rt 'lc 'cc 'rc 'lb 'cb 'rb))

(define location/c (or/c symbol? (struct/c align symbol? symbol?) coord?))

(define style/c
  (struct/c style (or/c #f string?) (or/c #f string?) (or/c #f string?)))

(define node/c
  (struct/c node location/c 
            (or/c #f symbol?) (or/c #f pict?)
            (or/c #f string?) (or/c #f style/c)))

;;; Macros
(define-syntax (nodes stx)
  
  (define-splicing-syntax-class node-subclause
    (pattern (~seq (~and #:at kw) (~and loc-expr val)))
    (pattern (~seq (~and #:name kw) (~and name-expr val)))
    (pattern (~seq (~and #:pict kw) (~and pict-expr val)))
    (pattern (~seq (~and #:style kw) (~and style-expr val)))
    (pattern (~seq (~and #:text kw) (~and text-expr val))))
  
  (define-syntax-class node-clause
    (pattern ((~datum node) sub:node-subclause ...)
             #:with node
             #`(make-node #,@(flatten (map list ; interleave kws and vals
                                           (syntax->list #'(sub.kw ...))
                                           (syntax->list #'(sub.val ...)))))))
  
  (syntax-parse stx
    [(_ n:node-clause ...)
     #'(draw-nodes (list n.node ...))]))

;; listof<Node> -> Pict
(define/contract (draw-nodes nodes)
  (-> (listof node/c) pict?)
  
  ;; build a dict mapping node names to coordinates
  (define name-mapping
    (for/hash ([n nodes]
               #:when (node-name n))
      (when (not (coord? (node-loc n)))
        (error "Node can't have a named location while being named~n"))
      (values (node-name n) (node-loc n))))
  
  ;; -> coord?
  ;; given a node, extract its coordinates (or consult name table)
  (define (get-coord n)
    (define loc (node-loc n))
    (match loc
      [(? symbol?) (dict-ref name-mapping loc)]
      [(struct align ((and (? symbol?) name) (and (? symbol?) align)))
       (define c (dict-ref name-mapping name))
       (coord (coord-x c) (coord-y c) align)]
      [else loc]))
  
  ;; to build a pict for each node
  (define (draw-one n)
    (define sty (or (node-style n) (style #f #f #f)))
    ;; get the base pict for this node, or start with blank
    (define base-node-pict (or (node-pict n) (blank 0 0)))
    ;; build a text pict
    (define text-pict (text (or (node-text n) "")))
    ;; color the text if necessary
    (define colored-text
      (if (style-text-color sty)
          (colorize text-pict (style-text-color sty))
          text-pict))
    ;; stick the text (if present) on the base pict
    (define pict-with-text
      (cond [(and colored-text base-node-pict)
             (cc-superimpose colored-text base-node-pict)]
            [colored-text colored-text]
            [else base-node-pict]))
    ;; if a background color is needed, add a backdrop
    (define pict-with-backdrop
      (if (style-background-color sty)
          (backdrop pict-with-text #:color (style-background-color sty))
          pict-with-text))
    pict-with-backdrop)
  
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
      (define p (draw-one n))
      (define c (get-coord n))
      (define x (coord-x c))
      (define y (coord-y c))
      (define-values (dx dy) (pict-offsets p (coord-align c)))
      (values (max x-pos-max (+ x dx))
              (min x-neg-max (- x dx))
              (max y-pos-max (+ y dy))
              (min y-neg-max (- y dy)))))
  
  (define-values (w h) (values (+ xp (- xn)) (+ yp (- yn))))
  
  ;; gets the translated coords for each node
  (define (draw-coords n p)
    (define c (get-coord n))
    (define x (coord-x c))
    (define y (coord-y c))
    (define-values (dx dy) (pict-offsets p (coord-align c)))
    (values (if (> x 0)
                (- (+ (- xn) x) dx)
                (- (- (- xn) (- x)) dx))
            (if (> y 0)
                (- (- yp y) dy)
                (- (+ yp (- y)) dy))))
  
  ;; then draw on a blank pict of the right size
  (for/fold ([p (blank w h)])
            ([n nodes])
    (define pict-for-node (draw-one n))
    (define-values (x y) (draw-coords n pict-for-node))
    (if pict-for-node
        (pin-over p x y pict-for-node)
        p)))