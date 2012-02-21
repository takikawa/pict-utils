#lang racket

(require slideshow/pict
         (for-syntax syntax/parse))

(provide nodes)

;; a Node is a (node Coord Coord Pict String Style)
(struct node (x y pict text style))

(define (build-node x y
                    #:pict [pict #f]
                    #:style [style #f]
                    #:text [text #f])
  (node x y pict text style))

(define-syntax (nodes stx)
  
  (define-splicing-syntax-class node-subclause
    (pattern (~seq #:pict pict-expr)
             #:with kw #'#:pict
             #:with val #'pict-expr)
    (pattern (~seq #:style style-expr)
             #:with kw #'#:style
             #:with val #'style-expr)
    (pattern (~seq #:text text-expr)
             #:with kw #'#:text
             #:with val #'text-expr))
  
  (define-syntax-class node-clause
    (pattern ((~datum node) #:at ((~literal coord) e1 e2)
                            sub:node-subclause ...)
             #:with node #'(keyword-apply build-node
                                          '(sub.kw ...)
                                          (list sub.val ...)
                                          (list e1 e2))))
  
  (syntax-parse stx
    [(_ n:node-clause ...)
     #'(draw-nodes (list n.node ...))]))

;; listof<Node> -> Pict
(define (draw-nodes nodes)
  (define (draw-one n)
    (define base-node-pict (node-pict n))
    (cond [(and (node-text n) base-node-pict)
           (cc-superimpose (text (node-text n)) base-node-pict)]
          [(node-text n) (text (node-text n))]
          [else base-node-pict]))
  ;; first figure out the size of the base pict
  (define-values (w h)
    (for/fold ([x-max 0] [y-max 0])
              ([n nodes])
      ;; hack
      (define p (or (draw-one n) (blank 0 0)))
      (values (max x-max (+ (node-x n) (pict-width p)))
              (max y-max (+ (node-y n) (pict-height p))))))
  (for/fold ([p (blank w h)])
            ([n nodes])
    (define pict-for-node (draw-one n))
    (if pict-for-node
        (pin-over p (node-x n) (node-y n) pict-for-node)
        p)))