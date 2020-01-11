;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Assignment4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Shape is one of: 
; - Circle 
; - Square
; - Rectangle
 
(define-struct circl (x y r outline c))
; A Circle is a (make-circl Number Number Number Boolean Symbol)
; interpretation: x and y determine the center of the circle,
;   r the radius, outline whether it's outlined or solid,
;   and c its color

(define-struct squar (x y size outline c))
; A Square is a (make-squar Number Number Number Boolean Symbol)
; interpretation: Supply a good interpretation of Square.
 
(define-struct recta (x y width height outline c))
; A Rectangle is a (make-recta Number Number Number Number Boolean Symbol)
; interpretation: Supply a good interpretation of Rectangle.

;Shape -> ?
(define (shape-template s)
  (cond [(circl? s) (circl-x s) (circl-y s) (circl-r s) (circl-outline s) (circl-c s) ...]
        [(squar? s) (squar-x s) (squar-y s) (squar-size s) (squar-outline s) (squar-c s) ...]
        [else (recta-x s) (recta-y s) (recta-r s) (recta-outline s) (recta-c s) ...]))

;Shape -> Shape
(define (shape-shift-x s delta)
  (cond [(circl? s) (make-circl (+ (circl-x s) delta) (circl-y s) (circl-r s) (circl-outline s) (circl-c s))]
        [(squar? s) (make-squar (+ (squar-x s) delta) (squar-y s) (squar-size s) (squar-outline s) (squar-c s))]
        [else (make-recta (+ (recta-x s) delta) (recta-y s) (recta-r s) (recta-outline s) (recta-c s))]))