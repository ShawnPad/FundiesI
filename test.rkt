;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname test) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define BG (empty-scene 1000 500))
(define-struct point (x y))
(define W 10)
(define BOI (square  W "solid" "blue"))
(define POINT (make-point  0 0))
(define (draw p)
  (place-image BOI (point-x p) (point-y p) BG))
(define (key-check p event)
  (move p event))
(define (move p event)
    (cond [(and (<= (point-x p) (- 1000 W)) (string=? event "right")) (make-point (+ 10 (point-x p)) (point-y p))]
        [(and (>= (point-x p) W) (string=? event "left")) (make-point (- (point-x p) 10) (point-y p))]
        [(and (<= (point-y p) (- 500 W)) (string=? event "down")) (make-point  (point-x p) (+ 10 (point-y p)))]
        [(and (>= (point-y p) W) (string=? event "up")) (make-point (point-x p) (- (point-y p) 10))]
        [else p]))
  
(big-bang POINT
  [to-draw draw]
  [on-key key-check])