;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Lab5) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define BW 1000)
(define BH 500)
(define BG (empty-scene BW BH "transparent"))
(define-struct player (pos vel acl color))
(define-struct game (ps mouse))
(define STARTL (cons (make-player (make-posn (/ BW 2) (/ BH 2)) (make-posn 1 1) (make-posn 0 0) 0) (cons (make-player (make-posn (/ BW 2) (/ BH 2)) (make-posn 1 1) (make-posn 0 0) 1) (cons (make-player (make-posn (/ BW 2) (/ BH 2)) (make-posn 1 1) (make-posn 0 0) 2) '()))))
(define GAME (make-game STARTL (make-posn 0 0)))
(define (towards vec1 vec2)
  (normalizev (make-posn (- (posn-x vec2) (posn-x vec1)) (- (posn-y vec2) (posn-y vec1)))))
(define (scalev vec s)
  (make-posn (* (posn-x vec) s) (* (posn-y vec) s)))
(define (choose-color n)
  (cond [(= n 0) "red"]
        [(= n 1) "orange"]
        [(= n 2) "darkolivegreen"]
        [(= n 3) "green"]
        [(= n 4) "blue"]
        [(= n 5) "purple"]
        [else "pink"]))
(define (normalizev vec)
  (make-posn (/ (posn-x vec) (magnitudev vec)) (/ (posn-y vec) (magnitudev vec))))
(define (magnitudev vec)
  (sqrt (+ (* (posn-x vec) (posn-x vec)) (* (posn-y vec) (posn-y vec)))))
(define (add vec2 vec1)
  (make-posn (+ (posn-x vec2) (posn-x vec1)) (+ (posn-y vec2) (posn-y vec1))))
(define (maxv vec m)
  (cond [(> (magnitudev vec) m) (scalev vec (/ m (magnitudev vec)))]
        [else vec]))
(define (tunpack g)
  (make-game (ticks (game-ps g) (game-mouse g)) (game-mouse g)))
(define (ticks g pos)
  (cond [(empty? g) '()]
        [else (cons (tick (first g) (posn-x pos) (posn-y pos)) (ticks (rest g) pos))]))
(define (tick p x y)
  (cond [(or (< (posn-y (player-pos p)) BH) (< (posn-y (player-vel p)) 0))  (make-player (add (player-pos p) (add (player-vel p) (player-acl p))) (add (player-vel p) (player-acl p)) (make-posn 0 1) (player-color p))]
        [(> (posn-y (player-vel p)) 0) (make-player (add (player-pos p) (add (player-vel p) (player-acl p))) (scalev (player-vel p) -1) (player-acl p) (player-color p))]
        [else p]))
(define (physs g x y e)
  (make-game (game-ps g) (make-posn x y)))
(define (dunpack g)
  (draws (game-ps g)))
(define (draws g)
  (cond [(empty? g) BG]
        [else (overlay (draw (first g)) (draws (rest g)))]))
(define (draw p)
  (place-image (circle 10 "solid" (choose-color (player-color p))) (posn-x (player-pos p)) (posn-y (player-pos p)) BG))
(define (aunpack g key)
  (make-game (addmore (game-ps g) key) (game-mouse g)))
(define (addmore g key)
  (cond [(string=? key "a") (cons (make-player (make-posn (random BW) 0) (make-posn 1 1) (make-posn 0 0) (random 5)) g)]
        [else g]))
(big-bang GAME
  [to-draw dunpack]
  [on-mouse physs]
  [on-key aunpack]
  [on-tick tunpack])
