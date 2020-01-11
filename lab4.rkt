;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define-struct game [current up?])
; A CircleGame is a (make-game NestedRings Boolean)
; - where current is the current set of nested rings
; - and up? is #true if we are adding rings and #false if not
#;(define (game-temp g)
    (...(game-current g) (game-up? g)...))

;(define-struct nested [outer inner])
; A NestedRings is one of:
; - CircInfo
;   -> representing a single circle
; - (make-nested CircInfo NestedRings)
;    -> representing an outer ring and some inner rings/circles
#;(define (nested-temp n)
    (cond [(CircInfo? n) ...]
          [else (nested-temp (nested-inner n)) ... (nested-outer n)]))



; choose-color : Nat -> Color
; Choose the color corresponding with the given number
(check-expect (choose-color 0) "red")
(check-expect (choose-color 1) "orange")
(check-expect (choose-color 2) "yellow")
(check-expect (choose-color 3) "green")
(check-expect (choose-color 4) "blue")
(check-expect (choose-color 5) "purple")
(check-expect (choose-color 6) "pink")
(define BG (empty-scene 500 500))
(define (choose-color n)
  (cond [(= n 0) "red"]
        [(= n 1) "orange"]
        [(= n 2) "yellow"]
        [(= n 3) "green"]
        [(= n 4) "blue"]
        [(= n 5) "purple"]
        [else "pink"]))
(define (draw g)
  (overlay (nest (game-current g)) BG))

(define-struct nested [outer inner])
; - (make-nested CircInfo NestedRings)
(define-struct CircInfo (radius color))

(define circ0 (make-CircInfo 10 3))
(define circ1 (make-nested (make-CircInfo (+ 9 (CircInfo-radius circ0)) 4) circ0))
;(define circ2 (make-nested (make-CircInfo (+ 9 (CircInfo-radius circ1)) (random 5)) circ1))
;game->img
(check-expect (nest circ1) (overlay (makecirc circ0) (makecirc (make-CircInfo (+ 9 10) 4))))
(define (nest n)
  (cond [(CircInfo? n) (makecirc circ0)]
        [else (overlay (nest (nested-inner n)) (makecirc (nested-outer n)))]))
;circinfo -> img
(define (makecirc c)
  (circle (CircInfo-radius c) 'solid (choose-color (CircInfo-color c))))
(define START (make-game circ0 #true))
;game->game
(define (tick g)
  (cond
    [(CircInfo? (game-current g)) (make-game (make-nested (make-CircInfo (+ 5 (CircInfo-radius (game-current g))) (random 5)) (game-current g)) (not (game-up? g)))]
    [(game-up? g) (make-game (make-nested (make-CircInfo (+ 5 (CircInfo-radius (nested-outer (game-current g)))) (random 5)) (game-current g)) (< (CircInfo-radius (nested-outer (game-current g))) 230))]
    [else (make-game (nested-inner (game-current g)) #false)]))
(big-bang START
  [to-draw draw]
  [on-tick tick 0.1])