;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment6) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)
 
; An Invader is a Posn
; INTERP: represents the location of the invader
 
; A Bullet is a Posn
; INTERP: represents the location of a bullet
 
; A Location is a Posn
; INTERP: represents a location of a spaceship
 
; A Direction is one of:
; - "left"
; - "right"
; INTERP: represent direction of movement for the spaceship
 
(define-struct ship (dir loc))
; A Ship is (make-ship Direction Location)
; INTERP: represent the spaceship with its current direction
;         and movement
 
; A List of Invaders (LoI) is one of
; - '()
; - (cons Invader LoI)
 
; A List of Bullets (LoB) is one of
; - '()
; - (cons Bullet LoB)
 
(define-struct world (ship invaders ship-bullets invader-bullets))
; A World is (make-world Ship LoI LoB LoB)
; INTERP: represent the ship, the current list of invaders, the inflight spaceship bullets
;         and the inflight invader bullets
 
 
(define WIDTH 500)
(define HEIGHT 500)
 
(define MAX-SHIP-BULLETS 3)
 
(define MAX-INVADER-BULLETS 15)
 
(define BACKGROUND (empty-scene WIDTH HEIGHT))
 
(define SPACESHIP-BULLET-IMAGE (circle 2 'solid 'black))
 
(define SHIP-WIDTH 25)
 
(define SHIP-HEIGHT 15)
 
(define SPACESHIP-IMAGE (rectangle SHIP-WIDTH SHIP-HEIGHT 'solid 'black))
 
(define INVADER-SIDE 20)
 
(define INVADER-IMAGE (square INVADER-SIDE 'solid 'red))
 
(define INVADER-BULLET-IMAGE (circle 2 'solid 'red))
 
(define SHIP-SPEED 10)
 
(define BULLET-SPEED 10)
 
(define SHIP-INIT (make-ship 'left (make-posn 250 480)))
 
(define INVADERS-INIT
        (list (make-posn 100 20) (make-posn 140 20) (make-posn 180 20)
              (make-posn 220 20) (make-posn 260 20) (make-posn 300 20)
              (make-posn 340 20) (make-posn 380 20) (make-posn 420 20)
              (make-posn 100 50) (make-posn 140 50) (make-posn 180 50)
              (make-posn 220 50) (make-posn 260 50) (make-posn 300 50)
              (make-posn 340 50) (make-posn 380 50) (make-posn 420 50)
              (make-posn 100 80) (make-posn 140 80) (make-posn 180 80)
              (make-posn 220 80) (make-posn 260 80) (make-posn 300 80)
              (make-posn 340 80) (make-posn 380 80) (make-posn 420 80)
              (make-posn 100 110) (make-posn 140 110) (make-posn 180 110)
              (make-posn 220 110) (make-posn 260 110) (make-posn 300 110)
              (make-posn 340 110) (make-posn 380 110) (make-posn 420 110)))
 
(define WORLD-INIT (make-world SHIP-INIT INVADERS-INIT empty empty))