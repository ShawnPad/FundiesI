;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname class1) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define SEG-RAD 5)
(define GRID-W (* SEG-RAD 2))
(define BW (* 30 GRID-W))
(define BH (* 30 GRID-W))
(define BG (empty-scene BW BH "transparent"))
(define FOOD (circle SEG-RAD 'solid 'green))
(define SEG (circle SEG-RAD 'solid 'red))
(define GO (place-image (text "Game Over Lol" 36 'black) (/ BW 2) (/ BH 2) BG))
;constant: H,W,BG,SC,FC,R,SS,SC,FC,GO
;change: SP,SV,SL,FP

;A Snake is a (make-snake Direction LoP)
;A Direction is one of: "up" "down" "left" or "right"

;A LoP is one of:
;-'()
;-(cons posn LoP)

;Food is a posn

;A Game is a (make-world Snake Food)

(define-struct snake (dir segs))
(define-struct world (snake food))

(define FOOD0 (make-posn 10 7))
(define SNAKE0 (make-snake "up" (cons (make-posn 3 18) (cons (make-posn 3 19) (cons (make-posn 3 20) '())))))
(define WORLD0 (make-world SNAKE0 FOOD0))

#;(define (snake-temp s)
    (dir-temp (snake-dir s)) ... (lop-temp (snake-segs s)))
#;(define (dir-temp d)
    (cond [(string=? d "up") ...]
          [(string=? d "down") ...]
          [(string=? d "left") ...]
          [(string=? d "right") ...]))
#;(define (lop-temp alop)
    (cond [(empty? alop) ...]
          [(cons? alop) ... (posn-temp (first alop))
                        ... (lop-temp (rest alop))]))
#;(define (posn-temp p)
    ... (posn-x p) ... (posn-y p))
#;(define (world-temp w)
    ... (snake-temp (world-snake w)) ... (posn-temp (world-food w)))

;World->Image
;draws the snake and good onto the scene
(check-expect (draw-world WORLD0)
              (place-image FOOD-IMG (* 10 GRID-W) (* 7 GRID-W)
                           (place-image SEG-IMG (* 3 GRID-W) (* 18 GRID-W)
                                        (place-image SEG-IMG (* 3 GRID-W) (* 19 GRID-W)
                                                     (place-image SEG-IMG (* 3 GRID-W) (* 20 GRID-W) BG)))))
(define (draw-world w)
  (draw-seg (world-food w) FOOD-IMG (draw-snake (world-snake w))))
(define (draw-snake alop)
  (cond [(empty? alop) BG]
        [(cons? alop) (draw-seg (first alop) SEG-IMG) (draw-snake (rest alop))]))
(define (draw-seg p img1 img)
  (place-image img1 (* (posn-x p) GRID-WIDTH) (* (posn-y p) GRID-WIDTH) img))
;World->World
;move the snake in its direction, grows if eating
(define (move-world w)
  (if (posn=? (first (snake-segs (world-snake w))) (world-food))
      (grow-snake w)
      (move-snake w)))
(define (move-snake w)
  ())
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2)) (= (posn-y p1) (posn-y p2))))
;World KeyEvent -> World
(define (change-dir w key)
  (if (or (string=? key "up") (string=? key "down") (string=? key "left") (string=? key "right"))
      (make-world (make-snake key (snake-segs (world-snake w))) (world-food w))
      w))
;World -> Boolean
(define (collision? w)
  #false)
;World -> Image
(define (show-end w)
  BG)

;to-draw
;on-tick, moves snake, grow, eat
;on-key, change dir
;stop-when
;world->world
;launches snake game
(define (main w)
  (big-bang w 
    [to-draw draw-world]
    [on-key change-dir]
    [on-tick move-world]
    [stop-when collision? show-end]))