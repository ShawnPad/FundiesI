;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname duks) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define WIDTH 500)
(define BG (empty-scene WIDTH WIDTH))
(define SPEED 10)
(define CENTER (make-posn (/ WIDTH 2) (/ WIDTH 2)))
(define DUCK (circle 10 'solid 'yellow))
(define POOL (circle 250 'solid 'blue))

; go-with-the-flow : Posn -> Posn
; produces a new posn that is the next point on a
; roundabout path to the CENTER
(define (go-with-the-flow p)
  (if (and (= (posn-x p) (posn-x CENTER))
           (= (posn-y p) (posn-y CENTER)))
      CENTER
      (make-posn (+ (posn-x p) (* SPEED (cos (find-angle (- (posn-x p) (posn-x CENTER))
                                                         (- (posn-y p) (posn-y CENTER))))))
                 (+ (posn-y p) (* SPEED (sin (find-angle (- (posn-x p) (posn-x CENTER))
                                                         (- (posn-y p) (posn-y CENTER)))))))))
 
; find-angle : Number Number -> Number
; Find the angle to travel at when moving from one position to another
(check-within (find-angle 50 2) 2.3962 0.001)
(check-within (find-angle 1 100) -2.3662 0.001)
(define (find-angle dx dy)
  (atan (- dx dy) (- 0 dx dy)))
(define (tick lod)
  (filter not-center? (map go-with-the-flow lod)))
(define (not-center? d)
  (not (and (= (posn-x d) (posn-x CENTER)) (= (posn-y d) (posn-y CENTER)))))
(define (draw lod)
  (foldr placeduks BG lod))
(define (placeduks d img)
  (place-image DUCK (posn-x d) (posn-y d) img))
(define (makeduk n)
  (make-posn (random WIDTH) (random WIDTH)))
(define INT-DUCKS (build-list 10 makeduk))
(define (main w)
  (big-bang w
    [on-tick tick]
    [to-draw draw]))
(main INT-DUCKS)