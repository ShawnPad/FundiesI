;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Assignment2-ShawnPadilla) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;ex4
;int-int->int
;speed is a positive integer
;roadspeed is a positive integer
;amount-of-ticket takes speed of the car and max roadspeed and outputs the ticket fee
;percent excess | ticket fee
; < 10% | 50
; < 25% | 150
; < 100% | 400
; beyond | 2500
(define (amount-of-ticket speed roadspeed)
  (cond [(> (- (* (/ speed roadspeed) 100) 100) 100) 2500]
        [(> (- (* (/ speed roadspeed) 100) 100) 25) 400]
        [(> (- (* (/ speed roadspeed) 100) 100) 10) 150]
        [(> (- (* (/ speed roadspeed) 100) 100) 0) 50]
        [else 0]))
(check-expect (amount-of-ticket 201 100) 2500)
(check-expect (amount-of-ticket 200 100) 400)
(check-expect (amount-of-ticket 125 100) 150)
(check-expect (amount-of-ticket 110 100) 50)
(check-expect (amount-of-ticket 100 100) 0)

;ex5
(define RADIUS 50)
(define WIDTH 1000)
(define HEIGHT 500)
(define BG (empty-scene WIDTH HEIGHT))
(define SUN (circle RADIUS "solid" "orange"))
;world->image
;takes worldstate and image of sun is placed at worldstate y pos and middle of screen x
;worldstate is an integer
(define (draw world)
  (place-image SUN (/ WIDTH 2) world BG))
(check-expect (draw 0) (place-image SUN (/ WIDTH 2) 0 BG))
;world->world
;iterates worldstate a set amount depending on current worldstate
;world < height/2 fast
;world < height/4 slow
;world < height super slow
;world = height stop
(define (tick world)
  (cond [(< world (/ HEIGHT 2)) (+ 10 world)]
        [(< world (* 3 (/ HEIGHT 4))) (+ 5 world)]
        [(< world HEIGHT) (+ 1 world)]
        [else world]))
(check-expect (tick 0) 10)
(check-expect (tick (/ HEIGHT 2)) (+ 5 (/ HEIGHT 2)))
(check-expect (tick (* 3 (/ HEIGHT 4))) (+ 1 (* 3 (/ HEIGHT 4))))
(check-expect (tick HEIGHT) HEIGHT)
(big-bang 0
  [to-draw draw]
  [on-tick tick])

;ex6
(define-struct time (hours minutes))
(define START (make-time 0 0))
;time->time
;given a time the time is iterated one minute
;time is a structure containing ints hr and min where hr is 0 to 11 and min is 0 to 59
;minutes > 60 hours +1 then minutes returns to 0
;hours = 12 then hours returns to 0
(define (tock t)
  (cond [(= (time-hours t) 12) (make-time 0 0)]
        [(< (time-minutes t) 59) (make-time (time-hours t) (+ 1 (time-minutes t)))]
        [else (make-time (+ 1 (time-hours t)) 0)]
        ))
(check-expect (tock (make-time 0 0)) (make-time 0 1))
(check-expect (tock (make-time 0 59)) (make-time 1 0))
(check-expect (tock (make-time 12 59)) (make-time 0 0))
;time->text
;given a time the time is turned into text formated "hr:min" placed at center of screen
(define (time->text t)
  (place-image (text (get-time t) 36 "black") 500 250 BG))
(check-expect (time->text (make-time 10 10)) (place-image (text "10:10" 36 "black") 500 250 BG))
;time->string
;given a time a string of the time is given as "hr:min"
(define (get-time t)
  (string-append (single->double (time-hours t)) ":" (single->double (time-minutes t))))
(check-expect (get-time (make-time 10 10)) "10:10")
;just for fun
;int->string
;given a number returns a string in the form numnum where numbers less than ten return a zero in front
;ex: 1->01 and 10->10
(define (single->double num)
  (cond [(< num 10) (string-append "0" (number->string num))]
        [else (number->string num)]))
(check-expect (single->double 10) "10")
(check-expect (single->double 9) "09")
(define (main t)
  (big-bang t
    [to-draw time->text]
    [on-tick tock]))
(main START)