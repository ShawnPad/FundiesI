;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Assignment1-ShawnPadilla) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;ex2
;int->int
;given a number, the number is squared then 5 is added
(define (sq5 num)
  (+ (* num num) 5))
;ex3
;int->int
;given t func returns value of t term in arithmatic sequence
(define (func t) 
  (+ 2 (- (* t 3) 3)))
;ex4
;int->int
;given x returns y value of given parabola
(define (func2 x)
  (- (* x x) 3))

;check-epects
;ex2
(check-expect (sq5 2) 9)
(check-expect (sq5 3) 14)
;ex3
(check-expect (func 1) 2)
(check-expect (func 2) 5)
(check-expect (func 9) 26)
;ex4
;x= 0 1 2 3
;y= -3 -2 1 6
(check-expect (func2 0) -3)
(check-expect (func2 1) -2)
(check-expect (func2 2) 1)