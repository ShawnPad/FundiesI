;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname stuff) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
;[X -> Boolean] [List-of X] -> [List-of X]
;get all items in the list that pass the test
(check-expect (get-items even? (list 1 2 3)) (list 2))
(check-expect (get-items long-string (list "a" "aaaaaaaaaaaaaaaaaaaaaaaaa")) (list "aaaaaaaaaaaaaaaaaaaaaaaaa"))
(define (get-items p? l)
  (cond [(empty? l) '()]
        ((cons? l) (if (p? (first l))
                   (cons (first l) (get-items p? (rest l)))
                   (get-items p? (rest l))))))
(define (long-string s)
  (> (string-length s) 10))

;;ormap [X -> Boolean] [List-of X] -> Boolean
;;checks if at least on item in the list passes the given test

;andmap [X -> Boolean] [List-of X] -> Boolean
;;checks if all items in the list pass the given test

;;filter: [X -> Boolean] [List-of X] -> [List-of X]
;;get all items in the list that pass the test

;;foldr,foldl [X Y -> Y] [List-of X] -> Y
;;combines items in the list using given func

;;map: [X -> Y] [List-of X] -> [List-of Y]
;;applies a function to each member of the list

;(foldr ____ BASE L)
(check-expect (shepherd (list (make-posn 10 10))) #true)
(check-expect (shepherd (list (make-posn 50 50))) #false)
(define (shepherd alop)
  (andmap in? alop))
(define (in? p)
  (and (< 0 (posn-x p) 50)
       (< 0 (posn-y p) 50)))
(check-expect (herd (list (make-posn 10 10) (make-posn 40 40))) (list (make-posn 15 20)))
(define (herd alop)
  (filter in? (map move alop)))
(define (move p)
  (make-posn (+ (posn-x p) 5) (+ (posn-y p) 10)))

(check-expect (sums (list (list 1 2) (list 3 4))) (list 3 7))
(check-expect (sums (list )) (list ))
(define (sums aloon)
  (map sum aloon))
(define (sum alon)
  (foldr + 0 alon))