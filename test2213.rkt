;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname test2213) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; Kind = 'produce | 'meat | 'dairy | 'bakery
;;; An Item is a (make-item Kind String Number)
;;; ShoppingList = [List-of Item]
(define-struct item (kind name price))
(check-expect (total-price (list
                            (make-item 'produce "Broccoli" 3.27) ; Broccoli is $3.27/bunch this week.
                            (make-item 'dairy "Gallon milk" 3.75))) 7.02) ; A gallon of milk is $3.75.

(define (total-price sl)
  (foldr (lambda (x sofar) (+ (item-price x) sofar)) 0 sl))
(check-expect (expensive-items (list
                                (make-item 'produce "Broccoli" 3.27) ; Broccoli is $3.27/bunch this week.
                                (make-item 'dairy "Gallon milk" 3.75)) 3.27) (list (make-item 'dairy "Gallon milk" 3.75)))
(define (expensive-items sl p)
  (filter (lambda (x) (> (item-price x) p)) sl))
#;(check-expect (discount (list
                         (make-item 'produce "Broccoli" 3.27) ; Broccoli is $3.27/bunch this week.
                         (make-item 'dairy "Gallon milk" 3.75)))
              (list (make-item 'produce "Broccoli" 3.27)
                    (make-item 'dairy "Gallon milk" 3.75)))
(define (discount sl)
  (map (lambda (x) (make-item (item-kind x) (item-name x) (* 0.93 (item-price x)))) sl))
(check-expect (vegetarian? (list
                            (make-item 'meats "Broccoli" 3.27) ; Broccoli is $3.27/bunch this week.
                            (make-item 'dairy "Gallon milk" 3.75))) #true)
(define (vegetarian? sl)
  (andmap (lambda (x) (not (symbol=? (item-kind x) 'meat))) sl))

(define (sortq l)
  (foldr insert '() l))
(define (insert n l)
  (foldr (lambda (x sofar) (if (< n x)
                  (cons x l)
                  (cons x sofar))) (list n) l)
  #;(cond [(empty? l) (list n)]
        [else (if (< n (first l))
                  (cons n l)
                  (cons (first l) (insert n (rest l))))]))