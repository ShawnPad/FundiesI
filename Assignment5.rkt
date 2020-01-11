;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Assignment5) (read-case-sensitive #t) (teachpacks ((lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
;ex6
; A Size is one of:
; - "small"
; - "medium"
; - "large"
 
(define-struct drip-coffee [cream size])
(define-struct latte [size])
(define-struct cortado [size])
; A Coffee is one of:
; - (make-drip-coffee Boolean Size)
; - (make-latte Size)
; - (make-cortado Size)
; INTERPRETATION: Represents three possible coffee orders.  Each order 
; has a size; drip coffee might also have cream in it.
 
; A CoffeeOrder (List-of-Coffee) is one of:
; - '()
; - (cons Coffee CoffeeOrder)
; INTERPRETATION: The list of coffee orders at a local coffee shop
 
; A MaybeCoffee is one of
; - #false
; - Coffee
; INTERPRETATION: Represents maybe having a Coffee

(define C1 (make-cortado "small"))
(define C2 (make-latte "medium"))
(define C3 (make-latte "large"))
(define C4 (make-drip-coffee #true "large"))
(define CO1 (cons C3 '()))
(define CO2 (cons C3 (cons C1 '())))
(define CO3 (cons C2 (cons C3 '())))
(define CO4 (cons C3 (cons C2 (cons C1 (cons C1 '())))))
(define CO5 (cons C4 (cons C1 (cons C1 (cons C1 (cons C1 (cons C1 '())))))))
(define CO6 (cons C3 (cons C1 (cons C2 (cons C1 (cons C1 (cons C1 '())))))))
(check-expect (last-latte CO1) C3)
(check-expect (last-latte CO2) C3)
(check-expect (last-latte CO3) C3)
(check-expect (last-latte CO4) C2)
(check-expect (last-latte CO5) #false)
(check-expect (last-latte CO6) C2)
#;(define (CoffeeOrder-temp co)
    (cond [(empty? co) ...]
          [(cons? co) ... (coffee-temp (first co))
                      ... (CoffeeOrder-temp (rest co))]))
#;(define (coffee-temp c)
    (cond [(latte? c) ... (size-temp (latte-size c))]
          [(cortado? c) ...(size-temp (cortado-size c))]
          [(drip-coffee? c) ...(size-temp (drip-coffee-size c)) (drip-coffee-cream c)]))
#;(define (size-temp s)
    (cond [(string=? s "small") ...]
          [(string=? s "medium") ...]
          [(string=? s "large") ...]))
;CoffeeOrder -> Latte or Boolean
;returns the last latte in a CoffeeOrder if none returns false
(define (last-latte co)
  (cond [(empty? co) #false]
        [(cons? co) (if (latte? (last-latte (rest co)))
                        (last-latte (rest co))
                        (if (latte? (first co)) (first co) #false))]))

;ex6
(define-struct dinner-check [food drink tax tip])
; A DinnerCheck is a (make-dinner-check Number Number Number Number)
; INTERPRETATION: Represents the bill for a table in a restaurant,
; with numbers representing the cost of the food, drink, tax, and tip.
 
; An OrderBook (List-of-DinnerCheck) is one of:
; - '()
; - (cons DinnerCheck OrderBook)

(define-struct orderb (cons dinner-check orderb))

(define orderb1 (cons (make-dinner-check 1 2 .3 1) '()))
(define orderb2 (cons (make-dinner-check 51 52 10 53) orderb1))
(define orderb3 (cons (make-dinner-check 3 9 1.2 12) orderb2))

;; OrderB -> ??
#; (define (orderb-temp anorb)
     (cond [(empty? anorb) ...]
           [(cons? anorb) ... (first anorb)
                          ... (rest anorb)]))

;; DinnerCheck -> ??
#; (define (din-check-temp din-check)
     (din-check-food din-check) ... (din-check-drink din-check) ...
     (din-check-tax din-check) ... (din-check-tip din-check) ... )

;; anomalous?: DinnerCheck -> Boolean
;; determines if a DinnerCheck is anomalous
;; if the tax is not equal to 10% of food and drink
;; if tip is greater than 50% of food, drink, and tax
(define (anomalous? din-check)
  (or (not (= (dinner-check-tax din-check)
              (dtax (dinner-check-food din-check) (dinner-check-drink din-check))))
      (> (dinner-check-tip din-check)
         (dtip (dinner-check-food din-check) (dinner-check-drink din-check)))))

;; flag-anomalous: OrderB -> OrderB
;; creates a list of all anomalous orders
(check-expect (flag-anomalous '()) '())
(check-expect (flag-anomalous orderb1) '())
(check-expect (flag-anomalous orderb2) (cons (make-dinner-check 51 52 10 53) '()))
(check-expect (flag-anomalous orderb3) (cons (make-dinner-check 3 9 1.2 12)
                                             (cons (make-dinner-check 51 52 10 53) '())))
(define (flag-anomalous anorb)
  (cond [(empty? anorb) '()]
        [(cons? anorb) (if (anomalous? (first anorb))
                           (cons (first anorb) (flag-anomalous (rest anorb)))
                           (flag-anomalous (rest anorb)))]))

;; dtax: Food Drink -> Tax
(define (dtax a b)
  (* .1 (+ a b)))
;; dtip: Food Drink -> Tip
(define (dtip x y)
  (* .5 (+ x y (dtax x y))))

;ex7
(define-struct seat [number is-empty passenger-name])
; A Seat is a (make-seat String Boolean String)
 
; INTERPRETATION: A seat assignment on a plane, consisting of a
; seat number ("34B"), whether or not it is empty, and the name of the
; passenger if it is not empty.  If a seat assignment is empty, the
; passenger name should be the empty string ("").
 
; A Flight (List-of-Seat) is one of:
; - '()
; - (cons Seat Flight)
; INTERPRETATION: Represents the seats present in a given flight.
; (The seats are not guaranteed to be ordered by seat number; it's just a
; set of seats.)

(define S1 (make-seat "1A" #false "Bob"))
(define S2 (make-seat "1B" #false "Joe"))
(define S3 (make-seat "2A" #false "John"))
(define S4 (make-seat "2B" #false "Kevin"))
(define S5 (make-seat "3A" #true ""))
(define S6 (make-seat "3B" #true ""))
(define F1 (cons S1 (cons S2 (cons S3 (cons S4 (cons S5 (cons S6 '())))))))
(check-expect (seat-member? "1A" F1) #true)
(check-expect (seat-member? "4A" F1) #false)
#;(define (flight-temp f)
    (cond [(empty? f) ...]
          [(cons? f) ... (seat-temp (first f))
                     ... (flight-temp (rest f))]))
#;(define (seat-temp s)
    ... (seat? s)
    ... (seat-number s)
    ... (seat-isempty s)
    ... (seat-passenger-name s))
;String Flight -> Boolean
;checks if a seat number appears in a flight
(define (seat-member? n f)
  (cond [(empty? f) #false]
        [(cons? f) (or (seat-temp n (first f)) (seat-member? n (rest f)))]))
(define (seat-temp n s)
  (string=? n (seat-number s)))
