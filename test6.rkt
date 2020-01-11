;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname test6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (makestuff n)
  (if (= n 1) (list (list (list n n) (list n n) ))
      (list n n)))
(define (unzip l)
  (cond [(empty? l) '()]
        [(cons? l) (append (check-list (first l)) (unzip (rest l)))]))
(define (check-list l)
  (if (cons? (first l))
      (find-list-pair l) (list l)))
(define (find-list-pair j)
  (cond [(empty? j) '()]
        [(pair? (first j)) (append (list (first j)) (find-list-pair (rest j)))]
        [else (find-list-pair (first j))]))
(define (pair? j)
  (number? (first j)))
(unzip (build-list 5 makestuff))
