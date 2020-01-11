;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sorting) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;insertion sort
(define (nsort alon)
  (cond [(empty? alon) '()]
        [else (insert (first alon) (nsort (rest alon)))]))
(define (insert n alon)
  (cond [(empty? alon) (list n)]
        [else (if (> (first alon) n) (cons n alon)
                  (cons (first alon) (insert n (rest alon))))]))
;quick sort
(define (qsort alon)
  (cond [(empty? alon) '()]
        [else (local [(define pivot (first alon))
                      (define less (filter (lambda (x) (< x pivot)) alon))
                      (define equals (filter (lambda (x) (= x pivot)) alon))
                      (define great (filter (lambda (x) (> x pivot)) alon))]
                (append (qsort less) equals (qsort great)))]))