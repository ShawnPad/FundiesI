;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname A7-checkz) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;(check-expect (alphabetic? (list )) #true)
;(check-expect (alphabetic? (list "lol" "you cool")) #true)
;(check-expect (alphabetic? (list "okay" "1234")) #false)
;(check-expect (alphabetic? (list "okay1" "you cool")) #false)


;(check-expect (factorial 0) 1)
;(check-expect (factorial 1) 1)
;(check-expect (factorial 2) 2)
;(check-expect (factorial 3) 6)

(check-expect (at-0 (list add1 sub1 abs)) (list 1 -1 0))
(define (at-0 lof)
  (map set0 lof))
(define (set0 f)
  (local [(define apply0 f)]
    (apply0 0)))

(define-struct tweet [author text])
(define-struct image-tweet [author text image])
(define-struct retweet [author tweet])
; A Tweet is one of:
; - (make-tweet String String)
; - (make-image-tweet String String Image)
; - (make-retweet String Tweet)
; INTERPRETATION:  A tweet is either a message (make-tweet),
; a message and an image (make-image-tweet), or a retweet
; of another tweet (make-retweet).  All tweets have authors.
 
; A Feed is a [List-of Tweet]
; INTERPREATION:  A list of the Tweets in a user's feed.
#;(define (tweet-temp t)
    (cond [(tweet? t) (tweet-author t) (tweet-text t)]
          [(image-tweet? t) (tweet-author t) (tweet-text t)]
          [(retweet? t) (tweet-author t) (tweet-temp (retweet-tweet t))]))
#;(define (feed-temp f)
    (cond [(empty? f) ...]
          [(cons? f) ... (tweet-temp (first f))
                     ... (feed-temp (rest f))]))
(check-expect (count-tweets.v1 (list (make-tweet "me" "hello") (make-retweet "you" (make-tweet "me" "hello")))) 3)
(check-expect
 (count-tweets.v1
  (list (make-tweet "foo" "bar")
        (make-retweet "baz"
                      (make-retweet "blah"
                                    (make-image-tweet "arg"
                                                      "grr"
                                                      empty-image)))))
 4)
(define (count-tweets.v1 f)
  (cond [(empty? f) 0]
        [(cons? f) (+ (count-t (first f))
                      (count-tweets.v1 (rest f)))]))
(define (count-t t)
  (cond [(tweet? t) 1]
        [(image-tweet? t) 1]
        [(retweet? t) (+ 1 (count-t (retweet-tweet t)))]))
(check-expect (count-tweets.v2 (list (make-tweet "me" "hello") (make-retweet "you" (make-tweet "me" "hello")))) 3)
(check-expect
 (count-tweets.v2
  (list (make-tweet "foo" "bar")
        (make-retweet "baz"
                      (make-retweet "blah"
                                    (make-image-tweet "arg"
                                                      "grr"
                                                      empty-image)))))
 4)
(check-expect (count-tweets.v2 (list (make-image-tweet "arg"
                                "grr"
                                empty-image))) 1)
(define (count-tweets.v2 f)
  (foldr count-t.v2 0 f))
(define (count-t.v2 t n)
  (cond [(tweet? t) (+ n 1)]
        [(image-tweet? t) (+ n 1)]
        [(retweet? t) (+ 1 n (count-t (retweet-tweet t)))]))
(check-expect (absolute (list -1 10 -5)) (list 1 10 5))
(define (absolute lon)
  (map abs lon))
(check-expect (largest (list 1 10 2)) 10)
(define (largest lon)
  (foldr compare 0 lon))
(define (compare n on)
  (if (> n on) n on))