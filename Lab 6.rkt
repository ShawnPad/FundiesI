;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Lab 6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An AddressBook is one of:
; - '()
; - (cons Record AddressBook)
 
(define-struct record [first last phone add])
; A Record is a (make-record String String Nat Address)
; - where first is the first name of the person this record is about
; - last is the last name of the person this record is about
; - phone is the phone number of the person this record is about
; - and add is the address of the person this record is about
 
(define-struct address [num st city state zip])
; An Address is a (make-address Nat String String String Zip)
; - where num is the street number of the address
; - st is the street name of the address
; - city is the name of the city this address is in
; - state is the name of the state this address is in
; - and zip is the zipcode of the city this address is in
 
(define-struct zipcode [a b c d e])
; A Zip is a (make-zipcode Digit Digit Digit Digit Digit)
; - where a is the first digit in the zipcode
; - b is the second digit in the zipcode
; - c is the third digit in the zipcode
; - d is the fourth digit in the zipcode
; - and e is the fifth digit in the zipcode
 
; A Digit is a Nat in the range [0,9]
#;(define (addressbook-temp ab)
    (cond [(empty? ab) ...]
          [(cons? ab)
           ... (record-temp (first ab))
           ... (addressbook-temp (rest ab))]))
#;(define (record-temp r)
    (record-first r) (record-last r) (record-phone r) (address-temp (record-add r)))
#;(define (address-temp a)
    (address-num a) (address-st a) (address-city a) (address-state a) (zip-temp (address-zip a)))
#;(define (zip-temp z)
    (zip-a z) (zip-b z) (zip-c z) (zip-d z) (zip-e z))
(define BOOK (list (make-record "me" "you" 123 (make-address 123 "poo" "toilet" "bathroom" (make-zipcode 1 2 3 4 5)))))
(check-expect (update-zip-for-all BOOK 1)
              (list (make-record "me" "you" 123 (make-address 123 "poo" "toilet" "bathroom" (make-zipcode 1 2 3 4 1)))))
(define (update-zip-for-all book digit)
      (cond [(empty? book) '()]
          [(cons? book)
           (cons (change-record (first book) digit)
                 (update-zip-for-all (rest book) digit))]))
(define (change-record r digit)
    (make-record (record-first r) (record-last r) (record-phone r) (change-address (record-add r) digit)))
(define (change-address a digit)
    (make-address (address-num a) (address-st a) (address-city a) (address-state a) (change-zip (address-zip a) digit)))
(define (change-zip z digit)
    (make-zipcode (zipcode-a z) (zipcode-b z) (zipcode-c z) (zipcode-d z) digit))
(define (move-posn-up p)
  (move-posn-y p -1))
  ;(make-posn (posn-x p) (+ (posn-y p) -1)))
(define (move-posn-down p)
  (move-posn-y p 1))
  ;(make-posn (posn-x p) (+ (posn-y p) 1)))
(define (move-posn-y p y)
  (make-posn (posn-x p) (+ (posn-y p) y)))
(define (prefix-with-hello alos)
  (cond [(empty? alos) '()]
        [(cons? alos) (cons (string-append "hello " (first alos))
                            (prefix-with-hello (rest alos)))]))
(define (prefix-with-goodbye alos)
  (cond [(empty? alos) '()]
        [(cons? alos) (cons (string-append "byelol " (first alos))
                            (prefix-with-goodbye (rest alos)))]))
(define (prefix-with alos prefix)
  (cond [(empty? alos) '()]
        [(cons? alos) (cons (string-append prefix " " (first alos))
                            (prefix-with-hello (rest alos)))]))