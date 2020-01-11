;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname class2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define (greeting name);string->string given name produces greeting
  (string-append "Hello " name " you are a loser lol"))
(overlay (circle 30 "solid" "red") (square 50 "solid" "blue") (empty-scene 100 100))
