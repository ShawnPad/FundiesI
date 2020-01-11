;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A [BT X] is one of:
; - (make-node X [BT X] [BT X])
; - X
(define-struct node (data left right))
; find-largest : [BT Number] -> Number
; finds the largest number in the tree
(check-expect (find-largest (make-node 10 (make-node 4 3 2) (make-node 19 10 23))) 23)
(check-expect (find-largest (make-node 100 (make-node 4 3 2) (make-node 19 10 10))) 100)
(define (find-largest bt)
  (cond [(number? bt) bt]
        [else (max (max (node-data bt) (find-largest (node-left bt)))
                   (find-largest (node-right bt)))]))

; first-lexicographically : [BT String] -> String
; finds the first string in lexicographic order
(check-expect (first-lexicographically (make-node "abc"
                                                  (make-node "lol" "you" "cool")
                                                  (make-node "lol" "you" "cool"))) "abc")
(check-expect (first-lexicographically (make-node "abc"
                                                  (make-node "lol" "you" "cool")
                                                  (make-node "lol" "you" "aaa"))) "aaa")
(check-expect (first-lexicographically (make-node "abc"
                                                  (make-node "lol" "you" "cool")
                                                  (make-node "lol" "a" "aaa"))) "a")
(define (first-lexicographically bt)
  (cond [(string? bt) bt]
        [else ((lambda (x y) (if (string<=? x y) x y))
               ((lambda (x y) (if (string<=? x y) x y))
                (node-data bt) (first-lexicographically (node-left bt)))
               (first-lexicographically (node-right bt)))]))

; find-largest2 : [BT Number] -> Number
; finds the largest number in the tree
(check-expect (find-largest2 (make-node 10 (make-node 4 3 2) (make-node 19 10 23))) 23)
(check-expect (find-largest2 (make-node 100 (make-node 4 3 2) (make-node 19 10 10))) 100)
(define (find-largest2 bt)
  (winningX max bt number?))

; first-lexicographically2 : [BT String] -> String
; finds the first string in lexicographic order
(check-expect (first-lexicographically2 (make-node "abc"
                                                   (make-node "lol" "you" "cool")
                                                   (make-node "lol" "you" "cool"))) "abc")
(check-expect (first-lexicographically2 (make-node "abc"
                                                   (make-node "lol" "you" "cool")
                                                   (make-node "lol" "you" "aaa"))) "aaa")
(check-expect (first-lexicographically2 (make-node "abc"
                                                   (make-node "lol" "you" "cool")
                                                   (make-node "lol" "a" "aaa"))) "a")
(define (first-lexicographically2 bt)
  (winningX (lambda (x y) (if (string<=? x y) x y)) bt string?))

; winningX : [X X -> X] [BT X] Predicate -> X
; compares all Xs in tree with the operator
(check-expect (winningX max (make-node 10 (make-node 4 3 2) (make-node 19 10 23)) number?) 23)
(check-expect (winningX (lambda (x y) (if (string<=? x y) x y))
                        (make-node "abc"
                                   (make-node "lol" "you" "cool")
                                   (make-node "lol" "you" "aaa")) string?) "aaa")
(define (winningX op bt p)
  (cond [(p bt) bt]
        [else (op (op
                   (node-data bt) (winningX op (node-left bt) p))
                  (winningX op (node-right bt) p))]))

; palindrome : [NEList-of 1String] -> [NEList-of 1String]
; creates a palindrome from the string in the list
(check-expect (palindrome (list "abc")) (list "abcba"))
(check-expect (palindrome (list "race")) (list "racecar"))
(define (palindrome l)
  (local [(define (palindromeacc l acc)
            (cond [(empty? l) (list (string-append (implode (append acc (rest (reverse acc))))))]
                  [else (palindromeacc (rest l) (append acc (list (first l))))]))]
    (palindromeacc (explode (first l)) '())))

; only-leaves : [BT Symbol] -> [List-of Symbol]
(check-expect (only-leaves (make-node "lol" (make-node "cool" 'leaf 'leaf) 'leaf))
              (list 'leaf 'leaf 'leaf))
(check-expect (only-leaves 'leaf) (list 'leaf))
(define (only-leaves bt)
  (local [(define (only-leavesacc bt acc worklist)
            (cond
              [(symbol? bt) (if (empty? worklist)
                                (cons bt acc)
                                (only-leavesacc (first worklist) (cons bt acc) (rest worklist)))]
              [else (only-leavesacc (node-left bt) acc (cons (node-right bt) worklist))]))]
    (only-leavesacc bt '() '())))

; fibonacci : Number -> Number
; creates the corresponding fibonacci number
(check-expect (fibonacci 0) 0)
(check-expect (fibonacci 1) 1)
(check-expect (fibonacci 11) 89)
(check-expect (fibonacci 10) 55)
(define (fibonacci n)
  (cond [(zero? n) 0]
        [(= 1 n) 1]
        [else (+ (fibonacci (sub1 n)) (fibonacci (- n 2)))]))

; fibonacci2 : Number -> Number
; creates the corresponding fibonacci number using an accumulator
(check-expect (fibonacci2 0) 0)
(check-expect (fibonacci2 1) 1)
(check-expect (fibonacci2 11) 89)
(check-expect (fibonacci2 10) 55)
(define (fibonacci2 n)
  (local [(define (fibonacciacc n acc)
            (cond[(zero? n) (second acc)]
                 [(= 1 n) (first acc)]
                 [(= (length acc) (add1 n)) (first acc)]
                 [else (fibonacciacc n (cons (+ (first acc) (second acc)) acc))]))]
    (fibonacciacc n '(1 0))))