;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct leaf [])
(define-struct node [data left right])
; A [Tree X] is one of:
; - (make-leaf)
; - (make-node X [Tree X] [Tree X])
 
 
(define-struct person [name yob])
; A Person is a (make-person String Number)
; - where name is the person's name
; - and yob is their year of birth
 
; A FT is a [Tree Person] and represents a family tree, with the youngest person at the root.
#;(define (tree-temp t)
    (cond [(leaf? t) ...]
          [(node? t) ... (node-data t) (tree-temp (node-left t)) (tree-temp (node-right t))]))
;mirror: Tree -> Tree
;swaps the left and right nodes for the whole tree
(check-expect (mirror (make-leaf)) (make-leaf))
(check-expect (mirror (make-node "cool" (make-leaf) (make-leaf)))
              (make-node "cool"(make-leaf) (make-leaf))) 
(check-expect (mirror (make-node "cool"
                                 (make-node "beans" (make-leaf) (make-leaf))
                                 (make-node "are" (make-leaf) (make-leaf))))
              (make-node "cool"
                         (make-node "are" (make-leaf) (make-leaf))
                         (make-node "beans" (make-leaf) (make-leaf))))
(check-expect (mirror (make-node "lol"
                                 (make-node "you" (make-leaf) (make-leaf))
                                 (make-node "cool" (make-leaf)
                                            (make-node "you think" (make-leaf) (make-leaf)))))
              (make-node "lol"
                         (make-node "cool"
                                    (make-node "you think" (make-leaf) (make-leaf)) (make-leaf))
                         (make-node "you" (make-leaf) (make-leaf))))
(define (mirror t)
  (par-tree (make-leaf) flip t)
  #;(cond [(leaf? t) t]
          [(node? t) (make-node (node-data t) (mirror (node-right t)) (mirror (node-left t)))]))

;names: FT -> [List-of String]
;returns all names in family tree
(check-expect (names (make-node (make-person "Bob" 100)
                                (make-node (make-person "Joe" 130)
                                           (make-node (make-person "Jack" 160)
                                                      (make-leaf) (make-leaf))
                                           (make-leaf))
                                (make-node (make-person "Carl" 132) (make-leaf) (make-leaf))))
              (list "Bob" "Joe" "Jack" "Carl"))
(check-expect (names (make-node (make-person "Joe" 130) (make-leaf) (make-leaf)))
              (list "Joe"))
(check-expect (names (make-leaf)) (list ))
(define (names t)
  (par-tree '() getnames t)
  #;(cond [(leaf? t) '()]
          [(node? t) (cons (person-name (node-data t))
                           (append (names (node-left t))
                                   (names (node-right t))))]))

;par-tree: X [X Tree Tree->Y] Tree -> Y
;abstraction for names and mirror
(check-expect (par-tree '() getnames (make-node (make-person "Joe" 130) (make-leaf) (make-leaf)))
              (list "Joe"))
(check-expect (par-tree (make-leaf) flip (make-node "cool"
                                                    (make-node "beans" (make-leaf) (make-leaf))
                                                    (make-node "are" (make-leaf) (make-leaf))))
              (make-node "cool"
                         (make-node "are" (make-leaf) (make-leaf))
                         (make-node "beans" (make-leaf) (make-leaf))))
(define (par-tree base op t)
  (cond [(leaf? t) base]
        [(node? t) (op (node-data t)
                       (par-tree base op (node-left t))
                       (par-tree base op (node-right t)))]))
;flip: X Tree Tree -> Tree
;swaps left and right node
(check-expect (flip "pie" (make-node "yum" (make-leaf) (make-leaf)) (make-leaf))
              (make-node "pie" (make-leaf) (make-node "yum" (make-leaf) (make-leaf))))
(check-expect (flip "pie" (make-leaf) (make-leaf)) (make-node "pie" (make-leaf) (make-leaf)))
(define (flip d t1 t2)
  (make-node d t2 t1))
;getnames: Person List List -> [List-of String]
;creates lists of names from tree
(check-expect (getnames (make-person "Joe" 130) (list "Bob") (list "Kim" "Bryan"))
              (list "Joe" "Bob" "Kim" "Bryan"))
(check-expect (getnames (make-person "Joe" 130) (list ) (list ))
              (list "Joe"))
(define (getnames d t1 t2)
  (cons (person-name d) (append t1 t2)))

; An Sexpr is one of: 
; – Symbol 
; – Lexpr
 
; A Lexpr is one of: 
; – '()
; – (cons Sexpr Lexpr)

#;(define (sexpr-temp s)
    (cond [(symbol? s) ...]
          [(lexpr? s) (lexpr-temp s)]))
#;(define (lexpr-temp l)
    (cond [(empty? l) ...]
          [(cons? l) ... (sexpr-temp (first l)) (lexpr-temp (rest l))]))

;contains-same-symbols?: Sexpr Sexpr -> Boolean
;are the symbols in each sexpr present in both?
(check-expect (contains-same-symbols? 'a '(a)) #true)
(check-expect (contains-same-symbols? '((a (b b) c)) '(c b a)) #true)
(check-expect (contains-same-symbols? 'a '(a b)) #false)
(define (contains-same-symbols? s1 s2)
  (local [;given Sexpr expect list
          (define (flatten l)
            (cond [(symbol? l) (list l)]
                  [(cons? l) (parse l)]))
          ;given list expect list
          (define (parse l)
            (foldr (lambda (s sofar) (append (flatten s) sofar)) '() l))]
    (and (andmap (lambda (as) (member as (flatten s2))) (flatten s1))
         (andmap (lambda (as) (member as (flatten s1))) (flatten s2)))))