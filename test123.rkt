;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname test123) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "lab8-teachpack.rkt")
(check-expect (flatten-json
               (list 'null '()
                     (make-object
                         (list (list 'greeting
                                     (make-object
                                         (list (list 'formal "hello")
                                               (list 'informal "howdy")
                                               (list 'garbage (list true 1)))))))))
              (list (list "0-NULL" 'null)
                    (list "2-greeting-formal-STRING" "hello")
                    (list "2-greeting-informal-STRING" "howdy")
                    (list "2-greeting-garbage-0-BOOLEAN" true)
                    (list "2-greeting-garbage-1-NUMBER" 1)))
(define (flatten-json ajson)
  (unzip (filter (lambda (j) (not (empty? j))) (getlist ajson))))
(define (unzip l)
  (cond [(empty? l) '()]
        [(cons? l) (append (check-list (first l)) (unzip (rest l)))]))
(define (check-list l)
  (if (cons? (first l))
      (find-list-pair l) (list l)))
(define (getlist ajson)
  (cond [(string? ajson) (list "STRING" ajson)]
        [(number? ajson) (list "NUMBER" ajson)]
        [(boolean? ajson) (list "BOOLEAN" ajson)]
        [(symbol? ajson) (list "NULL" ajson)]
        [(list? ajson) (list-stuff ajson)]
        [(object? ajson) (obj-stuff ajson)]))
; JSONArray -> ?
(define (list-stuff ajson)
  (build-list (length ajson)
              (lambda (n) (append-to-front
                           (getlist (list-ref ajson n))
                           (number->string n)))))
(define (append-to-front j s)
  (cond [(empty? j) '()]
        [(pair? j) (list (string-append s "-" (first j)) (second j))]
        [else  (map (lambda (ajson) (append-to-front ajson s)) (find-list-pair j))
         #;(cons
               (list (string-append s "-" (first (find-pair j))) (second (find-pair j)))
               (append-to-front (rest j) s))]))
(define (pair? j)
  (string? (first j)))
(define (find-pair j)
  (cond
    [(string? (first j)) j]
    [else (find-pair (first j))]))
(define (find-list-pair j)
  (cond [(empty? j) '()]
        [(pair? (first j)) (append (list (first j)) (find-list-pair (rest j)))]
        [else (find-list-pair (first j))]))
#;(define (find-list-pair j)
  (cond [(and (pair? (first j)) (pair? (second j))) j]
        [else (find-list-pair (first j))]))
; JSONObject -> ?
(define (obj-stuff ajson)
  (jplist-stuff (object-content ajson)))

; [List-of JSONPair] -> ?
(define (jplist-stuff ajp)
  (map pair-stuff ajp)
  #;(cond [(empty? ajp) '()]
        [(cons? ajp) (append (pair-stuff (first ajp))
                             (jplist-stuff (rest ajp)))]))

; JSONPair -> ?
(define (pair-stuff ajson)
  (append-to-front (getlist (second ajson)) (symbol->string (first ajson))))
(flatten-json
               (list 'null '()
                     (make-object
                         (list (list 'greeting
                                     (make-object
                                         (list (list 'formal "hello")
                                               (list 'informal "howdy")
                                               (list 'garbage (list true 1)))))))))
;(check-expect (flatten-json (make-object (list (list 'formal "hello")))) (list (list "formal-STRING" "hello")))
#|(define (flatten-json ajson)
  (getlist ajson))
; JSON -> ?
(check-expect (getlist (list 'formal "hello")) (list (list "formal-STRING" "hello")))
(define (getlist ajson)
  (cond [(string? ajson) (list "STRING" ajson)]
        [(number? ajson) (list "NUMBER" ajson)]
        [(boolean? ajson) (list "BOOLEAN" ajson)]
        [(symbol? ajson) (list "NULL" ajson)]
        [(list? ajson) (list-stuff (filter (lambda (x) (not (empty? x))) ajson))]
        [(object? ajson) (obj-stuff ajson)]))
(check-expect (list-stuff (list 'null '())) (list (list "0-NULL" 'null)))

(define (list-stuff ajson)
  (cond [(empty? ajson) '()]
        [(cons? ajson) (append (build-list (sub1 (length ajson)) (lambda (n) (append-to-pair (getlist (first ajson)) (number->string n))))
                               (list-stuff (rest ajson)))]))
                               
(define (append-to-pair jp s)
  (list (string-append s "-" (first jp)) (second jp)))
; JSONObject -> ?
(define (obj-stuff ajson)
  (jplist-stuff (object-content ajson)))

; [List-of JSONPair] -> ?
(define (jplist-stuff ajp)
  (map pair-stuff ajp))

; JSONPair -> ?
(define (pair-stuff ajson)
  #;(local [(define restoflist (getlist (second ajson)))]
      (list (string-append (symbol->string (first ajson)) "-" (first restoflist)) (second restoflist)))
  (append-to-pair (getlist (second ajson)) (symbol->string (first ajson))))

(define (flatten-json ajson)
  (getlist ajson))
(define (getlist ajson)
  (cond [(string? ajson) (list "STRING" ajson)]
        [(number? ajson) (list "NUMBER" ajson)]
        [(boolean? ajson) (list "BOOLEAN" ajson)]
        [(symbol? ajson) (list "NULL" ajson)]
        [(list? ajson) (list-stuff ajson)]
        [(object? ajson) (obj-stuff ajson)]))

; JSONArray -> ?
(define (list-stuff ajson)
  (build-list (length ajson)
              (lambda (n) (append-to-front
                           (getlist (list-ref ajson n))
                           (number->string n)))))
(define (append-to-front j s)
  (cond [(empty? j) '()]
        [(pair? j) (list (string-append s "-" (first j)) (second j))]
        [else (map (lambda (ajson) (list (string-append s "-" (first ajson)) (second ajson))) j)]))
(define (pair? j)
  (string? (first j)))
(define (find-pair j)
  (cond
    [(string? (first j)) j]
    [else (find-pair (first j))]))  
; JSONObject -> ?
(define (obj-stuff ajson)
  (jplist-stuff (object-content ajson)))

; [List-of JSONPair] -> ?
(define (jplist-stuff ajp)
  (cond [(empty? ajp) '()]
        [(cons? ajp) (append (pair-stuff (first ajp))
                             (jplist-stuff (rest ajp)))]))

; JSONPair -> ?
(define (pair-stuff ajson)
  (append-to-front (getlist (second ajson)) (symbol->string (first ajson))))|#
