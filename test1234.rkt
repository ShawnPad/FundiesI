;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname test1234) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; A NumTree (binary tree with numbers at the leaves) is one of:
;;; - Number
;;; - (make-node NumTree NumTree)
(define-struct node (left right))
(define tree1 (make-node (make-node (make-node 8 0)
                                    (make-node 1 3))
                         (make-node (make-node 2 7)
                                    (make-node 9 4))))
(define (tree-nav t lod)
  (cond [(empty? lod) t]
        [(cons? lod) (if (symbol? 'left (first lod))
                         (tree-nav (node-left t) (rest lod))
                         (tree-nav (node-right t) (rest lod)))]))