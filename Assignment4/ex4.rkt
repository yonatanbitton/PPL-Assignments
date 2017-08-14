#lang racket


; Signature: append(list1, list2)
; Purpose: return a list which the arguments lists, from left to right.
; Purpose: Append list2 to list1.
; Type: [List * List -> List]
; Example: (append '(1 2) '(3 4)) => '(1 2 3 4)
; Tests: (append '() '(3 4)) => '(3 4)
(define append
 (lambda (x y)
 (if (empty? x)
 y
 (cons (car x)
 (append (cdr x) y)))))


; Signature: append$(list1, list2, cont)
; Purpose: return a list which the arguments lists, from left to right.
; Purpose: Append list2 to list1,CPS style. 
; Type: ; Type: [List(T1) * List(T2) * [List(T1 U T2) -> T3] -> List]
; Example: (append$ '(1 2) '(3 4) (lambda (x) x)) => '(1 2 3 4)
; Tests: (append$ '() '(3 4) (lambda (x) x)) => '(3 4)
(define append$
  (lambda (x y c)
    (if (empty? x)
        (c y)
        (append$ (cdr x) y (lambda (appended-cdr) (c (cons (car x) appended-cdr)))))))




(define make-tree list)
(define add-subtree cons)
(define make-leaf (lambda (x) x))
(define empty-tree? empty?)
(define first-subtree car)
(define rest-subtree cdr)
(define leaf-data (lambda (x) x))
(define composite-tree? list?)
(define leaf? (lambda (x) (not (composite-tree? x))))


; Signature: equal-trees$(t1, t2, succ, fail)
; Type: [Tree(T1) * Tree(T2) * [Tree([T1 U T2)->T3] * [Tree([T1 U T2)->T3]]
; Purpose: On success, equal-trees$ activates the succ function on the a new tree
; which it's values are pairs that are constructed from the original trees. CPS style. 
; Example: (equal-trees$ '(1 (2) ((4 5))) '(1 (#t) ((4 5))) id id) => ‘((1 . 1) ((2 . #t)) (((4 . 4) (5 . 5))))
; Tests: (equal-trees$ '(1 (2) ((4 5))) '(1 (#t) ((4 5))) id id) => ‘((1 . 1) ((2 . #t)) (((4 . 4) (5 . 5))))
(define equal-trees$
  (lambda (t1 t2 succ fail)
     
    (cond ((and(empty? t1)(not(empty? t2))) (fail (cons t1 (car t2))))
          ((and(empty? t2)(not(empty? t1))) (fail (cons (car t1) t2)))
          ((and(empty? t1)(empty? t2)) (succ '()))
          ((and (leaf? t1) (leaf? t2)) (succ (cons (leaf-data t1) (leaf-data t2))))
          ((and (leaf? t1) (not (leaf? t2))) (fail (cons (leaf-data t1) t2)))
          ((and (not (leaf? t1)) (leaf? t2)) (fail (cons t1 (leaf-data t2))))
          (else
           (equal-trees$ (first-subtree t1) (first-subtree t2) 
           	(lambda (first-res)                                  
                     (equal-trees$ (rest-subtree t1) (rest-subtree t2)                                                                              
                                   (lambda (rest-res)
                                      (succ (add-subtree first-res rest-res))
                                     )
                                   (lambda (rest-res)
                                     (fail rest-res))
                                   ))
             (lambda (first-res)
               (fail first-res)
               )
)))))

