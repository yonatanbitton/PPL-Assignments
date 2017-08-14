#lang racket
(require "parser.rkt")
(provide (all-defined-out))

;; <cexp> ::= <number>                      / num-exp(val:number)
;;         |  <boolean>                     / bool-exp(val:boolean)
;;         |  <string>                      / str-exp(val:string)
;;         |  <variable>                    / var-exp(var:symbol)
;;         |  ( lambda ( <var>* ) <cexp>+ ) / proc-exp(params:List(var-exp), body:List(cexp))
;;         |  ( if <cexp> <cexp> <cexp> )   / if-exp(test: cexp, then: cexp, else: cexp)
;;         |  ( let ( binding* ) <cexp>+ )  / let-exp(bindings:List(binding), body:List(cexp))
;;         |  ( <cexp> <cexp>* )            / app-exp(rator:cexp, rands:List(cexp))
;;         |  ( quote <sexp> )              / literal-exp(val:sexp)

;; Purpose: rewrite a single let-exp as a lambda-application form
;; Signature: rewrite-let(cexp)
;; Type: [Cexp -> Cexp]
;; Example:
;; (unparse (rewrite-let (parse '(let ((x 1)) (+ x x))))) => '((lambda (x) (+ x x)) 1)
(define rewrite-let
  (lambda (cexp)
    (let ((vars (map binding->var (let-exp->bindings cexp)))
          (vals (map binding->val (let-exp->bindings cexp)))
          (body (let-exp->body cexp)))
          display vars
          display vals
          display body
      (make-app-exp 
       (make-proc-exp vars body)
       vals))))


;; Purpose: rewrite all occurrences of let in an expression to lambda-applications.
;; Signature: rewrite-all-let(exp)
;; Type: [Exp -> Exp]
;; Example:
;; (unparse (rewrite-all-let (parse '(lambda (x) (let ((y (+ x 2))) (* y y))))))
;;  => '(lambda (x) ((lambda (y) (* y y)) (+ x 2)))
(define rewrite-all-let
  (lambda (exp)
    (cond ((def-exp? exp) (make-def-exp (def-exp->var exp)
                                        (rewrite-all-let (def-exp->val exp))))
          ((num-exp? exp) exp)
          ((bool-exp? exp) exp)
          ((str-exp? exp)  exp)
          ((var-exp? exp)  exp)
          ((literal-exp? exp) exp)
          ((proc-exp? exp) (make-proc-exp (proc-exp->params exp)
                                          (map rewrite-all-let (proc-exp->body exp))))
          ((if-exp? exp) (make-if-exp
                          (rewrite-all-let (if-exp->test exp))
                          (rewrite-all-let (if-exp->then exp))
                          (rewrite-all-let (if-exp->else exp))))
          ((let-exp? exp) (rewrite-all-let (rewrite-let exp)))
          ((app-exp? exp) (make-app-exp (rewrite-all-let (app-exp->rator exp))
                                        (map rewrite-all-let (app-exp->rands exp))))
          (else (error "Unknown exp type: " exp)))))


; Purpose: rewrite expressions containing let*-exp into ASTs that contain only let-exp
; Signature: rewrite-all-let*(exp)
; Type: [CExp -> CExp]
;EXAMPLE: Original command: (parse '(let* ((x 1) (y 3)) (+ y x))) 
(define rewrite-all-let* ;exp=(let*-exp ((binding (var-exp x) (num-exp 1)) (binding (var-exp y) (num-exp 3))) ((app-exp (var-exp +) ((var-exp y) (var-exp x)))))
  (lambda (exp)
   
    (cond ((def-exp? exp) (make-def-exp (def-exp->var exp)
                                        (rewrite-all-let* (def-exp->val exp))))
          ((num-exp? exp) exp)
          ((bool-exp? exp) exp)
          ((str-exp? exp)  exp)
          ((var-exp? exp)  exp)
          ((literal-exp? exp) exp)
          ((proc-exp? exp) (make-proc-exp (proc-exp->params exp)
                                          (map rewrite-all-let* (proc-exp->body exp))))
          ((if-exp? exp) (make-if-exp
                          (rewrite-all-let* (if-exp->test exp))
                          (rewrite-all-let* (if-exp->then exp))
                          (rewrite-all-let* (if-exp->else exp))))
          ;((let-exp? exp) (rewrite-all-let* (rewrite-let exp))) ;if in the let expression I have let* 
          ((let-exp? exp)
           (make-let-exp (map (lambda (binding)  ;second version - just if it's b                    
                                (make-binding (binding->var binding)
                                              (rewrite-all-let* (binding->val binding)))
                               ) (let-exp->bindings exp))
                         (map rewrite-all-let* (let-exp->body exp) ;another version - activate on everyone
                               ) 
                         )
          )
          ((let*-exp? exp)         
           (if (= (length (second exp)) 0)
               (make-let-exp (list) (list (rewrite-all-let* (car (third exp)))))
           (if (= (length (second exp)) 1) ;second exp=((binding (var-exp x) (num-exp 1)) (binding (var-exp y) (num-exp 3)))               
               (make-let-exp
                     (list (make-binding(second (car (second exp))) ;car exp = (binding (var-exp x) (num-exp 1))
                                   (rewrite-all-let* (third (car (second exp))))))                    
                     (map rewrite-all-let* (third exp))                    
                     )  ;third exp =  ((app-exp (var-exp +) ((var-exp y) (var-exp x))))). (In each recursive call aswell)             
              (make-let-exp ;ELSE
                     (list (make-binding(second (car (second exp))) ;car exp = (binding (var-exp x) (num-exp 1))
                                   (third (car (second exp)))))
                     (list (rewrite-all-let* ( make-let*-exp (cdr (second exp)) (third exp)))) 
                     ;(make-let*-exp (cdr (second exp)) (third exp)) ;BODY: (let-exp ((binding (var-exp y) (num-exp 3))) ((app-exp (var-exp +) ((var-exp y) (var-exp x)))))
                     )
              )
          )
          )
          ((app-exp? exp) (make-app-exp (rewrite-all-let* (app-exp->rator exp))
                                        (map rewrite-all-let* (app-exp->rands exp))))
          (else (error "Unknown exp type: " exp)))))


