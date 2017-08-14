#lang racket
(require "utils.rkt"
         "env.rkt"
         "L1-ast.rkt"
         "L2-ast.rkt"
         "L3-ast.rkt"
         "L2-eval.rkt"
         "L3-eval.rkt")


(define tests
  (lambda ()
    (display "tests:\t")
    (let ((ge (make-empty-env)))
      (run-tests
       (test (L3-applicative-eval (parseL3 '((lambda (a lazy) 1) (/ 1 0))) ge) => '1)
       
       ))))

(tests)