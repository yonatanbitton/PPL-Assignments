#lang racket
(require "utils.rkt"
         "env-closure.rkt"
         "env-box.rkt"
         "env-ast.rkt"
         "env-box-eval.rkt")

(provide (all-defined-out))

;; Support exception handling
(define (try f (ans 'error))
  (with-handlers ((exn?
                   (lambda (exn) ans))) (f)))

(define env-box-eval-tests
  (lambda ()
    (display "env-box-eval-tests:\t")
    (let ((ge the-global-env))
      (run-tests
       (test (env-box-eval (parseL4 '((lambda (x lazy) 1) (/ 1 0))) ge) => '1)

       ))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(env-box-eval-tests)
