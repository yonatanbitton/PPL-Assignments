#lang racket
(require "L5-ast.rkt"
         "tenv.rkt")
(provide (all-defined-out))

;; Purpose: Check that type expressions are equivalent
;; as part of a fully-annotated type check process of exp.
;; Throws an error if the types are different - void otherwise.
;; Exp is only passed for documentation purposes.
;; Type: [TE * TE * Exp -> Void]
;; @@@ Collect errors
(define check-equal-type!
  (lambda (te1 te2 exp)
    (cond ((and (tvar? te1) (tvar? te2))
           (if (eq? (tvar->var te1) (tvar->var te2))
               'same-tvars-ok
               (check-tvar-equal-type! te1 te2 exp)))
          ((tvar? te1) (check-tvar-equal-type! te1 te2 exp))
          ((tvar? te2) (check-tvar-equal-type! te2 te1 exp))
          ((and (atomic-te? te1) (atomic-te? te2))
           (if (not (eq? (atomic-te->name te1)
                         (atomic-te->name te2)))
               (error "Incompatible atomic types " te1 te2 exp)
               'atomic-ok))
          ((and (proc-te? te1) (proc-te? te2))
           (let ((args-te1 (proc-te->param-tes te1))
                 (args-te2 (proc-te->param-tes te2))
                 (return-te1 (proc-te->return-te te1))
                 (return-te2 (proc-te->return-te te2)))
             (if (not (= (length args-te1) (length args-te2)))
                 (error "Wrong number of arguments " te1 te2 exp)
                 'args-number-ok)
             (for-each (lambda (rand1 rand2)
                         (check-equal-type! rand1 rand2 exp))
                       args-te1 args-te2)
             (check-equal-type! return-te1 return-te2 exp)
             'proc-tes-ok))
          ((and (pair-te? te1) (pair-te? te2))
           (check-equal-type! (pair-te->first-te te1) (pair-te->first-te te2) exp)
           (check-equal-type! (pair-te->second-te te1) (pair-te->second-te te2) exp))
          ((and (symbol-te? te1) (symbol-te? te2)) 'OK)
          (else (error "Bad type expression " te1 te2 exp)))))

;; Purpose: check that a type variable matches a type expression
;; Exp is only passed for documentation purposes.
;; Signature: check-tvar-equal-type(tvar, te, exp)
;; Type: [Tvar * Texp * Exp -> Symbol]
;; Pre-conditions: Tvar is not bound
(define check-tvar-equal-type!
  (lambda (tvar te exp)
    (if (tvar-non-empty? tvar)
        (check-equal-type! (tvar->contents tvar) te exp)
        (let ((v1 (check-no-occurrence! tvar te exp)))
          (tvar-set-contents! tvar te)
          'tvar-set-ok))))

;; Purpose: when attempting to bind tvar to te - check whether tvar occurs in te.
;; Throws error if a circular reference is found.
;; Exp is only passed for documentation purposes.
;; Signature: check-no-occurrence!(tvar, te, exp)
;; Type: [Tvar * Texp * Exp -> Symbol]
;; Pre-conditions: Tvar is not bound
(define check-no-occurrence!
  (lambda (tvar te exp)
    (letrec ((loop (lambda (te1)
                     (cond ((atomic-te? te1) #t)
                           ((proc-te? te1)
                            (for-each loop (proc-te->param-tes te1))
                            (loop (proc-te->return-te te1))
                            'proc-te-ok)
                           ((tvar? te1)
                            (if (eq? (tvar->var te1) (tvar->var tvar))
                                (error "Occurrence check error - circular unification" tvar te exp)
                                'tvar-ok))
                           (else (error "Bad type expression" te exp))))))
      (loop te))))


;; Compute the type of Typed-Scheme AST exps to TE
;; ===============================================
;; Compute a Typed-Scheme AST exp to a Texp on the basis of its structure and the annotations it contains.

;; Purpose: Compute the type of a concrete fully-typed expression
;; Signature: typeof(exp)
;; Type: [Sexp -> Concrete-texp]
(define typeof
  (lambda (concrete-exp)
    (let ((exp (parseL5 concrete-exp)))
      (unparse-texp (typeof-exp exp (make-empty-tenv))))))

;; Purpose: Compute the type of an expression
;; Signature: typeof=exp(exp, tenv)
;; Type: [Exp * Tenv -> TExp]
;; Traverse the AST and check the type according to the exp type.
(define typeof-exp
  (lambda (exp tenv)
    (cond ((num-exp? exp)    (typeof-num-exp exp))
          ((bool-exp? exp)   (typeof-bool-exp exp))
          ((prim-op? exp)    (typeof-prim-op exp))
          ((var-ref? exp)    (apply-tenv tenv (var-ref->var exp)))
          ((lit-exp? exp)    (typeof-quote-exp exp tenv))
          ((def-exp? exp)    (typeof-defexp exp tenv))
          ((if-exp? exp)     (typeof-if-exp exp tenv))
          ((proc-exp? exp)   (typeof-proc-exp exp tenv))
          ((let-exp? exp)    (typeof-let-exp exp tenv))
          ((letrec-exp? exp) (typeof-letrec-exp exp tenv))
          ((app-exp? exp)    (typeof-app-exp exp tenv))
          ;; lit-exp skipped
          (else (error "Unknown exp type" exp)))))

;; Purpose: COmpute the type of a sequence of expressions
;; Signature: typeof-exps(exps, tenv)
;; Type: [List(Cexp) * Tenv -> Texp]
;; Check all the exps in a sequence - return type of last.
;; Pre-conditions: exps is not empty.
(define typeof-exps
  (lambda (exps tenv)
   
    (cond ((empty? (cdr exps)) (typeof-exp (car exps) tenv))
          (else (let ((check-first (typeof-exp (car exps) tenv)))
                  (typeof-exps (cdr exps) tenv))))))
    
;; a number literal has type num-te
;; Type: [Num-exp -> Num-te]
(define typeof-num-exp
  (lambda (num) (make-num-te)))


;; a boolean literal has type bool-te
;; Type: [Bool-exp -> Bool-te]
(define typeof-bool-exp
  (lambda (bool) (make-bool-te)))

;; Purpose: compute the type of an quote-exp 
;; Signature: typeof-quote-exp(quote-exp, tenv)
;; Type: [quote-exp -> Texp]
;; Typing rule:
;;   if typeof<quote-exp> = (lit-exp T1)
;;   then type<quote-exp> = (lit-exp-te T1)
;(make-pair-te (typeof-exp (parseL5 (car (second quote-exp))) (make-empty-tenv)) (typeof-exp (parseL5 (cdr (second quote-exp))) (make-empty-tenv)))

(define typeof-quote-exp
  (lambda (quote-exp tenv)
   
    (if (pair? (second quote-exp))
        (if (and (symbol? (car (second quote-exp))) (symbol? (cdr (second quote-exp))))
        (make-pair-te (make-symbol-te (car (second quote-exp))) (make-symbol-te (cdr (second quote-exp))))

        (if (symbol? (car (second quote-exp)))
        (make-pair-te (make-symbol-te (car (second quote-exp))) (typeof-exp (parseL5 (cdr (second quote-exp))) (make-empty-tenv)))

        (if (symbol? (cdr (second quote-exp)))
        (make-pair-te (typeof-exp (parseL5 (car (second quote-exp))) (make-empty-tenv)) (make-symbol-te (cdr (second quote-exp))))
        (make-pair-te (typeof-exp (parseL5 (car (second quote-exp))) (make-empty-tenv)) (typeof-exp (parseL5 (cdr (second quote-exp))) (make-empty-tenv)))
        )
        )  
        )
    (if (var-ref? (parseL5 (second quote-exp)))
       (make-symbol-te (second quote-exp))
       (typeof-exp (parseL5 (second quote-exp)) tenv))
       )
       ))


;(symbol? 'x)
;(parseL5 'x)
;; primitive ops have known proc-te types
;; Type: [Prim-op -> Proc-te]
(define typeof-prim-op
  (lambda (x)
   
    (let ((num-op-te      (parse-texp '(number * number -> number)))
          (num-comp-op-te (parse-texp '(number * number -> boolean)))
          (bool-op-te     (parse-texp '(boolean -> boolean)))
          ; (cons-op-te     '(proc-te (num-te num-te) (pair-te num-te num-te)))
          ;(cons-op-te     '(T1 * T2 -> (pair-te T1 T2)))
          (cons-op-te     (parse-texp '(T1 * T2 -> (Pair T1 T2))))
          (car-op-te     (parse-texp '((Pair T1 T2) -> T1)))
          (cdr-op-te    (parse-texp '((Pair T1 T2) -> T2)))
          (x (prim-op->op x)))
      (cond ((eq? x '+) num-op-te)
            ((eq? x '-) num-op-te)
            ((eq? x '*) num-op-te)
            ((eq? x '/) num-op-te)
            ((eq? x '<) num-comp-op-te)
            ((eq? x '>) num-comp-op-te)
            ((eq? x '=) num-comp-op-te)
            ((eq? x 'not) bool-op-te)
            ((eq? x 'cons) cons-op-te)
            ((eq? x 'car) car-op-te)
            ((eq? x 'cdr) cdr-op-te)
            ))))

;(typeof-prim-op '(prim-op +))
;(typeof-prim-op '(prim-op cons))
;(typeof-prim-op '(prim-op car))
;(typeof-prim-op '(prim-op cdr))

;; Purpose: compute the type of an def-exp 
;; Signature: typeof-def-exp(def-exp, tenv)
;; Type: [def-exp * Tenv -> Texp]
;; Typing rule:
;;   if typeof<defexp->var>(tenv) = T1
;;      typeof<defexp->val>(tenv) = T1
;;   then type<(defexp)>(tenv) = void
(define typeof-defexp
  (lambda (defexp tenv)
     
    (let ((var-te (var-decl->texp (def-exp->var defexp))) 
          (val-te (typeof-exp (def-exp->val defexp) tenv)))
      (check-equal-type! var-te val-te defexp)
      (make-void-te))))


;; Purpose: compute the type of an if-exp
;; Signature: typeof-if-exp(ifexp, tenv)
;; Type: [If-exp * Tenv -> Texp]
;; Typing rule:
;;   if type<test>(tenv) = boolean
;;      type<then>(tenv) = t1
;;      type<else>(tenv) = t1
;; then type<(if test then else)>(tenv) = t1
(define typeof-if-exp
  (lambda (ifexp tenv)
    (let ((test-te (typeof-exp (if-exp->test ifexp) tenv))
          (then-te (typeof-exp (if-exp->then ifexp) tenv))
          (else-te (typeof-exp (if-exp->else ifexp) tenv)))
      (check-equal-type! test-te (make-bool-te) ifexp)
      (check-equal-type! then-te else-te ifexp)
      then-te)))

;; Purpose: compute the type of a proc-exp
;; Signature: typeof-proc-exp(proc, tenv)
;; Type: [proc-exp * Tenv -> Texp]
;; Typing rule:
;; If   type<body>(extend-tenv(x1=t1,...,xn=tn; tenv)) = t
;; then type<lambda (x1:t1,...,xn:tn) : t exp)>(tenv) = (t1 * ... * tn -> t)
(define typeof-proc-exp
  (lambda (proc tenv)
    (let ((params (proc-exp->params proc))
          (body (proc-exp->body proc)))
      (let ((params-tes (map var-decl->texp params))
            (return-te (proc-exp->return-te proc)))
       
        (check-equal-type!
         (typeof-exps body (extend-tenv tenv (map var-decl->var params) params-tes))
         return-te
         proc)
        (make-proc-te params-tes return-te)))))


;; Purpose: compute the type of an app-exp
;; Signature: typeof-app-exp(app, tenv)
;; Type: [app-exp * Tenv -> Texp]
;; Typing rule:
;; If   type<rator>(tenv) = (t1*..*tn -> t)
;;      type<rand1>(tenv) = t1
;;      ...
;;      type<randn>(tenv) = tn
;; then type<(rator rand1...randn)>(tenv) = t
;; We also check the correct number of arguments is passed.
(define typeof-app-exp
  (lambda (app tenv)
    (let ((rator (app-exp->rator app))
          (rands (app-exp->rands app)))
      (let ((rator-te (typeof-exp rator tenv)))
   
        (if (and (list? rator) (prim-op? rator) (eq? (prim-op->op rator) 'cons))
            (let ((updated-rator-te (make-proc-te (list (typeof-exp (first rands) tenv) (typeof-exp (second rands) tenv))
                                                  (make-pair-te (typeof-exp (first rands) tenv) (typeof-exp (second rands) tenv)))))    
              
              (if (not (= (length rands)
                          (length (proc-te->param-tes updated-rator-te))))
                  (error "Wrong number of arguments passed to procedure"
                         (unparse app))
                  'number-or-args-ok)
               
              (for-each (lambda (rand-i t-i)
                    
                          (check-equal-type! (typeof-exp rand-i tenv) t-i app))
                        rands
                        (proc-te->param-tes updated-rator-te))
              (proc-te->return-te updated-rator-te))
            (if (and (list? rator) (prim-op? rator) (eq? (prim-op->op rator) 'car))                
                (if (not (pair-te? (typeof-exp (first rands) tenv)))
                          (error "Not a pair!"
                                 (first rands))
                      (pair-te->first-te (typeof-exp (first rands) tenv)))             
                  (if (and (list? rator) (prim-op? rator) (eq? (prim-op->op rator) 'cdr))
                    (if (not (pair-te? (typeof-exp (first rands) tenv)))
                          (error "Not a pair!"
                                 (first rands))
                      (pair-te->second-te (typeof-exp (first rands) tenv)))
                    (let ((rator-te (typeof-exp rator tenv)))
                 
                      (if (not (= (length rands)
                                  (length (proc-te->param-tes rator-te))))
                          (error "Wrong number of arguments passed to procedure"
                                 (unparse app))
                          'number-or-args-ok)
                      (for-each (lambda (rand-i t-i)
                                  (check-equal-type! (typeof-exp rand-i tenv) t-i app))
                                rands
                                (proc-te->param-tes rator-te))
                      (proc-te->return-te rator-te))))
            )))))

;; Purpose: compute the type of a let-exp
;; Signature: typeof-let-exp(letexp, tenv)
;; Type: [Let-exp * Tenv -> Texp]
;; Typing rule:
;; If   type<val1>(tenv) = t1
;;      ...
;;      type<valn>(tenv) = tn
;;      type<body>(extend-tenv(var1=t1,..,varn=tn; tenv)) = t
;; then type<let ((var1 val1) .. (varn valn)) body>(tenv) = t
(define typeof-let-exp
  (lambda (letexp tenv)
    (let ((bdgs (let-exp->bindings letexp))
          (body (let-exp->body letexp)))
      (let ((vars (map binding->var bdgs))
            (vals (map binding->val bdgs)))
        (let ((vars (map var-decl->var vars))
              (var-tes (map var-decl->texp vars)))
          
          (for-each (lambda (var-te val)
                      (check-equal-type! var-te (typeof-exp val tenv) letexp))
                    var-tes vals)
          (typeof-exps body (extend-tenv tenv vars var-tes)))))))

;; Purpose: compute the type of a letrec-exp
;; Signature: typeof-letrec-exp(letrecexp, tenv)
;; Type: [Letrec-exp * Tenv -> Texp]
;; We make the same assumption as in L4 that letrec only binds proc values.
;; Typing rule:
;;   (letrec((p1 (lambda (x11 ... x1n1) body1)) ...) body)
;;   tenv-body = extend-tenv(p1=(t11*..*t1n1->t1)....; tenv)
;;   tenvi = extend-tenv(xi1=ti1,..,xini=tini; tenv-body)
;; If   type<body1>(tenv1) = t1
;;      ...
;;      type<bodyn>(tenvn) = tn
;;      type<body>(tenv-body) = t
;; then type<(letrec((p1 (lambda (x11 ... x1n1) body1)) ...) body)>(tenv-body) = t
(define typeof-letrec-exp
  (lambda (letrecexp tenv)
    (let ((bdgs (letrec-exp->bindings letrecexp))
          (body (letrec-exp->body letrecexp)))
      (let ((ps (map (lambda (b) (var-decl->var (binding->var b))) bdgs))
            (procs (map binding->val bdgs)))
        (let ((paramss (map proc-exp->params procs))
              (bodies (map proc-exp->body procs)))
          (let ((tijs (map (lambda (params) (map var-decl->texp params)) paramss))
                (tis (map proc-exp->return-te procs)))
            (let ((tenv-body (extend-tenv
                              tenv
                              ps
                              (map (lambda (tij ti) (make-proc-te tij ti))
                                   tijs tis))))
              (let ((tenv-is (map (lambda (params tij)
                                    (extend-tenv tenv-body (map var-decl->var params) tij))
                                  paramss tijs)))
                
                (for-each (lambda (body-i ti tenv-i)
                          
                          (check-equal-type! (typeof-exps body-i tenv-i) ti letrecexp))
                          bodies
                          tis
                          tenv-is)                
                (typeof-exps body tenv-body)))))))))

;(typeof-exp (parseL5 '(quote x)) (make-empty-tenv))
#;(typeof-exp (parseL5 '(lambda ([x : symbol]) : symbol (quote x))) (make-empty-tenv))
#;(unparse-texp (typeof-exp (parseL5 '(lambda ([x : symbol]) : symbol (quote x))) (make-empty-tenv)))

;(symbol-te? (make-symbol-te 5))

#;(check-equal-type!
         (make-symbol-te 5)
         (make-symbol-te 5)
         '(proc-exp ((var-decl x symbol-te)) ((lit-exp x)) symbol-te))
 