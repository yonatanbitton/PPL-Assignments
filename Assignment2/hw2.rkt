#lang racket
(require "parser.rkt")
(provide (all-defined-out))

; Purpose: Produce a list of lists of length window each with the elements slided by factor window 
; Signature: sliding-window(list,window)
; Type: [List * Number -> List]
(define sliding-window
      (lambda (lst window)
          (if (> window (length lst))
          '()
           (append
                (list (take lst window) )  
                (sliding-window (cdr lst) window)
                )          
          )
      )
  )

; Purpose: Compute the greatest node data value in the tree
; Signature: greatest-node(tree)
; Type: [List(Number) -> Number]
(define greatest-node
  (lambda (tree)
    (if (empty? tree)
        -1
        (if (integer? tree)
            tree
            (max (car tree) (greatest-node (second tree)) (greatest-node (third tree))               
                 )
            )
        )
    )
  )


; Purpose: Compute the number of nodes whose data value is equal to x
; Signature: count-node(tree,x)
; Type: [List * T -> Number]
(define count-node
  (lambda (tree x)
    (if (empty? tree)
        0        
        (if (list? tree)
            (if (eq? (car tree) x)
                (+ 1 (count-node (cdr tree) x))
                (+ (count-node (car tree) x) (count-node (cdr tree) x))
                )
            (if (eq? tree x)
                1
                0
                )
            )
        
        )
    )
  )


; Purpose: Compute the mirrored tree
; Signature: mirror-tree(tree)
; Type: [List -> List]
(define mirror-tree
  (lambda (tree)
     (if (empty? tree)
      tree
          (if (list? tree)
              (if (= (length tree) 1)
                  (mirror-tree (car tree)) ;else, list of length 3
                  (list (mirror-tree (first tree)) (mirror-tree (third tree)) (mirror-tree (second tree)))
              )
              tree
          )
    )
  )
)


; Purpose: unparse a Scheme AST into Javascript syntax without considering infix notation
; Signature: unparse->js(ast,output-port)
; Type: [Exp * Output-Port -> Void]
(define unparse->js
  (lambda (ast output-port)
    (cond ((def-exp? ast)
           ;(fprintf (current-output-port) "Eat my shorts")
           (fprintf output-port "const ~a = " (unparse (def-exp->var ast)))
           (unparse->js (def-exp->val ast) output-port)
           (fprintf output-port ";") 
           )
          ((cexp? ast)
           (cond ((num-exp? ast)                                  
                                   (fprintf output-port "~a" (num-exp->val ast))                                  
                                   )
                  
                 ((bool-exp? ast) (if (eq? (bool-exp->val ast) '#t)
                                   
                                   (fprintf output-port "true")
                                   (fprintf output-port "false"))                                  
                                   )
                 ((str-exp? ast)  (fprintf output-port "~s" (str-exp->val ast)))
                 ((var-exp? ast)  (
                                   if (eq? (var-exp->var ast) '=)
                                   (fprintf output-port "==")                                     
                                   (fprintf output-port "~a" (var-exp->var ast)) 
                                                ))
                 ((literal-exp? ast)
                  (fprintf output-port "~a" (list 'quote (literal-exp->val ast))) 
                                     )
                 ((proc-exp? ast)



                 (cond ((> (length (proc-exp->body ast)) 0)                                
                        (map (lambda (b) 
                               (if (eq? (car (proc-exp->params ast)) b)
                                   (fprintf output-port "(")
                                   'NOFIRST ;*Shahaf - (void)
                                   )
                               (unparse->js b output-port)
                               (if (eq? (last (proc-exp->params ast)) b)
                                   (fprintf output-port ") => { ")
                                   (fprintf output-port ",") 
                                   )
                               ) (proc-exp->params ast))
                        (map (lambda (b)                               
                               (unparse->js b output-port)
                               (if (eq? (last (proc-exp->body ast)) b)
                                   (fprintf output-port " }")
                                   (fprintf output-port "; ") 
                                   )
                               ) (proc-exp->body ast))
                        (display "")
                                   )
                  
                 ((= (length (proc-exp->body ast)) 0) 
                  'PROCCALL
                  'OKMASHU
                  (fprintf output-port "true")
                  )))                           
                 ((if-exp? ast)
                  (unparse->js (if-exp->test ast) output-port)
                  (fprintf output-port " ? ")
                  (unparse->js (if-exp->then ast) output-port)
                  (fprintf output-port " : ")
                  (unparse->js (if-exp->else ast) output-port)
                  )
                 ((let-exp? ast)
                                    ;(display  (let-exp->bindings ast))
                                     (fprintf output-port "let ")
                                    (map (lambda (b)
                                           (unparse->js (binding->var b) output-port)
                                           (fprintf output-port " = ")
                                           (unparse->js (binding->val b) output-port)
                                           (if (eq? (last (let-exp->bindings ast)) b)
                                               'OK
                                               (fprintf output-port ", "))                                                              
                                           )
                                         (let-exp->bindings ast)
                                         )
                                    (fprintf output-port "; " )                                  
                                     ;(display  (let-exp->body ast))
                                     (map (lambda (b)
                                            (unparse->js b output-port)
                                            (if (eq? (last (let-exp->body ast)) b)
                                               (fprintf output-port ";")
                                               (fprintf output-port "; "))                                              
                                            )
                                          (let-exp->body ast)
                                          )
                                     (display "")
                                    ;(fprintf (current-output-port) "~a" (map unparse->js (let-exp->body ast) output-port))      
                                      )
                 ((app-exp? ast)                               
                                 (if (eq? (second (app-exp->rator ast)) '=)
                                 (fprintf output-port "==" )
                                 (fprintf output-port "~a" (second (app-exp->rator ast))))
                                 (fprintf output-port "(" ) 
                                 ; we want to activate unparse->js on each one of the rands expressions
                                 (map (lambda (b)                                        
                                        (unparse->js b output-port)
                                        (if (eq? (last (app-exp->rands ast)) b)
                                        'OK
                                        (fprintf output-port "," )) 
                                        )
                                      (app-exp->rands ast))
                                 (fprintf output-port ")" ) 
                                   ;    (map(unparse->js (app-exp->rands ast)  current-output-port)))
                  )
                 (else (error "Unknown exp type: " ast))))
          (else (error "Unknown exp type: " ast)))
    
   )
)

; Purpose: unparse a Scheme AST into Javascript syntax while considering infix notation
; Signature: unparse->js-infix(ast,output-port)
; Type: [Exp * Output-Port -> Void]

(define unparse->js-infix
  (lambda (ast output-port)
    (cond ((def-exp? ast)
           ;(fprintf (current-output-port) "Eat my shorts")
           (fprintf output-port "const ~a = " (unparse (def-exp->var ast)))
           (unparse->js-infix (def-exp->val ast) output-port)
           (fprintf output-port ";") 
           )
          ((cexp? ast)
           (cond ((num-exp? ast)                                  
                                   (fprintf output-port "~a" (num-exp->val ast))                                  
                                   )
                  
                 ((bool-exp? ast) (if (eq? (bool-exp->val ast) '#t)
                                   
                                   (fprintf output-port "true")
                                   (fprintf output-port "false"))                                  
                                   )
                 ((str-exp? ast)  (fprintf output-port "~s" (str-exp->val ast)))
                 ((var-exp? ast)  (
                                   if (eq? (var-exp->var ast) '=)
                                   (fprintf output-port "==")                                     
                                   (fprintf output-port "~a" (var-exp->var ast)) 
                                                ))
                 ((literal-exp? ast)
                  (fprintf output-port "~a" (list 'quote (literal-exp->val ast))) 
                                     )
                 ((proc-exp? ast)



                 (cond ((> (length (proc-exp->body ast)) 0)                                
                        (map (lambda (b) 
                               (if (eq? (car (proc-exp->params ast)) b)
                                   (fprintf output-port "(")
                                   'NOFIRST
                                   )
                               (unparse->js-infix b output-port)
                               (if (eq? (last (proc-exp->params ast)) b)
                                   (fprintf output-port ") => { ")
                                   (fprintf output-port ",") 
                                   )
                               ) (proc-exp->params ast))
                        (map (lambda (b)                               
                               (unparse->js-infix b output-port)
                               (if (eq? (last (proc-exp->body ast)) b)
                                   (fprintf output-port " }")
                                   (fprintf output-port "; ") 
                                   )
                               ) (proc-exp->body ast))
                        (display "")
                                   )
                  
                 ((= (length (proc-exp->body ast)) 0) 
                  'PROCCALL
                  'OKMASHU
                  (fprintf output-port "true")
                  )))                           
                 ((if-exp? ast)
                  (unparse->js-infix (if-exp->test ast) output-port)
                  (fprintf output-port " ? ")
                  (unparse->js-infix (if-exp->then ast) output-port)
                  (fprintf output-port " : ")
                  (unparse->js-infix (if-exp->else ast) output-port)
                  )
                 ((let-exp? ast)
                                    ;(display  (let-exp->bindings ast))
                                    (fprintf output-port "let ")
                                    (map (lambda (b)
                                           (unparse->js-infix (binding->var b) output-port)
                                           (fprintf output-port " = ")
                                           (unparse->js-infix (binding->val b) output-port)
                                           (if (eq? (last (let-exp->bindings ast)) b)
                                               'OK
                                               (fprintf output-port ", "))                                                              
                                           )
                                         (let-exp->bindings ast)
                                         )
                                    (fprintf output-port "; " )                                  
                                     ;(display  (let-exp->body ast))
                                     (map (lambda (b)
                                            (unparse->js-infix b output-port)
                                            (if (eq? (last (let-exp->body ast)) b)
                                               (fprintf output-port ";")
                                               (fprintf output-port "; "))    
                                            )
                                          (let-exp->body ast)
                                          )
                                     (display "")
                                    ;(fprintf (current-output-port) "~a" (map unparse->js (let-exp->body ast) output-port))      
                                      )
                 ((app-exp? ast)                
                  (cond ((or (equal? (app-exp->rator ast) (make-var-exp '+))
                                                 (equal? (app-exp->rator ast) (make-var-exp '-))
                                                 (equal? (app-exp->rator ast) (make-var-exp '*))
                                                 (equal? (app-exp->rator ast) (make-var-exp '=))
                                                 (equal? (app-exp->rator ast) (make-var-exp '/)))                                                       
                         (map (lambda (b)                               
                                (if (eq? (first (app-exp->rands ast)) b)
                                    (fprintf output-port "(")
                                    'NotFirst)
                                (unparse->js-infix b output-port)                              
                                (if (eq? (last (app-exp->rands ast)) b)
                                    (fprintf output-port ")")
                                    (if (or (equal? (app-exp->rator ast) (make-var-exp '+))
                                                 (equal? (app-exp->rator ast) (make-var-exp '-))
                                                 (equal? (app-exp->rator ast) (make-var-exp '*))
                                                 (equal? (app-exp->rator ast) (make-var-exp '=))
                                                 (equal? (app-exp->rator ast) (make-var-exp '/))) 
                                        (
                                         if (eq? (second (app-exp->rator ast)) '=)
                                            (fprintf output-port " == ")                                     
                                            (fprintf output-port " ~a " (second (app-exp->rator ast)))
                                            )                                                                           
                                        (unparse->js-infix ast output-port)
                                        )
                                    )
                                )
                              (app-exp->rands ast)
                              )
                          (display "")
                         )                  
                        (else
                         'NotBasicOps                       
                                 (if (eq? (second (app-exp->rator ast)) '=)
                                 (fprintf output-port "==" )
                                 (fprintf output-port "~a" (second (app-exp->rator ast))))
                                 (fprintf output-port "(" ) 
                                 ; we want to activate unparse->js on each one of the rands expressions
                                 (map (lambda (b)                                        
                                        (unparse->js-infix b output-port)
                                        (if (eq? (last (app-exp->rands ast)) b)
                                        'OK
                                        (fprintf output-port "," )) 
                                        )
                                      (app-exp->rands ast))
                                 (fprintf output-port ")" )
                                         )
                       
                        )
                                  
                                   ;    (map(unparse->js (app-exp->rands ast)  current-output-port)))
                  )
                 (else (error "Unknown exp type: " ast))))
          (else (error "Unknown exp type: " ast)))
    
   )
)
