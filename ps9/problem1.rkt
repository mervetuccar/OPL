;;;Merve Tuccar
#lang r5rs
;;;Problem 1: Exercise 4.4 (“or” only) on p. 374. Remember to name your new “or” 
;;;as “uml:or”. Make sure to implement short-circuiting!

;;;In the following part, or? is added in cond as the part of case analysis, see the line with stars. 
;;;And the right below of this part, the uml:or special form part is implemented. 
;;;You can run the commented out examples to test the code with the explanation of how the code works,
;;;please find them below, of the uml:or implementation. 

(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
	((and? exp) (eval-and exp env))
        ((or? exp) (eval-or (cdr exp) env))  ;;OR ADDED HERE**************************************************     
        ((lambda? exp)
         (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
	((let? exp) (mc-eval (let->combination exp) env))
        ((application? exp)
         (mc-apply (mc-eval (operator exp) env)
		(list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- MC-EVAL"))))


;;; Procedures to implement uml:or as a special form in our new language **********************************
;;;Unlike and, if any expresiion evaluates a true value, true is returned. And remaining expresions
;;;are not evaluated. If there is no expresion to evaluate, false is returned. If all expressions
;;;are evaluated as false, false is returned (the code goes back to base case, null? state and returns false).

(define (eval-or exp env)
    (if (null? exp) #f
	(if (true? (mc-eval (car exp) env))
	    #t
	    (eval-or (cdr exp) env))))

(define (or? exp) (tagged-list? exp 'uml:or))


;;; MC-Eval input: (uml:or (uml:> 1 2) (uml:= 1 2) (uml:< 1 2))
;;; MC-Eval value: #t

;;; MC-Eval input: (uml:or (uml:> 1 2) (uml:= 1 2))
;;; MC-Eval value: #f

;;; MC-Eval input: (uml:or (uml:> 1 2))
;;; MC-Eval value: #f

;;; MC-Eval input: (uml:or (uml:> 3 2) (uml:= 2 2) (uml:< 1 2))
;;; MC-Eval value: #t
;;**********************************************************************************************************

