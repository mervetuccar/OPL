;;;Merve Tuccar
#lang r5rs

;;;Problem 2: Exercise 4.9 on p. 376. Pick one of the iteration constructs 
;;;(do, for, while or until) to implement.

;;;In the following part, do-while? is added in cond as the part of case analysis, see the line with stars. 
;;;And the right below of this part, the uml:do-while special form part is implemented. 
;;;You can run the commented out examples to test the code with the explanation of how the code works,
;;;please find them below, of the uml:do-while implementation. 

(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((do-while? exp) (eval-do-while exp env)) ;;do-WHILE ADDED HERE***********************************************
	((and? exp) (eval-and exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
	((let? exp) (mc-eval (let->combination exp) env))
        ((application? exp)
         (mc-apply (mc-eval (operator exp) env)
		(list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- MC-EVAL"))))



;;; Procdures to implement uml:eval-do-while**************************************************************************
;;; do-While evaluates a given statement at least once, and continues executing until the given predicate returns false. 
;;; The following code evaluates the body and returns false when the condition is met.
;;; Please see the example outputs at the end.

(define (do-while? exp)
  (tagged-list? exp 'uml:do-while))

(define (do-while-predicate exp) (cadr exp))

(define (eval-do-while exp env)
  (define (do-while-body body)
    (let ((eval (mc-eval (list body) env)))
      (if (eq? eval #t) (do-while-body body)
          eval)))
    (do-while-body (do-while-predicate exp)))


;;; MC-Eval input: (uml:define x 2)
;;; MC-Eval value: ok

;;; MC-Eval input: (uml:do-while (uml:lambda () (uml:begin (uml:set! x (uml:+ x 2)) (uml:< x 14))))
;;; MC-Eval value: #f


;;; MC-Eval input: x
;;; MC-Eval value: 14

