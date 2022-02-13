#lang racket
;;;;CSDS 345
;;;; Quyen Huynh
;;;; Tammy Lin
;;;; Elizabeth Waters
;;;; Part 1 

(require "simpleParser.rkt")
(require  "lex.rkt")

;; take in the filename 
(define intepreter
  (lambda (filename)
    (intepreterRule (parse filename) empty-lis)))

(define intepreterRule
  (lambda (expression state)
    (cond
    ((null? expression) '())
    ((number? expression) expression)
    (else (intepreterRule (the-rest expression) (Mstate (the-head expression)))))))

;; take in an expression and a state -> return the type updated of the expression 
(define Mstate
  (lambda (expression state)
    ((null? expression) expression)
    ((eq? (operator expression) 'var) (declare expression state));make a func. for declare
    ((eq? (operator expression) '=) (assign expression state));; call assign
    ((eq? (operator expression) 'while) (while-loop expression state)) ;; call while
    ((eq? (operator expression) 'return) (return expression state));; call return 
    ((eq? (operator expression) 'if) (if-loop expression state))
    (else 'Mtype "Not Valid Type")))

;; return the value for the expression 
(define Mvalue
  (lambda (expression state)
    (cond
      ((number? expression) (expression))
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
      ((if (eq? (isVariable? expression state) #t)) Mvalue (retrieveValue expression state) state)  ;; if it's not a variable -> then retrieve its value ;;change this because it wasnt recursive  
      ((eq? (operator expression) '+) (+ (Mvalue (leftoperand expression)state) (Mvalue (rightoperand expression)state)))
      ((eq? (operator expression) '-) (- (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression))))
      ((eq? (operator expression) '*) (* (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression))))
      ((eq? (operator expression) '/) (quotient (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression))))
      ((eq? (operator expression) '%) (remainder (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression))))
      (else (error 'badop "Bad operator")))))


((var x) (= x 10) (var y (+ (* 3 x) 5)) (while (!= (% y x) 3) (= y (+ y 1))) (if (> x y) (return x) (if (> (* x x) y) (return (* x x)) (if (> (* x (+ x x)) y) (return (* x (+ x x))) (return (- y 1))))))

;;declare
(define declare
  (lambda (lis state)
    (cond
      ((null? lis) '()) ;;invalid expression cant be declared 
      ((eq? (check-declare expression state) #t) (error 'Mstate "Already declared"))
      ((eq? (check-declare expression state) #f) (add-bind(lis (Mvalue (cddr lis) state) state)))
      (else (error 'declare "No Value")))))

;; check if a varible is ion the state alr -> if yes then you want remove-binding and then add-binding ELSE if its not in the there then error ELSE check M-value    

;;boolean
(define Mbooelan
  (lambda (if-cond state)
    ((null? if-cond) ( error 'Mboolean "Invalid Statement"))
      ((eq? (operator if-cond) '<)   (< (Mvalue (operand1 if-cond) state) (Mvalue (operand2 if-cond) state)))
      ((eq? (operator if-cond) '>)   (> (Mvalue (operand1 if-cond) state) (Mvalue (operand2 if-cond) state)))
      ((eq? (operator if-cond) '<=)  (<= (Mvalue (operand1 if-cond) state) (Mvalue (operand2 if-cond) state)))
      ((eq? (operator if-cond) '>=)  (>= (Mvalue (operand1 if-cond) state) (MValue (operand2 if-cond) state)))
      ((eq? (operator if-cond) '==)  (eq? (Mvalue (operand1 if-cond) state) (Mvalue (operand2 if-cond) state)))
      ((eq? (operator if-cond) '!=)  (not (eq? (Mvalue (operand1 if-cond) state) (Mvalue (operand2 if-cond) state))))
      ((eq? (operator if-cond) '||)  (or (Mvalue (operand1 if-cond) state) (Mvalue (operand2 if-cond) state)))
      ((eq? (operator if-cond) '&&)  (and (Mvalue (operand1 if-cond) state) (Mvalue (operand2 if-cond) state)))
      ((eq? (operator if-cond) '!)   (not (Mvalue (operand1 if-cond) state))))) ;; what happen if it's a list like (!= (% y x) 3)


;; assign
(define assign
  (lambda ( expression state)
    (cond
      ((null? (cdr expression)) (error 'assign "cant assign"))
      ((eq? (check-declare expression state) #t) (add-bind( (cons 'var (cadr exp)) (Mvalue(cddr expression) state) (removebind (cadr exp)))))
      (else (error 'assign "expression has not declared")))))

;; removebind 
(define removebind
  (lambda (name state)
    (cond
      [(null? state) '()]
      [(eq? name (first-state-var(s)) (cdr state))]
      [else (cons (car state) (removebind(cdr state)))])))


;;if-loop
(define if-loop
  (lambda (lis state)
    (cond
      ((null? lis) (error 'if-loop "input expression is null"))
      ((Mboolean (car(cdr lis)) state) (Mstate (first-satement lis) state))
      ((not(Mboolean (car(cdr lis)) state)) (Mstate ( second-statement lis) state))
      (else (error 'if-loop "invalid statement"))))) ;; any other condition to check 

;; while-loop
(define while-loop
  (lambda (lis state)
    (cond
      ((null? lis) (error 'while-loop "invvalid while-loop"))
      ((Mboolean (car(cdr lis)) state) ()))) ;; need to finish 

;;return 





    
    


    
;; ********** helper ********* ;;
(define check-declare
  (lambda (lis state)
    (cond
      ((null? state) #f)
      ((eq? first-state-var input-name) #t)
      (else (check-declare(lis next-s))))))

(define add-bind
  (lambda (lis value state)
    (cons (format (car lis) (caar lis) value )state)))  ;; format the input - name type value

(define format
  (lambda (lis value)
    (append(append (car lis) (car (car lis)) ) value))) ;; we are appending it value twice here and add-bind 

 (define retrieveValue
   (lambda (name state)
     (cond
       ((null? state) (error 'retrieveValue "Error"))
       ((eq? name first-state-var(state)) (cddr (first-state-var(state))))
       (else (retrieveValue (name next-s))))))


;;************ Abstraction ************** ;;
(define operator
  (lambda (exp)
    (car exp)))

(define next-s
  (lambda (state)
    (cdr state)))

(define leftoperand cadr)

(define rightoperand caddr)

(define the-rest cdr)
(define the-head car)
(define empty-lis '())

(define input-name
  (lamnda (lis)
          (car(cdr lis))))

(define first-state-var
  (lambda (state)
    (cadr (car lis)))) ;; in order to change it, we can do caar of the state here to put it in (var value state) 

(define the-hhead cadr) ;; the front of the front of the state

(define isVariable
  (lambda (name state)
    (cond
      ((null? state) #f)
      ((eq? name first-state-var(state)) #t)
      (else (isVariable name next-s)))))

(define first-statement
  (lambda (lis)
    (car(cdr(lis)))))

(define second-statement
  (lambda (lis)
    (car(cddr(lis)))))


    

