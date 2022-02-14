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
    (intepreterRule (parser filename) empty-lis)))

(define intepreterRule
  (lambda (expression state)
    (cond
    ((null? expression) '())
    ((number? expression) expression)
    (else (intepreterRule (the-rest expression) (Mstate (the-head expression)))))))

;; take in an expression and a state -> return the type updated of the expression 
(define Mstate
  (lambda (expression state)
    (cond
    ((null? expression) expression)
    ((eq? (operator expression) 'var) (declare expression state));make a func. for declare
    ((eq? (operator expression) '=) (assign expression state));; call assign
    ((eq? (operator expression) 'while) (while-loop expression state)) ;; call while
    ((eq? (operator expression) 'return) (return expression state));; call return 
    ((eq? (operator expression) 'if) (if-stmt expression state))
    (else (error 'Mstate "Not Valid Type")))))

;; return the value for the expression 
(define Mvalue
  (lambda (expression state)
    (cond
      ((null? expression) (error 'Mvalue "enter value invalid"))
      ((number? expression) expression)
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
      ((eq? (isVariable? expression state) #t) (Mvalue (retrieveValue expression state) state))  ;; if it's not a variable -> then retrieve its value ;;change this because it wasnt recursive  
      ((eq? (operator expression) '+) (+ (Mvalue (leftoperand expression)state) (Mvalue (rightoperand expression)state)))
      ((eq? (operator expression) '-) (- (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '*) (* (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '/) (quotient (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      ((eq? (operator expression) '%) (remainder (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))
      (else (error 'badop "Bad operator")))))


;;((var x) (= x 10) (var y (+ (* 3 x) 5)) (while (!= (% y x) 3) (= y (+ y 1))) (if (> x y) (return x) (if (> (* x x) y) (return (* x x)) (if (> (* x (+ x x)) y) (return (* x (+ x x))) (return (- y 1))))))

;;declare (var x) (var x 10)
(define declare
  (lambda (lis state)
    (cond
      ((null? lis) '()) ;;invalid expression cant be declared 
      ((eq? (check-declare lis state) #t) (error 'Mstate "Already declared"))
      ((and (eq? (check-declare lis state) #f) (null? (cddr lis))) (add-bind lis null state))
      ((eq? (check-declare lis state) #f)  (add-bind lis (Mvalue (caddr lis) state) state))
      (else (error 'declare "No Value")))))

;; check if a varible is ion the state alr -> if yes then you want remove-binding and then add-binding ELSE if its not in the there then error ELSE check M-value    

;;boolean
(define Mboolean
  (lambda (if-cond state)
    (cond 
      ((null? if-cond) (error 'Mboolean "Invalid Statement"))
      ((or (null? (leftoperand if-cond)) (null? (rightoperand if-cond))) (error 'Mboolean "Invalid stmt"))
      ((eq? (operator if-cond) '<)   (< (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state)))
      ((eq? (operator if-cond) '>)   (> (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state)))
      ((eq? (operator if-cond) '<=)  (<= (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state)))
      ((eq? (operator if-cond) '>=)  (>= (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state)))
      ((eq? (operator if-cond) '==)  (eq? (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state)))
      ((eq? (operator if-cond) '!=)  (not (eq? (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state))))
      ((eq? (operator if-cond) '||)  (or (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state)))
      ((eq? (operator if-cond) '&&)  (and (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state)))
      ((eq? (operator if-cond) '!)   (not (Mvalue (leftoperand if-cond) state)))))) ;; what happen if it's a list like (!= (% y x) 3)


;; assign
(define assign
  (lambda ( expression state)
    (cond
      ((null? (cdr expression)) (error 'assign "cant assign"))
      ((eq? (check-declare expression state) #t) (add-bind(cons 'var (cons (cadr expression) '())) (Mvalue(caddr expression) state) (removebind (cadr expression) state)))
      (else (error 'assign "expression has not declared")))))

;; removebind 
(define removebind
  (lambda (name state)
    (cond
      [(null? state) '()]
      [(eq? name (first-state-var state)) (cdr state)]
      [else (cons (car state) (removebind(cdr state)))])))


;;if-stmt caddr
(define if-stmt
  (lambda (lis state)
    (cond
      ((null? lis) (error 'if-stmt "input expression is null"))
      ((Mboolean (car(cdr lis)) state) (Mstate (car(cdr (cdr lis))) state)) ;; check condition
      (else (if-stmt (cadddr lis) state)))))

      
;; while-loop
(define while-loop
  (lambda (lis state)
    (cond
      ((null? lis) (error 'while-loop "invalid while-loop"))
      ((Mboolean (car(cdr lis)) state) (Mstate lis (Mstate(caddr lis) state)))
      ((not (Mboolean (cadr lis) state)) state)))) ;; need to finish


;;return
(define return
  (lambda (lis state)
    (cond
      ((null? (cdr lis)) lis)
      ((eq? (Mboolean (cadr lis) state) #t) (return-add-bind #t state))
      ((eq? (Mboolean (cadr lis) state) #f) (return-add-bind #f state))
      (else (return-add-bind (Mvalue (cadr lis) state) state)))))






    
    


    
;; ********** helper ********* ;;
(define check-declare
  (lambda (lis state)
    (cond
      ((null? state) #f)
      ((eq? (first-state-var state) (cadr lis)) #t)
      (else (check-declare lis (next-s state ))))))

(define add-bind
  (lambda (lis value state)
    (cons (append (cons (car lis) (cons (cadr lis) '())) (cons value '())) state)))  ;; format the input - name type value

 (define retrieveValue
   (lambda (name state)
     (cond
       ((null? state) (error 'retrieveValue "Error"))
       ((eq? name (first-state-var state)) (car(cdr(cdr(car state)))))
       (else (retrieveValue name (next-s state))))))


(define return-add-bind
  (lambda (value state)
    (cons (cons 'return (cons value '())) state)))


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

; lis: (var x value)
(define input-name
  (lambda (lis)
          (car(cdr lis))))

(define first-state-var
  (lambda (state)
    (cadr (car state))));; in order to change it, we can do caar of the state here to put it in (var value state) 

(define the-hhead cadr) ;; the front of the front of the state

(define isVariable?
  (lambda (name state)
    (cond
      ((null? state) #f)
      ((eq? name (first-state-var state)) #t)
      (else (isVariable? name (next-s state))))))


(define first-statement
  (lambda (lis)
    (caddr (lis))))

(define second-statement
  (lambda (lis)
    (car(cddr(lis)))))


    

