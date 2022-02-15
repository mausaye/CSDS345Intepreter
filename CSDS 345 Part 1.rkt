#lang racket
;;;;CSDS 345
;;;; Quyen Huynh
;;;; Tammy Lin
;;;; Elizabeth Waters
;;;; Part 1 

(require "simpleParser.rkt")
(require  "lex.rkt")

(define interpret
  (lambda (filename)
    (cond
      ((eq? (caar (interpreter filename)) 'return) (cadar (interpreter filename)))
      (else (interpret (cdr (interpreter filename)))))))

;; take in the filename 
(define interpreter
  (lambda (filename)
    (interpreterRule (parser filename) empty-lis)))

(define interpreterRule
  (lambda (expression state)
    (cond
    ((null? expression) '())
    ((number? expression) expression)
    ((null? (cdr expression)) (Mstate (the-head expression) state))
    (else (interpreterRule (the-rest expression) (Mstate (the-head expression) state))))))

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
    (else (error "Invalid Type")))))

;; return the value for the expression 
(define Mvalue
  (lambda (expression state)
    (cond
      ((null? expression) ((error 'Mvalue "No assigned value")))
      ((number? expression) expression)
      ((or (eq? expression 'true) (eq? expression #t)) #t)
      ((or (eq? expression 'false) (eq? expression #f)) #f) 
      ((and (not (list? expression)) (not (check-declare expression state))) (error "expression not declare"))
      ((eq? (Mboolean expression state) #t) (Mboolean expression state))
      ((eq? (Mboolean expression state) #f) (Mboolean expression state))
      ((eq? (isVariable? expression state) #t) (Mvalue (retrieveValue expression state) state));; if it's not a variable -> then retrieve its value ;;change this because it wasnt recursive  
      ((eq? (operator expression) '+) (+ (Mvalue (leftoperand expression)state) (Mvalue (rightoperand expression)state)))
      ((and (eq? (operator expression) '-) (null? (cddr expression))) (- 0 (Mvalue(leftoperand expression) state)))
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
      ;((eq? (check-declare lis state) #f) (add-bind lis (Mboolean (caddr lis) state) state))
      ((eq? (check-declare lis state) #f)  (add-bind lis (Mvalue (caddr lis) state) state))
      (else (error 'declare "No Value")))))

;; check if a varible is ion the state alr -> if yes then you want remove-binding and then add-binding ELSE if its not in the there then error ELSE check M-value    

;;boolean
(define Mboolean
  (lambda (if-cond state)
    (cond 
      ((null? if-cond) (error 'Mboolean "Invalid Statement"))
      ((number? if-cond) (Mvalue if-cond state))
      ((eq? if-cond 'true) #t)
      ((eq? if-cond 'false)  #f)
      ((isVariable? if-cond state) (Mvalue (retrieveValue if-cond state) state))
      ((eq? (operator if-cond) '<)   (< (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state)))
      ((eq? (operator if-cond) '>)   (> (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state)))
      ((eq? (operator if-cond) '<=)  (<= (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state)))
      ((eq? (operator if-cond) '>=)  (>= (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state)))
      ((eq? (operator if-cond) '==)  (eq? (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state)))
      ((eq? (operator if-cond) '!=)  (not (eq? (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state))))
      ((eq? (operator if-cond) '||)  (or (Mboolean (leftoperand if-cond) state) (Mboolean (rightoperand if-cond) state)))
      ((eq? (operator if-cond) '&&)  (and (Mboolean (leftoperand if-cond) state) (Mboolean (rightoperand if-cond) state)))
      ((eq? (operator if-cond) '!)   (not (Mboolean (leftoperand if-cond) state)))))) ;; what happen if it's a list like (!= (% y x) 3)


;; assign
(define assign
  (lambda ( expression state)
    (cond
      ((null? (cdr expression)) (error 'assign "cant assign"))
      ((eq? (check-declare (cadr expression) state) #t) (add-bind(cons 'var (cons (cadr expression) '())) (Mvalue(caddr expression) state) (removebind (cadr expression) state)))
      (else (error 'assign "expression has not declared")))))

;; removebind 
(define removebind
  (lambda (name state)
    (cond
      [(null? state) '()]
      [(eq? name (first-state-var state)) (cdr state)]
      [else (cons (car state) (removebind name (cdr state)))])))


;;if-stmt caddr
(define if-stmt
  (lambda (lis state)
    (cond
      ((null? lis) (error 'if-stmt "input expression is null"))
      ((Mboolean (car(cdr lis)) state) (Mstate (car(cdr (cdr lis))) state)) ;; check condition
      ((null? (cdddr lis)) state)
      (else (Mstate (cadddr lis) state)))))

      
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
      ((eq? (Mvalue (car(cdr lis)) state) #t) (return-add-bind 'true state))
      ((eq? (Mvalue (car(cdr lis)) state) #f) (return-add-bind 'false state))
      ((Mboolean (cadr lis) state) (return-add-bind (Mvalue (cadr lis) state) state))
      ((not (Mboolean (cadr lis) state)) (return-add-bind (Mvalue (cadr lis) state) state))
      (else (return-add-bind (Mvalue (cadr lis) state) state)))))






    
    


    
;; ********** helper ********* ;;
(define check-declare
  (lambda (name state)
    (cond
      ((null? state) #f)
      ((eq? (first-state-var state) name) #t )
      (else (check-declare name (next-s state ))))))

;(define check-declare 
 ; (lambda (lis state)
  ; (cond
   ;   ((null? state) #f)
    ;  ((null? lis) #t)
     ; ((null? (cdr lis)) #f)
      ;(else (and (check-declare-helper(cadr lis) state) (check-declare (cdr lis) state))))))


(define add-bind
  (lambda (lis value state)
    (cons (append (cons (car lis) (cons (cadr lis) '())) (cons value '())) state)))  ;; format the input - name type value

 (define retrieveValue
   (lambda (name state)
     (cond
       ((null? state) (error 'retrieveValue "Error: no values in state"))
       ((and (eq? name (first-state-var state)) (null? (caddar state))) (error 'retrieveValue "Error: variable used before assignment"))
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


    

