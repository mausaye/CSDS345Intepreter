#lang racket
;;;;CSDS 345
;;;; Quyen Huynh
;;;; Tammy Lin
;;;; Elizabeth Waters
;;;; Part 1 

(require "simpleParser.rkt")
(require  "lex.rkt")

;;
; interpret: (file name)
; takes a file name with code segments and returns the value
; returned in the program
;;
(define interpret
  (lambda (filename)
    (cond
      ((eq? (caar (interpreter filename)) 'return) (cadar (interpreter filename))) ;; retrieves the element associated with the return in the state
      (else (interpret (cdr (interpreter filename))))))) ;; finds the 'return atom

;;
; interpreter: (file name)
; take in the filename and returns the state of the program after the code from the file is executed
;;
(define interpreter
  (lambda (filename)
    (interpreterRule (parser filename) empty-lis)))

;;
; interpreterRule: (expression: parsed code, state: the state of the program)
; Iterates through each section of the program, matching the keywords to the correct functions
;;
(define interpreterRule
  (lambda (expression state)
    (cond
    ((null? expression) '()) ;; the expression is empty, thus nothing to add to state
    ((number? expression) expression) ;; expression is a number, return the number
    ((null? (cdr expression)) (Mstate (the-head expression) state)) ;; only traversed on the first component since there is no other expression
    (else (interpreterRule (the-rest expression) (Mstate (the-head expression) state)))))) ;; traversed on the first list and the remaining lists

;;
; Mstate: (expression: parsed code segment beginning with a keyword, state: current state of the program)
; Take in an expression and a state, and returns the state of the program after the
; code segment has run.
;;
(define Mstate
  (lambda (expression state)
    (cond
    ((null? expression) expression)
    ((eq? (operator expression) 'var) (declare expression state))
    ((eq? (operator expression) '=) (assign expression state))
    ((eq? (operator expression) 'while) (while-loop expression state)) 
    ((eq? (operator expression) 'return) (return expression state))
    ((eq? (operator expression) 'if) (if-stmt expression state))
    (else (error "Invalid Type")))))

;;
; Mvalue: (expression: the parsed code segment, state: the current state of the program)
; example inputs:
; (+ x y) -> x + y,
; (-(+ x 5) z) -> (x + 5) - z
; x -> value of x
; Returns the value of the expression.
;;
(define Mvalue
  (lambda (expression state)
    (cond
      ((null? expression) ((error 'Mvalue "No assigned value")))
      ((number? expression) expression)
      ((or (eq? expression 'true) (eq? expression #t)) #t) ;; maps the atom 'true to #t
      ((or (eq? expression 'false) (eq? expression #f)) #f) ;; maps the atom 'false to #f
      ((and (not (list? expression)) (not (check-declare expression state))) (error "expression not declare")) ;; checks if it is a variable that has not been declared
      ((eq? (Mboolean expression state) #t) (Mboolean expression state)) ;; computes a boolean expression corresponding to true
      ((eq? (Mboolean expression state) #f) (Mboolean expression state)) ;; computes a boolean expression corresponding to false
      ((eq? (isVariable? expression state) #t) (Mvalue (retrieveValue expression state) state)) ;; retrieves the value of a variable   
      ((eq? (operator expression) '+) (+ (Mvalue (leftoperand expression)state) (Mvalue (rightoperand expression)state))) 
      ((and (eq? (operator expression) '-) (null? (cddr expression))) (- 0 (Mvalue(leftoperand expression) state)))  
      ((eq? (operator expression) '-) (- (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state)))  
      ((eq? (operator expression) '*) (* (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state))) 
      ((eq? (operator expression) '/) (quotient (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state))) 
      ((eq? (operator expression) '%) (remainder (Mvalue (leftoperand expression) state) (Mvalue (rightoperand expression) state))) 
      (else (error 'badop "Bad operator")))))

;;
; declare: (lis: the parsed code segment, state: the current state of the program)
; declares an input variable by placing it into the state
; already declared variables will throw an error.
; example inputs:
; (var x) -> s: ((var x ())
; (var x 5) -> s: ((var x 5))
; (var x (+ x y)) -> s: (var x (x + y))
;;
(define declare
  (lambda (lis state)
    (cond
      ((null? lis) '()) ;; invalid expression cant be declared 
      ((eq? (check-declare (cadr lis) state) #t) (error 'Mstate "Variable already declared")) ;; variable already declared 
      ((and (eq? (check-declare (cadr lis) state) #f) (null? (cddr lis))) (add-bind lis null state)) ;; declaring a variable to a null value
      ((eq? (check-declare lis state) #f)  (add-bind lis (Mvalue (caddr lis) state) state)) ;; declaring a variable to a value
      (else (error 'declare "No Value")))))

;;
; Mboolean: (if-cond: the condition to be evaulated, state: the current state of the program)
; Computes the boolean expression provided.
;;
(define Mboolean
  (lambda (if-cond state)
    (cond 
      ((null? if-cond) (error 'Mboolean "Invalid Statement"))
      ((number? if-cond) (Mvalue if-cond state))
      ((eq? if-cond 'true) #t) ;; converts the atom true to the value #t
      ((eq? if-cond 'false)  #f) ;; converts the atom false to the value #f
      ((isVariable? if-cond state) (Mvalue (retrieveValue if-cond state) state)) ;; retrieves the boolean variable value
      ((eq? (operator if-cond) '<)   (< (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state))) 
      ((eq? (operator if-cond) '>)   (> (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state))) 
      ((eq? (operator if-cond) '<=)  (<= (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state))) 
      ((eq? (operator if-cond) '>=)  (>= (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state))) 
      ((eq? (operator if-cond) '==)  (eq? (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state))) 
      ((eq? (operator if-cond) '!=)  (not (eq? (Mvalue (leftoperand if-cond) state) (Mvalue (rightoperand if-cond) state)))) 
      ((eq? (operator if-cond) '||)  (or (Mboolean (leftoperand if-cond) state) (Mboolean (rightoperand if-cond) state))) 
      ((eq? (operator if-cond) '&&)  (and (Mboolean (leftoperand if-cond) state) (Mboolean (rightoperand if-cond) state)))
      ((eq? (operator if-cond) '!)   (not (Mboolean (leftoperand if-cond) state)))))) 


;;
; assign: (expression: the assign expression, state: the current state of the program)
; assigns the variable to the associated value.
; inputs: (= variableName value)
; example:
; (= x y) -> x = y
; (= x (+ y z)) -> x = y + z
;;
(define assign
  (lambda (expression state)
    (cond
      ((null? (cddr expression)) (error 'assign "No value given to assign")) ;; No value to be assigned
      ((eq? (check-declare (cadr expression) state) #t) (add-bind(cons 'var (cons (cadr expression) '())) (Mvalue(caddr expression) state) (removebind (cadr expression) state))) ;; Checking a variable declaration before assigning its value
      (else (error 'assign "Expression has not declared")))))

;;
; removebind: (name: name of the variable to remove, state: the state of the program)
; Given a variable name, the bind in the state is removed
;; 
(define removebind
  (lambda (name state)
    (cond
      [(null? state) '()]
      [(eq? name (first-state-var state)) (cdr state)] ;; checks if the variable is present if so, remove from state
      [else (cons (car state) (removebind name (cdr state)))])))


;;
; if-stmt: (lis: the if expression, state: the state of the program)
; Computes the if statement and changes the state accordingly
; input:
; (if (cond) (stmt1)): if
; (if (cond) (stmt1) (if (cond) (stmt2) (...))): if and else-if
; (if (cond) (stmt1) (stmt2): if and else
;;
(define if-stmt
  (lambda (lis state)
    (cond
      ((null? lis) (error 'if-stmt "Input expression is null"))
      ((Mboolean (car(cdr lis)) state) (Mstate (car(cdr (cdr lis))) state)) ;; Check the condition and change the state if condition is true
      ((null? (cdddr lis)) state) ;; Checks if the else statement exists
      (else (Mstate (cadddr lis) state))))) ;; Checks if the else if statement exists

      
;;
; while-loop: (lis: the while expression, state: the current state of the program)
; Computes a while loop and changes the state accordingly
; while (cond) (stmt)
;;
(define while-loop
  (lambda (lis state)
    (cond
      ((null? lis) (error 'while-loop "invalid while-loop"))
      ((Mboolean (car(cdr lis)) state) (Mstate lis (Mstate(caddr lis) state)))
      ((not (Mboolean (cadr lis) state)) state)))) 


;;
; return: (lis: return statement, state: current state of the program)
; Computes the return expression and adds the return value to the state
; input format: (return (expression))
;;
(define return
  (lambda (lis state)
    (cond
      ((null? (cdr lis)) lis)
      ((eq? (Mvalue (car(cdr lis)) state) #t) (return-add-bind 'true state)) ;; checks for #t or #f so their respective atoms can be returned
      ((eq? (Mvalue (car(cdr lis)) state) #f) (return-add-bind 'false state))
      ((Mboolean (cadr lis) state) (return-add-bind (Mvalue (cadr lis) state) state)) ;; checks for boolean expressions
      ((not (Mboolean (cadr lis) state)) (return-add-bind (Mvalue (cadr lis) state) state))
      (else (return-add-bind (Mvalue (cadr lis) state) state))))) ;; checks for arthemetic expressions


;; ********** Helper Functions ********* ;;
(define check-declare
  (lambda (name state)
    (cond
      ((null? state) #f)
      ((eq? (first-state-var state) name) #t )
      (else (check-declare name (next-s state ))))))

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


    

