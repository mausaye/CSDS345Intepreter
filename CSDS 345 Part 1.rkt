#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                            ;
; Quyen Huynh                                ;
; Tammy Lin                                  ;
; Elizabeth Waters                           ; 
; CSDS 345 Interpreter Part 1                ;
;                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "simpleParser.rkt")
(require  "lex.rkt")

;;
; interpret: (file name)
; takes a file name with code segments and returns the value
; returned in the program
; cadar is return vaue and caar is the-state
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
    (interpreterRule (parser filename) '(()) )))

;;
; interpreterRule: (expression: parsed code, state: the state of the program)
; Iterates through each section of the program, matching the keywords to the correct functions
;;
(define interpreterRule
  (lambda (expression state)
    (call/cc
     (lambda (return)
       (cond
         ((null? expression) '()) ;; the expression is empty, thus nothing to add to state
         ((number? expression) expression) ;; expression is a number, return the number
         ((null? (the-rest expression)) (Mstate (the-head expression) state return (lambda (cont) cont)
                                                                                   (lambda (break) break)
                                                                                   (lambda (throw) throw))) ;;(new added) only traversed on the first component since there is no other expression
         (else (interpreterRule (the-rest expression) (Mstate (the-head expression) state return (lambda (cont) cont)
                                                                                                 (lambda (break) break)
                                                                                                 (lambda (throw) throw))))))))) ;; (new added) traversed on the first list and the remaining lists
;;
; Mstate: (expression: parsed code segment beginning with a keyword, state: current state of the program)
; Take in an expression and a state, and returns the state of the program after the
; code segment has run.
;;
(define Mstate
  (lambda (expression state return continue break throw)
    (cond
    ((null? expression) expression)
    ((eq? (operator expression) 'begin) (beginScope (cdr expression) state return continue break throw)) 
    ((eq? (operator expression) 'try) (try expression state return continue break throw)) 
    ((eq? (operator expression) 'catch) (catch expression state return continue break throw)) 
    ;((eq? (operator expression) 'finally) (finally expression state return continue break throw))
    ((eq? (operator expression) 'continue) (continue (cdr state)))
    ((eq? (operator expression) 'throw) (throw (Mvalue (cadr expression) state)))
    ((eq? (operator expression) 'break) (break (next-s state)))
    ((eq? (operator expression) 'var) (declare expression state))
    ((eq? (operator expression) '=) (assign state (cadr expression) (Mvalue (caddr expression) state)))
    ((eq? (operator expression) 'while) (while-loop expression state return continue break throw)) 
    ((eq? (operator expression) 'return) (return (Mvalue (cadr expression) state)))
    ((eq? (operator expression) 'if) (if-stmt expression state return continue break throw))
    (else (error "Invalid Type")))))


; caddr exp : catch
; cadr exp : try body (does not include try word)
; cadddr exp: finally

(define finally-statement
  (lambda (expression)
    (cadddr expression)))

(define catch-statement
  (lambda (expression)
    (caddr expression)))

(define try-body
  (lambda (expression)
    (cadr expression)))

(define catch-variable
  (lambda (expression)
    (caadr expression)))
        

(define try
  (lambda (exp state return continue break throw)

     (define throwval (call/cc (lambda (newthrow) (try-helper (try-body exp) state return continue break newthrow))))
    (cond

     


      ((number? throwval) (finally (cadddr exp) (catch (caddr (catch-statement exp)) (add-bind state (catch-variable (catch-statement exp)) throwval)
                                          return continue break throw) return continue break throw))
      (else (finally (cadddr exp) state return continue break throw)))))



;(define mythrow
 ; (lambda (catch-exp state)
  ;  (Mstate (cdr catch-exp) (add-bind state (catch-variable catch-exp))))) 
;((= x 20) (if (> x 10) (throw 5)) (= x (+ x 5)))
;; executes the try body and looks for throw
(define try-helper
  (lambda (exp state return continue break throw)
    (cond
      ((null? exp) state)
      (else (try-helper (cdr exp) (Mstate (car exp) state return continue break throw) return continue break throw)))))
      
;; i want catch Mstate its body     
(define catch
  (lambda (catch-exp state return continue break throw)
    ;(cond
     ;((number? (call/cc (lambda (newthrow) (try-helper catch-exp) state return continue break newthrow)))) 
    ; (else
      (try-helper (cdr catch-exp) (Mstate (car catch-exp) state return continue break throw) return continue break throw)))
    
 (define finally
  (lambda (finally-statement state return continue break throw)
    (cond
      ((null? finally-statement) state)
      ((eq? (car finally-statement) 'finally) (finally (cadr finally-statement) state return continue break throw))
      (else (try-helper finally-statement state return continue break throw)))))
      
      
       
       
                          
;input (sum-with-cut '(1 2 3 cut 4 except 5 6 end 7 8) 'invalid)
;invalid should become a function
(define sum-with-cut
  (lambda (lis break)
    (cond
      ((null? lis) 0)
      ((eq? (car lis) 'cut) (call/cc (lambda (k) (sum-with-cut (cdr lis) k)))) ;; k value break returns
      ((eq? (car lis) 'end) (break (sum-with-cut (cdr lis) (lambda (v) v)))); 
      ((eq? (car lis) 'except) (sum-with-cut (cdr lis) (lambda (v) (break (+ (cadr lis) v))))) ;; v: state of sum before cut
      ((+ (car lis) (sum-with-cut (cdr lis) break))))))

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
      ((eq? (check-declare expression state) #t) (Mvalue (retrieveValue state expression) state)) ;; retrieves the value of a variable   
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
      ((and (eq? (check-declare (cadr lis) state) #f) (null? (cddr lis))) (add-bind state (cadr lis) null)) ;; declaring a variable to a null value
      ((eq? (check-declare lis state) #f)  (add-bind state (cadr lis) (Mvalue (caddr lis) state))) ;; declaring a variable to a value
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
      ((check-declare if-cond state) (Mvalue (retrieveValue state if-cond) state)) ;; retrieves the boolean variable value
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
  (lambda (state name new-value)
    (begin
      (cond 
      ((null? new-value)  (error 'assign "No value given to assign"))
      ((eq? (check-declare name state) #t) (set-box! (search-box name state) (list name new-value)))
      (else (error 'assign "Expression has not declared")))
      state)))

(define search-box
  (lambda (name state)
    (cond
      ((null? state) #f)
      ((list? (car state)) (or (search-box name (car state)) (search-box name (cdr state))))
      ((and (box? (car state)) (eq? name (car (unbox (car state))))) (car state))
      (else (search-box name (cdr state))))))

;;
; if-stmt: (lis: the if expression, state: the state of the program)
; Computes the if statement and changes the state accordingly
; input:
; (if (cond) (stmt1)): if
; (if (cond) (stmt1) (if (cond) (stmt2) (...))): if and else-if
; (if (cond) (stmt1) (stmt2): if and else
;;
(define if-stmt
  (lambda (lis state return continue break throw)
    (cond
      ((null? lis) (error 'if-stmt "Input expression is null"))
      ((Mboolean (cond-stmt lis) state) (Mstate (stmt-one lis) state return continue break throw))  ;; Check the condition and change the state if condition is true
      ((null? (else-stmt lis)) state) ;; Checks if the else statement exists
      (else (Mstate (else-if-stmt lis) state return continue break throw))))) ;; Checks if the else if statement exists
      
;;
; while-loop: (lis: the while expression, state: the current state of the program)
; Computes a while loop and changes the state accordingly
; while (cond) (stmt)
;;
(define while-loop
  (lambda (lis state return continue break throw)
    (call/cc (lambda (newbreak)
               (cond
                 ((null? lis) (error 'while-loop "invalid while-loop"))
                 ((Mboolean (cond-stmt lis) state) (Mstate lis (call/cc (lambda (cont) (Mstate (caddr lis) state return cont newbreak throw))) return continue newbreak throw))
                 ((not (Mboolean (cond-stmt lis) state)) state))))))
                     
;;
; return: (lis: return statement, state: current state of the program)
; Computes the return expression and adds the return value to the state
; input format: (return (expression))
;;
(define return
  (lambda (lis state)
    (cond
      ((null? (the-rest lis)) lis)
      ((eq? (Mvalue (cond-stmt lis) state) #t) (return-add-bind 'true state))  ;; checks for #t or #f so their respective atoms can be returned
      ((eq? (Mvalue (cond-stmt lis) state) #f) (return-add-bind 'false state))
      ((Mboolean (cond-stmt lis) state) (return-add-bind (Mvalue (cond-stmt lis) state) state)) ;; checks for boolean expressions
      ((not (Mboolean (cond-stmt lis) state)) (return-add-bind (Mvalue (cond-stmt lis) state) state))
      (else (return-add-bind (Mvalue (cond-stmt lis) state) state))))) ;; checks for arthemetic expressions

(define beginScope-helper 
  (lambda (expression state return continue break throw)
    (cond
      ((null? expression)(next-s state))
      (else (beginScope-helper (cdr expression) (Mstate (car expression) state return continue break throw) return continue break throw)))))

(define beginScope 
  (lambda (expression state return continue break throw)
       (beginScope-helper expression (addlayer state) return continue break throw)))

;; ********** Helper Functions ********* ;;

;;
; add a layer on the top 
;;
(define addlayer
  (lambda (state)
    (cons '() state )))

;;
; Checks if a variable has been declared in a state given a variable name and a state.
;;
(define check-declare
  (lambda (name state)
    (cond
      ((null? state) #f)
      ((list? (car state)) (or (check-declare name (car state)) (check-declare name (cdr state))))
      ((and (box? (car state)) (eq? (car (unbox (car state))) name) #t))
      (else (check-declare name (next-s state))))))

;;
; Adds a binding to the state in the format (type name value)
;;
(define add-bind
  (lambda (state name value)
    (cond
      ((null? state) (list (box (list name value))))
      ((and (not (box? (car state))) (car state)(list? (car state))) (cons (add-bind (car state) name value) (cdr state)))
      (else (cons (box (list name value)) state)))))

;;
; Retrieves a value of a variable in a state given the name and the state.
;;
 (define retrieveValue
  (lambda (state name)
    (cond
      ((null? state) #f)
      ((list? (car state)) (or (retrieveValue (car state) name) (retrieveValue (cdr state) name)))
      ((and (box? (car state)) (eq? (car (unbox (car state))) name)) (cadr (unbox (car state))))
      (else (retrieveValue (cdr state) name)))))

;(define setValue
 ; (lambda (state name new-value)
   ; (cond
    ;  ((null? state) #f)
     ; ((list? (car state)) (or (setValue (car state) name new-value) (setValue (cdr state) name new-value)))
     ; ((and (box? (car state)) (eq? (car (unbox (car state))) name)) (set-box! (car state) (list name new-value)))
     ; (else (setValue (cdr state) name new-value)))))


;;
; Adds a binding for the return statement. Form: (return value)
;;
(define return-add-bind
  (lambda (value state)
    (cons (cons 'return (cons value '())) state)))

;;************ Abstraction ************** ;;

;;
; The operator of an arthmetic expression
;;
(define operator
  (lambda (exp)
    (car exp)))

;;
; The next expression in the state
;;
(define next-s
  (lambda (state)
    (cdr state)))

;;
; The variable name in the state or input.
; input: (type name value)
;;
(define varName cadr)

;;
; The value of a variable in the state or input.
; input: (type name value)
;;
(define the-value caddr)

;;
; The value of the state input: (type name value)
;;
(define state-value caddar)

;;
; The condition of an if or while statement.
;;
(define cond-stmt cadr)

;;
; The right operand in an arthimetic expression.
;;
(define rightoperand caddr)

;;
; The left operand in an arthimetic expression.
;;
(define leftoperand cadr)

;;
; the remaining expression after the car is removed
;;
(define the-rest cdr)

;;
; The first statement in the expression
;;
(define the-head car)

;;
; The empty list
;;
(define empty-lis '())

;; 
; Returns the name of the variable input
; lis: (var x value)
;;
(define input-name
  (lambda (lis)
          (car(cdr lis))))

    
;; the first statement in the if-statement
(define stmt-one caddr)

;; the else statement within the if-statement
(define else-stmt cdddr)

;; the else-if in the if-statement
(define else-if-stmt cadddr)

