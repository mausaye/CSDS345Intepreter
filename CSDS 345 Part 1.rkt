#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                            ;
; Quyen Huynh                                ;
; Tammy Lin                                  ;
; Elizabeth Waters                           ; 
; CSDS 345 Interpreter Part 2              ;
;                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "functionParser.rkt")
(require  "lex.rkt")

;;
; interpret: (file name)
; take in the filename and returns the state of the program after the code from the file is executed
;;
(define interpret
  (lambda (filename)
    (interpreterRule (parser filename) initial-state)))

;;
; interpreterRule: (expression: parsed code, state: the state of the program)
; Iterates through each section of the program, matching the keywords to the correct functions
;;
(define interpreterRule
  (lambda (expression state)
    (call/cc
     (lambda (return)
       (cond
         ; The expression is empty, thus nothing to add to state
         ((null? expression)(error "no main function found" ))
         ((eq? (cadar expression) 'main) (interpret-main expression state return (lambda (cont) cont) (lambda (break) break) (lambda (throw) (error "Invalid throw statement"))))
         ; Expression is a number, return the number
         ((number? expression) expression)
         ; Only traversed on the first component since there is no other expression
         ((null? (the-rest expression)) (Mstate (the-head expression) state return 
                                                (lambda (cont) cont) (lambda (break) break) (lambda (throw) throw)))
         ; Traversed on the first list and the remaining lists
         (else (interpreterRule (the-rest expression) (Mstate (the-head expression) state return
                                                              (lambda (cont) cont) (lambda (break) break) (lambda (throw) (error "Invalid throw statement"))))))))))

;(define find
 ; (lambda (environment)
  ;  ((null? environment) '())
   ; ((eq? (car environment) 'main) (cddr environment))
    ;(else (find 'main (cdr environment)))))

(define interpret-main
  (lambda (exp environment return continue break throw)
    (cond
      ((null? exp) environment)
      (else (interpret-stmts (car (cdddar exp)) (addlayer environment) return continue break throw)))))

(define interpret-stmts
  (lambda (exp state return continue break throw)
    (cond
      ((null? exp) state)
      (else (interpret-stmts (the-rest exp) (Mstate (car exp) state return continue break throw) return continue break throw)))))
 
(define add-closure-top
  (lambda (func environment)
    (cond
      [(null? (car (cdddr func))) environment]
      ;cadr : function name caddr: formal params car cdddr: function body
      [(list? (cadddr func)) (insert-closure-top (box (append (append (cons 'closure (list (cadr func) (caddr func))) (list environment)) (car (cdddr func))))  environment)]
      [else (insert-closure-top (box (list 'closure (cadr func) (caddr func) (list environment) (caar (cdddr func))))  environment)])))

(define insert-global; (() () )  
  (lambda (binding environment)
    (cond
      ((null?  environment)  (list binding))
      ((null? (car environment)) (cons (list binding) (cdr environment)))
      ((and (list?(car environment)) (null? (cdr environment))) (list (cons binding (car environment))))
      [else (cons (car environment)(insert-global binding (cdr environment)))])))

(define insert-closure-top
  (lambda (binding env)
    (cond
      ((null? env) (list binding))
      ;((null? (list? (car env)) (list (list binding))))
      ((list? (car env)) (cons (insert-closure-top binding (car env)) (cdr env)))
      (else (cons binding env)))))
                
(define closure-name
  (lambda (closure)
    (cadr closure)))

(define closure-formal-param
  (lambda (closure)
    (caddr closure)))

(define closure-body
  (lambda (closure)
    (cddddr closure)))

(define closure-state
  (lambda (closure)
    (cadddr closure)))

(define retrieve-closure
  (lambda (name environment state)
    (cond
      ((null? environment) #f)
      ((list? (car environment)) (or (retrieve-closure name (car environment) state) (retrieve-closure name (cdr environment) state)))
      ((and (box? (car environment)) (eq? name (closure-name (unbox (car environment))))) (unbox (car environment)))
      (else (retrieve-closure name (cdr environment) state)))))

(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))

(define bind-formal-actual
  (lambda (formal actual environment throw)
    (cond
      ((and (null? formal) (null? actual)) environment)
      ((or (null? formal) (null? actual)) (error "mismatched number params"))
      ((and (null? (cdr formal))(atom? actual)) (add-bind environment (car formal) (Mvalue actual environment throw)))
      (else (bind-formal-actual (cdr formal) (cdr actual) (add-bind environment (car formal) (Mvalue (car actual) environment throw)))))))

(define createBinding
  (lambda (formal actual environment state throw)
    (cond
      ((and (null? formal) (null? actual)) '())
      ((or (null? formal) (null? actual)) (error "mismatched number params"))
      ((and (null? (cdr formal))(atom? actual)) (box (car formal) (Mvalue actual state throw)))
      (else (append (createBinding (cdr formal) (cdr actual) environment state throw) (list (box (list (car formal) (Mvalue (car actual) state throw)))))))))

(define find-value
  (lambda (params environment throw)
    (cond
      ((null? params) params)
      ((atom? params)  (Mvalue params environment throw))
      ((list? (car params)) (cons (Mvalue (car params) environment throw) (find-value (cdr params) environment)))
      (else  (cons (find-value (car params) environment) (find-value (cdr params) environment))))))

(define interpret-function
  (lambda (name actual-params environment throw)
    (call/cc
     (lambda (func-return)
       (cond
         ((not (retrieve-closure name environment environment)) (error "function undefined"))
         ((list? (retrieve-closure name environment environment)) (beginScope (cddddr (retrieve-closure name environment environment)) (cons (createBinding (closure-formal-param (retrieve-closure name environment environment)) actual-params (cadddr(retrieve-closure name environment environment)) environment throw) (cadddr(retrieve-closure name environment environment))) func-return (lambda (cont) cont) (lambda (break) break) throw))
         (else (beginScope (list (closure-body(retrieve-closure name environment environment))) (cons (createBinding (closure-formal-param (retrieve-closure name environment environment)) actual-params (cadddr(retrieve-closure name environment)) environment throw) (cadddr(retrieve-closure name environment environment))) func-return (lambda (cont) cont) (lambda (break) break) throw)))))))



(define interpret-function-no-return
  (lambda (name actual-params environment throw)
    (call/cc
     (lambda (env-return)
      (cond
        ((not (retrieve-closure name environment environment)) (error "function undefined"))
        ((list? (retrieve-closure name environment environment)) (interpret-body (cddddr (retrieve-closure name environment environment)) (addlayer (cons (createBinding (closure-formal-param (retrieve-closure name environment environment)) actual-params (cadddr(retrieve-closure name environment environment)) environment throw) environment ))  (lambda (return) return) (lambda (cont) cont) (lambda (break) break) throw env-return))
        (else (interpret-body (list (closure-body(retrieve-closure name environment environment))) (addlayer (cons (createBinding (closure-formal-param (retrieve-closure name environment environment)) actual-params (cadddr(retrieve-closure name environment)) environment throw) environment))  (lambda (return) return) (lambda (cont) cont) (lambda (break) break) throw env-return)))))))

(define interpret-body
  (lambda (expression state return continue break throw env-return)
    (cond
      ((null? expression) (next-s state))
      ((eq? 'return (caar expression)) (env-return state))
      (else (interpret-body (cdr expression) (Mstate (car expression) state return continue break throw) return continue break throw env-return )))))


;(define getActive
 ; (lambda (state)
  ;  (define removeglobal (remove-global state))
   ; (cond
    ; ((null? state) state)
     ;((list? (car state)) (cons (getActive (car state)) (cdr state)))
     ;((not (eq? (car (unbox (car state))) 'closure)) (getActive (cdr state)))
     ;((
;;

;;outer layer interpretation

;;
; Mstate: (expression: parsed code segment beginning with a keyword, state: current state of the program)
; Take in an expression and a state, and returns the state of the program after the
; code segment has run.
;;
(define Mstate
  (lambda (expression state return continue break throw)
    (cond
    ((null? expression) expression)
    ((eq? (operator expression) 'begin)        (beginScope (the-rest expression) state return continue break throw))
    ((eq? (operator expression) 'try)          (try expression state return continue break throw)) 
    ((eq? (operator expression) 'catch)        (catch expression state return continue break throw)) 
    ((eq? (operator expression) 'continue)     (continue (next-s state)))
    ((eq? (operator expression) 'throw)        (throw (Mvalue (throw-value expression) state throw)))
    ((eq? (operator expression) 'break)        (break (next-s state)))
    ((eq? (operator expression) 'var)          (declare expression state throw))
    ((eq? (operator expression) '=)            (assign state (leftoperand expression) (Mvalue (rightoperand expression) state throw)))
    ((eq? (operator expression) 'while)        (while-loop expression state return continue break throw)) 
    ((eq? (operator expression) 'return)       (return (execute-return (return-val expression) state throw)))
    ((eq? (operator expression) 'if)           (if-stmt expression state return continue break throw))
    ((eq? (operator expression) 'function)     (add-closure-top expression state))
    ((eq? (operator expression) 'funcall)      (interpret-function-no-return (cadr expression) (cddr expression) state) throw)
    ;((eq? (operator expression) 'funcall)      (interpret-function-no-return (cadr expression) (cddr expression) (cons (createBinding (closure-formal-param (retrieve-closure (cadr expression) state)) (cddr expression) state) (get-active-env (cadr expression) state))))
    (else (error "Invalid Type")))))

(define get-active-env
  (lambda (funct-name env)
    (cond
      ((null? env) (error "Function not declared in the environment"))
      ((null? (cdr env)) (cons (box (retrieve-closure funct-name env)) env))
      (else (cons (box (retrieve-closure funct-name env)) (cdr env)))))) 

;;
; Mvalue: (expression: the parsed code segment, state: the current state of the program)
; example inputs:
; (+ x y) -> x + y,
; (-(+ x 5) z) -> (x + 5) - z
; x -> value of x
; Returns the value of the expression.
;;
(define Mvalue
  (lambda (expression state throw)
    (cond
      ((null? expression)                                                    ((error 'Mvalue "No assigned value")))
      ((number? expression)                                                  expression)
      
      ; Maps the atom 'true to #t
      ((or (eq? expression 'true) (eq? expression #t))                       #t)
      
      ;; Maps the atom 'false to #f
      ((or (eq? expression 'false) (eq? expression #f))                      #f)
      
      ;; Checks if it is a variable that has not been declared
      ((and (not (list? expression)) (not (check-declare expression state))) (error "Expression not declare"))
      
      ;; Computes a boolean expression corresponding to true
      ((eq? (Mboolean expression state throw) #t)                                  (Mboolean expression state throw))
      
      ;; Computes a boolean expression corresponding to false
      ((eq? (Mboolean expression state throw) #f)                                  (Mboolean expression state throw))
      
      ;; Retrieves the value of a variable
      ((eq? (check-declare expression state) #t)                             (Mvalue (retrieveValue state expression) state throw))
      
      ((eq? (operator expression) '+)                                        (+ (Mvalue (leftoperand expression) state throw) (Mvalue (rightoperand expression) state throw))) 
      ((and (eq? (operator expression) '-)                                   (null? (cddr expression))) (- 0 (Mvalue(leftoperand expression) state throw)))  
      ((eq? (operator expression) '-)                                        (- (Mvalue (leftoperand expression) state throw) (Mvalue (rightoperand expression) state throw)))  
      ((eq? (operator expression) '*)                                        (* (Mvalue (leftoperand expression) state throw) (Mvalue (rightoperand expression) state throw))) 
      ((eq? (operator expression) '/)                                        (quotient (Mvalue (leftoperand expression) state throw) (Mvalue (rightoperand expression) state throw))) 
      ((eq? (operator expression) '%)                                        (remainder (Mvalue (leftoperand expression) state throw) (Mvalue (rightoperand expression) state throw)))
      ((eq? (operator expression) 'funcall)                                  (interpret-function (cadr expression) (cddr expression) state throw))
      (else                                                                  (error 'badop "Bad operator")))))

;;
; Mboolean: (if-cond: the condition to be evaulated, state: the current state of the program)
; Computes the boolean expression provided.
;;
(define Mboolean
  (lambda (if-cond state throw)
    (cond 
      ((null? if-cond)                   (error 'Mboolean "Invalid Statement"))
      ((number? if-cond)                 (Mvalue if-cond state throw))
      ((eq? if-cond 'true)               #t) ;; converts the atom true to the value #t
      ((eq? if-cond 'false)              #f) ;; converts the atom false to the value #f
      ((check-declare if-cond state)     (Mvalue (retrieveValue state if-cond) state throw)) ;; retrieves the boolean variable value
      ((eq? (operator if-cond) '<)       (< (Mvalue (leftoperand if-cond) state throw) (Mvalue (rightoperand if-cond) state throw))) 
      ((eq? (operator if-cond) '>)       (> (Mvalue (leftoperand if-cond) state throw) (Mvalue (rightoperand if-cond) state throw))) 
      ((eq? (operator if-cond) '<=)      (<= (Mvalue (leftoperand if-cond) state throw) (Mvalue (rightoperand if-cond) state throw))) 
      ((eq? (operator if-cond) '>=)      (>= (Mvalue (leftoperand if-cond) state throw) (Mvalue (rightoperand if-cond) state throw))) 
      ((eq? (operator if-cond) '==)      (eq? (Mvalue (leftoperand if-cond) state throw) (Mvalue (rightoperand if-cond) state throw))) 
      ((eq? (operator if-cond) '!=)      (not (eq? (Mvalue (leftoperand if-cond) state throw) (Mvalue (rightoperand if-cond) state throw)))) 
      ((eq? (operator if-cond) '||)      (or (Mboolean (leftoperand if-cond) state throw) (Mboolean (rightoperand if-cond) state throw))) 
      ((eq? (operator if-cond) '&&)      (and (Mboolean (leftoperand if-cond) state throw) (Mboolean (rightoperand if-cond) state throw)))
      ((eq? (operator if-cond) 'funcall) (interpret-function (cadr if-cond) (cddr if-cond) state throw))
      ((eq? (operator if-cond) '!)       (not (Mboolean (leftoperand if-cond) state throw)))))) 


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
  (lambda (lis state throw)
    (cond
      ((null? lis)                                                                    empty-lis)
      ((eq? (check-declare (varName lis) (car state)) #t)                                   (error 'Mstate "Variable already declared"))
      ((and (eq? (check-declare (varName lis) (car state)) #f) (null? (null-val lis)))     (add-bind state (varName lis) null)) 
      ((eq? (check-declare lis state) #f)                                             (add-bind state (varName lis) (Mvalue (the-value lis) state throw)))
      (else                                                                           (error 'declare "No Value")))))

;;
; assign: (expression: the assign expression, state: the current state of the program)
; assigns the variable to the associated value.
; inputs: (= variableName value)
; example:
; (= x y) -> x = yf
; (= x (+ y z)) -> x = y + z
;;
(define assign
  (lambda (state name new-value)
    (begin
      (cond 
      ((null? new-value)                                    (error 'assign "No value given to assign"))
      ((eq? (check-declare name state) #t)                  (set-box! (search-box name state) (list name new-value)))
      (else                                                 (error 'assign "Expression has not declared")))
                                                            state)))

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
      ((null? lis)                            (error 'if-stmt "Input expression is null"))
      
      ; Check the condition and change the state if condition is true
      ((Mboolean (cond-stmt lis) state throw)       (Mstate (stmt-one lis) state return continue break throw))
      
      ; Checks if the else statement exists
      ((null? (else-stmt lis))                state)

      ; Checks if the else if statement exists
      (else                                   (Mstate (else-if-stmt lis) state return continue break throw))))) 
      
;;
; while-loop: (lis: the while expression, state: the current state of the program)
; Computes a while loop and changes the state accordingly
; while (cond) (stmt)
;;
(define while-loop
  (lambda (lis state return continue break throw)
    (call/cc (lambda (newbreak)
               (cond
                 ((null? lis)                                       (error 'while-loop "invalid while-loop"))
                 ((Mboolean (cond-stmt lis) state throw)                  (Mstate lis (call/cc (lambda (cont) (Mstate (while-body lis) state return cont newbreak throw))) return continue newbreak throw))
                 ((not (Mboolean (cond-stmt lis) state throw))            state))))))


;;
; Executes the try body of the code.
;;
(define try
  (lambda (exp state return continue break throw)
    (define throwval (call/cc (lambda (newthrow) (try-helper (try-body exp) (addlayer state) return continue break newthrow))))
    (cond
      ((number? throwval) (finally (finally-statement exp) (catch (catch-body exp) (add-bind (addlayer state) (catch-variable (catch-statement exp)) throwval)
                                          return continue break throw) return continue break throw))
      (else (finally (finally-statement exp) (addlayer state) return continue break throw)))))

;;
; Executes the catch body of the code.
;;
(define catch
  (lambda (catch-exp state return continue break throw)
      (try-helper (the-rest catch-exp) (Mstate (first-element catch-exp) state return continue break throw) return continue break throw)))

;;
; Executes the finally block of the code.
;;
 (define finally
  (lambda (finally-statement state return continue break throw)
    (cond
      ((null? finally-statement)                          (next-s state))
      ((eq? (first-element finally-statement) 'finally)   (finally (finally-body finally-statement) state return continue break throw))
      (else                                               (try-helper finally-statement state return continue break throw)))))




;; ********** Helper Functions ********* ;;

;;
; Executes the body given and updates the state.
;;
(define try-helper
  (lambda (exp state return continue break throw)
    (cond
      ((null? exp)                   state)
      (else                          (try-helper (the-rest exp) (Mstate (first-element exp) state return continue break throw) return continue break throw)))))

;;
; Returns values in the appropriate format.
;;
(define execute-return
  (lambda (expression state throw)
    (cond
      ((or (eq? expression 'true) (eq? (Mvalue expression state throw) #t))      'true)
      ((or (eq? expression 'false) (eq? (Mvalue expression state throw) #f))      'false)
      (else                                    (Mvalue expression state throw)))))
      
;;
; Add a layer on the top of the state.
;;
(define addlayer
  (lambda (state)
    (cons empty-lis state )))

;;
; Runs each expression of the scope.
;;
(define beginScope-helper 
  (lambda (expression state return continue break throw)
    (cond
      ((null? expression)                 (next-s state))
      (else                               (beginScope-helper (the-rest expression) (Mstate (first-element expression)
                                                                                           state return continue break throw) return continue break throw)))))
;;
; Starts a new state and run the code inside the scope.
;;
(define beginScope 
  (lambda (expression state return continue break throw)
       (beginScope-helper expression (addlayer state) return continue break throw)))

;;
; Checks if a variable has been declared in a state given a variable name and a state.
;;
(define check-declare
  (lambda (name state)
    (cond
      ((null? state) #f)
      ;((atom? state) #f)
      ((list? (first-element state)) (or (check-declare name (first-element state)) (check-declare name (the-rest state))))
      ((and (box? (first-element state)) (eq? (box-name (unbox (first-element state))) name) #t))
      (else (check-declare name (the-rest state))))))

(define remove-global
  (lambda (state)
    (cond
      ((null? state) '())
      ((null? (cdr state)) '())
      (else (cons (car state) (remove-global (cdr state)))))))

(define get-global
  (lambda (state)
    (cond
      ((null? state) '())
      ((null? (cdr state)) state)
      (else (get-global (cdr state))))))


;;
; Adds a binding to the state in the format (type name value)
;;
(define add-bind
  (lambda (state name value)
    (cond
      ((null? state)                                                                               (list (box (list name value))))
      ((and (not (box? (first-element state))) (first-element state)(list? (first-element state))) (cons (add-bind (first-element state) name value) (the-rest state)))
      (else                                                                                        (cons (box (list name value)) state)))))

;;
; Retrieves a value of a variable in a state given the name and the state.
;;
 (define retrieveValue
  (lambda (state name)
    (cond
      ((null? state) #f)
      ((list? (first-element state))                                                           (or (retrieveValue (first-element state) name) (retrieveValue (the-rest state) name)))
      ((and (box? (first-element state))(eq? (box-name (unbox (first-element state))) name))   (box-value (unbox (first-element state))))
      (else (retrieveValue (the-rest state)                                                    name)))))

;;
; Searches the a state of boxes for a variable given a name.
;;
(define search-box
  (lambda (name state)
    (cond
      ((null? state) #f)
      ((list? (first-element state))           (or (search-box name (box-name state)) (search-box name (the-rest state))))
      ((and (box? (first-element state))       (eq? name (box-name (unbox (first-element state))))) (box-name state))
      (else                                    (search-box name (the-rest state))))))

;;************ Abstraction ************** ;;

;; the first element of the statement
(define first-element
  (lambda (exp)
    (car exp)))

;; the variable name in a box
(define box-value
  (lambda (exp)
    (cadr exp)))

;; the variable name in a box
(define box-name
  (lambda (exp)
    (car exp)))
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

;; the throw value of the throw expression
(define throw-value
  (lambda (exp)
    (cadr exp)))
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

;; the initial state of the program
(define initial-state '(()))

;; finally statement of the try statement
(define finally-statement
  (lambda (expression)
    (cadddr expression)))

;; the catch statement of the expression
(define catch-statement
  (lambda (expression)
    (caddr expression)))

;; the catch body of the catch statement
(define catch-body
  (lambda (expression)
    (caddr (caddr expression))))

;; the try body of the try statement
(define try-body
  (lambda (expression)
    (cadr expression)))

;; the catch variable of the catch statement
(define catch-variable
  (lambda (expression)
    (caadr expression)))

;; return value
(define return-val
  (lambda (exp)
    (cadr exp)))

;; the while body of the while statement
(define while-body
  (lambda (exp)
    (caddr exp)))

;; the null value
(define null-val cddr)

;; the finally body
(define finally-body
  (lambda (exp)
    (cadr exp)))