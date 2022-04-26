#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                            ;
; Quyen Huynh                                ;
; Tammy Lin                                  ;
; Elizabeth Waters                           ; 
; CSDS 345 Interpreter Part 4                ;
;                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "classParser.rkt")



(define interpret
  (lambda (filename main-class)
    (userFormat (interpreterRule (parser filename) main-class initial-state))))


(define bind-class-closure
  (lambda (expression compile-type instance state)
    (cond                                                   ;; name of class        super classs                          class-body                                             variable 
      ((eq? (car expression) 'class)  (box (cons 'class (cons (cadr expression) (list (caddr expression) (class-closure-body-func (cadddr expression) compile-type instance state) (car (class-closure-body-var (cadddr expression) compile-type instance '(()))) ))))))))  ;; '(class A '(closure))
     ;; (else (bind-class-closure (cdr expression) state)))))
   
(define class-closure-body-func
  (lambda (expression compile-type instance state)
    (cond
      ((null? expression) state)
      ;((list? (car expression))  (class-closure-body-var (car expression) (class-closure-body-var (cdr expression) state)))
      ((and (pair? (car expression)) (or (eq? (caar expression) 'function) (eq? (caar expression) 'static-function)))
                                    (cons  (add-func-closure-top (car expression) compile-type instance state) (class-closure-body-func (cdr expression) compile-type instance state)))
       ((and (pair? (car expression)) (eq? (caar expression) 'var)) (class-closure-body-func (cdr expression) compile-type instance state)) 
      ((pair? (car expression)) (append (class-closure-body-func (car expression) compile-type instance state) (class-closure-body-func (cdr expression)  compile-type instance state))) 
                                
      (else (class-closure-body-func (cdr expression) compile-type instance state)))))

(define class-closure-body-var
   (lambda (expression compile-type instance state)
    (cond
      ((null? expression) state)
      ;(( and (not (atom? (car expression))) (eq? (caar expression) 'var))  (declare expression state (lambda (throw) (error "invalid throw")))) 
      ((list? (car expression))  (if (or (eq? (caar expression) 'function) (eq? (caar expression) 'static-function))
                                     (class-closure-body-var (cdr expression) compile-type instance  state)
                         
                                     (class-closure-body-var (car expression) compile-type instance (class-closure-body-var (cdr expression) compile-type instance state)))) 
      ((eq? (car expression) 'var)  (declare expression compile-type instance state (lambda (throw) (error "invalid throw")))) 
      (else (class-closure-body-var (cdr expression) compile-type instance  state)))))

;; format : (var a (new B))
(define bind-instance-closure
     ;; this is the variable name 
   (lambda (type state)
    (cond
      ((null? type) (error "no vairbale name")); 
      (else (list type (cadddr (cdr (retrieve-closure type state))) (car (cadddr (retrieve-closure type state))) ))))) 


(define bind-global-helper
  (lambda (expression compile-type instance state)
    (cond
      ((null? expression) state)
      ((list? (car expression)) (append (bind-global-helper (cdr expression) compile-type instance state) (bind-global-helper (car expression) compile-type instance state)))
      ((eq? 'class (car expression)) (add-class-closure-top (bind-class-closure expression compile-type instance state) state)))))

(define bind-global
  (lambda (expression )
    (cons '() (bind-global-helper expression '() '() '())))) 
 
(define add-class-closure-top
  (lambda (closure state)
    (cond
      ((null? closure) state)
      ((null? state) (list (list closure)))
      (else (cons (append (list closure) (car state)) (cdr state))))))

;'(()
 ; (#&(class B (extends A) (((#&(closure set1 (a) (()) (funcall set2 a a)))) ((#&(closure main () (()) (var b (new B)) (funcall (dot b set1) 10) (return (funcall (dot b prod)))))) ()) (#&(b B))))
  ;(#&(class A () (((#&(closure prod () (()) (return (* (dot this x) (dot this y)))))) ((#&(closure set2 (a b) (()) (= x a) (= y b)))) ()) (#&(x 6) #&(y 7)))))
;;


; interpreterRule: (expression: parsed code, state: the state of the program)
; Iterates through each section of the program, matching the keywords to the correct functions
;;
(define interpreterRule
  (lambda (expression main-class state)
    (call/cc
     (lambda (return)
       ;(cond
         ; The expression is empty, thus nothing to add to state
         (lookup-main main-class  '() '() (bind-global expression) return (lambda (cont) cont) (lambda (break) break) (lambda (throw) (error "Invalid throw statement" )))))))

  
        ; ((eq? (cadar expression) 'main) (interpret-main expression state return
                                                    ;     (lambda (cont) cont) (lambda (break) break) (lambda (throw) (error "Invalid throw statement"))))
         ; Expression is a number, return the number
         ;((number? expression) expression)
         ; Only traversed on the first component since there is no other expression
         ;((null? (the-rest expression)) (Mstate (the-head expression) state return 
                                                ;(lambda (cont) cont) (lambda (break) break) (lambda (throw) throw)))
         ; Traversed on the first list and the remaining lists
         ;(else (interpreterRule (the-rest expression) main-class (Mstate (the-head expression) state return
                                                 ;             (lambda (cont) cont) (lambda (break) break) (lambda (throw) (error "Invalid throw statement"))))))))))
 (define lookup-main
     (lambda (main-class compile-type instance state return cont break throw)
       ;;retrieve the main 
        (interpret-main (retrieve-closure 'main (retrieve-closure main-class state)) compile-type instance  state return cont break throw)))



 ;;  (define lookup-main
  ;;   (lambda (main-class state return cont break throw)
       ;;retrieve the main 
      ;;  (interpret-main (retrieve-closure 'main (retrieve-closure main-class state)) (list (box (retrieve-closure main-class state))) return cont break throw)))
        ;; ((null? expression) (error "No main function found"))
         ;((eq? (car expression) 'main) (beginScope expression (retrieve-closure main-class state)))
         ;(else (lookup-main (cdr expression))))))
;;
; Evaluates the main function and runs the code inside the block.
;;

(define interpret-main
  (lambda (exp  compile-type instance environment return continue break throw)
    (cond
      ((null? exp) environment)
      (else (interpret-stmts (cdr (cdddr exp)) compile-type instance (addlayer environment) return continue break throw)))))

;; interpret the each input statement 
(define interpret-stmts
  (lambda (exp compile-type instance state return continue break throw)
    (cond
      ((null? exp) state)
      (else (interpret-stmts (the-rest exp) compile-type instance (Mstate (the-head exp) compile-type instance state return continue break throw) return continue break throw)))))   

;;
; Adds the closure bind to the top for the environment.
;;
(define add-func-closure-top
  (lambda (func compile-type instance environment)
    (cond
      ;;        func-body = cadddr 
      [(null? (cadddr func)) environment]
      [(list? (cadddr func)) (insert-func-closure-top (box (append (append (cons 'closure (list (closure-name func) (formal-param func))) (list environment)) compile-type (cadddr func)))  environment)]
      [else (insert-func-closure-top (box (list 'closure (input-name func) (formal-param func) (list environment) compile-type (function-body func))) compile-type instance environment)])))

;; Inserts a binding to the global scope
(define insert-global; (() ())  
  (lambda (binding environment)
    (cond
      ((null?  environment)  (list binding))
      ((null? (the-head environment))                                     (cons (list binding) (the-rest environment)))
      ((and (list?(the-head environment)) (null? (the-rest environment))) (list (cons binding (the-head environment))))
      [else                                                               (cons (the-head environment)(insert-global binding (the-rest environment)))])))

;; helper function for add-func-closure-top
(define insert-func-closure-top
  (lambda (binding  env)
    (cond
      ((null? env)                            (list binding))
      ((list? (the-head env))                 (cons (insert-func-closure-top binding (the-head env)) (the-rest env)))
      (else                                   (cons binding env)))))

;;
; retrieve-closure : take in the name of the function, the enviroment is the the active state of the state and the complete state
; return the closure with the name searched in it
;;

(define isNotInstance
  (lambda (state)
    (cond
      ((null? state) #t)
      ((or (eq? 'class (car (unbox state))) (eq? 'closure (car (unbox state)))) #t)
      (else  #f))))

(define retrieve-closure
  (lambda (name environment )
    (cond
      ((null? environment)                                                                                    #f)
      ((list? (the-head environment))                                                                         (or (retrieve-closure name (the-head environment)) (retrieve-closure name (the-rest environment))))
      ((and (box? (the-head environment))  (eq? name (closure-name (unbox (the-head environment)))))           (unbox (the-head environment)))
      ((and (and (box? (the-head environment)) (box? (cadr (unbox(the-head environment)))))  (and (eq? 'instance (car(unbox(cadr (unbox(the-head environment)))))) (eq? name (car (unbox (the-head environment))))))  (unbox( cadr(unbox (the-head environment)))))
      (else                                                                                                   (retrieve-closure name (the-rest environment))))))

(define retrieve-function-closure
  (lambda (name environment)
    (cond
      ((null? environment)                                                                                    #f)
      ((list? (the-head environment))                                                                        (or (retrieve-function-closure (the-head environment)) (retrieve-function-closure (the-rest environment))))
      ((eq? (

;;(define retrieve-class-closure
;;  (lambda (name environment)
;;    (cond
;;     ((null? environment)                                                                                    #f)
 ;;    ((list? (the-head environment))       (or (retrieve-class-closure name (the-head environment)) (retrieve-class-closure name (the-rest environment))))
  ;;    ((and (box? (the-head environment)) (eq? name (cadr (unbox (the-head environment)))))           (unbox (the-head environment)))
   ;;   (else                                                                                                   (retrieve-class-closure name (the-rest environment))))))


;;
; bind-formal-actual : take the formal parameter and the actual parameter, the enviroment is the first / active state and the state
; return the formal and actual parameter binded 
;;
(define bind-formal-actual
  (lambda (formal actual compile-type instance environment throw)
    (cond
      ((and (null? formal) (null? actual)) environment)
      ((or (null? formal) (null? actual)) (error "mismatched number params"))
      ((and (null? (the-rest formal))(atom? actual)) (add-bind environment (the-head formal) (Mvalue actual compile-type instance environment throw)))
      (else (bind-formal-actual (the-rest formal) (the-rest actual) (add-bind environment (the-head formal) (Mvalue (the-head actual) compile-type instance environment throw))))))) 

(define bind-this
  ;; compileName is the instance name 
  (lambda (compileName state)
    (cond
      ((null? compileName) state)
      (else (add-top (box (list 'this (box(retrieve-closure compileName state )))) state)))))


(define add-top
  (lambda (bind state)
    (cons (cons bind (car state)) (cdr state))))




;;
; createBinding : take the formal parameter, the actual parameter, the enviroment is the first/ active state, the state and throw
; return the binding for the actual and formal parameter in the enviroment, and the state and throw
;;
(define createBinding
  (lambda (formal actual compile-type instance environment state throw)
    (cond
      ((and (null? formal) (null? actual)) '())
      ((or (null? formal) (null? actual)) (error "mismatched number params"))
      ((and (null? (the-rest formal))(atom? actual)) (box (the-head formal) (Mvalue actual compile-type instance state throw)))
      (else (append (createBinding (the-rest formal) (the-rest actual) compile-type instance  environment state throw) (list (box (list (the-head formal) (Mvalue (the-head actual) compile-type instance state throw)))))))))

;;
; find-value : take the parameter , the enviroment and throw
; return the parameter evaluated in the enviroment
;;
(define find-value
  (lambda (params compile-type instance environment throw)
    (cond
      ((null? params) params)
      ((atom? params)  (Mvalue params compile-type instance environment throw))
      ((list? (the-head params)) (cons (Mvalue (the-head params) compile-type instance environment throw) (find-value (the-rest params) environment))) 
      (else  (cons (find-value (the-head params) environment) (find-value (the-rest params) environment))))))


;; evaluates a function that has a return value
(define interpret-function
  (lambda (name actual-params compile-type instance environment throw)
    (call/cc
     (lambda (func-return)
       (cond
       ;;  ((eq? (car (cadr name)) 'dot) (retrieve-closure (retrieveValue environment (cadr name)) environment))
        
         ((eq? (car name) 'dot) (interpret-stmts (closure-body (retrieve-closure (caddr name) ( cdddr(unbox (retrieveValue environment (cadr name)))))) compile-type instance
                                                 (bind-this (cadr name)(cons (createBinding (closure-formal-param (retrieve-closure (caddr name) (cdddr(retrieve-closure (cadr name) environment))))
                                                                      actual-params compile-type instance '() environment throw) environment))  
                                                       func-return (lambda (cont) cont) (lambda (break) break) throw))    ;; return body - execute body
        
         ((not (retrieve-closure (cadr name) environment)) (error "function undefined"))
         ((list? (retrieve-closure name environment environment)) (beginScope (closure-body (retrieve-closure name environment ))
                                                                              (cons (createBinding (closure-formal-param (retrieve-closure name  environment))
                                                                               actual-params compile-type instance (closure-state(retrieve-closure name environment)) environment throw)
                                                                               (closure-state(retrieve-closure name environment))) func-return (lambda (cont) cont) (lambda (break) break) throw))
         ;; if the closure is within a list
         (else (beginScope (list (closure-body(retrieve-closure name environment))) 
                           (cons (createBinding (closure-formal-param (retrieve-closure name environment))
                                 actual-params compile-type instance (closure-state(retrieve-closure name environment)) environment throw)
                                 (closure-state(retrieve-closure name environment))) func-return (lambda (cont) cont) (lambda (break) break) throw)))))))  


;; evaluates a function that does not have a return value
(define interpret-function-no-return
  (lambda (name actual-params compile-type instance environment throw)
    (call/cc
     (lambda (env-return)
      (cond
        ((eq? (car name) 'dot) (interpret-stmts (closure-body (retrieve-closure (caddr name) ( cdddr(unbox (retrieveValue environment (cadr name)))))) compile-type instance
                                                 (bind-this (cadr name)(cons (createBinding (closure-formal-param (retrieve-closure (caddr name) (cdddr(retrieve-closure (cadr name) environment))))
                                                                      actual-params compile-type instance '() environment throw) environment))  
                                                       env-return (lambda (cont) cont) (lambda (break) break) throw))
        ((not (retrieve-closure name  environment))(error "function undefined")) 
        ((list? (retrieve-closure name environment )) (interpret-body (closure-body (retrieve-closure name environment ))
                                                                                 (addlayer (cons (createBinding (closure-formal-param (retrieve-closure name environment ))
                                                                                  actual-params compile-type instance (closure-state(retrieve-closure name environment )) environment throw) environment ))
                                                                                 (lambda (return) return) (lambda (cont) cont) (lambda (break) break) throw env-return))
        (else (interpret-body (list (closure-body(retrieve-closure name environment )))
                              (addlayer (cons (createBinding (closure-formal-param (retrieve-closure name environment ))
                               actual-params compile-type instance (closure-state(retrieve-closure name environment)) environment throw) environment))
                              (lambda (return) return) (lambda (cont) cont) (lambda (break) break) throw env-return)))))))

;; runs the body of the function using Mstate
(define interpret-body
  (lambda (expression compile-type instance state return continue break throw env-return)
    (cond
      ((null? expression)                                                     (next-s state))
      ((eq? 'return (the-return expression))                                  (env-return state))
      (else                                                                   (interpret-body (the-rest expression) (Mstate (the-head expression) compile-type instance state return continue break throw) return continue break throw env-return )))))

;;
; Mstate: (expression: parsed code segment beginning with a keyword, state: current state of the program)
; Take in an expression and a state, and returns the state of the program after the
; code segment has run.
;;
(define Mstate
  (lambda (expression compile-type instance state return continue break throw)
    (cond
    ((null? expression) expression)
    ((eq? (operator expression) 'begin)        (beginScope (the-rest expression) state return continue break throw))
    ((eq? (operator expression) 'try)          (try expression state return continue break throw)) 
    ((eq? (operator expression) 'catch)        (catch expression state return continue break throw)) 
    ((eq? (operator expression) 'continue)     (continue (next-s state)))
    ((eq? (operator expression) 'throw)        (throw (Mvalue (throw-value expression) compile-type instance state throw)))
    ((eq? (operator expression) 'break)        (break (next-s state)))
    ((eq? (operator expression) 'var)          (declare expression compile-type instance state throw))
    ((eq? (operator expression) '=)            (assign state (leftoperand expression) (Mvalue (rightoperand expression) compile-type instance state throw)))
    ((eq? (operator expression) 'while)        (while-loop expression state return continue break throw)) 
    ((eq? (operator expression) 'return)       (return (execute-return (return-val expression) compile-type instance state throw)))
    ((eq? (operator expression) 'if)           (if-stmt expression state return continue break throw))
    ((eq? (operator expression) 'function)     (add-func-closure-top expression state))
    ((eq? (operator expression) 'funcall)      (interpret-function-no-return (closure-name expression) (func-param expression) compile-type instance state throw))
    ((eq? (operator expression) 'class)        (interpret-function-no-return (closure-name expression) (func-param expression) compile-type instance state throw));; newly added but need an intepret class function 
    (else (error "Invalid Type")))))

;;

;(define interpret-class
 ; (lambda (expression state return continue break throw)
  ;  (cond
    ;  ((and (null? (caddr expression)) (null? (cadddr expression))) state)
    ;  ((
;;
; get-active-env : take the function name and the enviroment
; return the active enviroment
;;
(define get-active-env
  (lambda (funct-name env)
    (cond
      ((null? env)                               (error "Function not declared in the environment"))
      ((null? (the-rest env))                    (cons (box (retrieve-closure funct-name env)) env))
      (else                                      (cons (box (retrieve-closure funct-name env)) (the-rest env))))))

;; value here is the var name 
(define instance-or-value
  (lambda (value  compile-type instance state throw)
    (cond
      ((number? value) value)
      ((not (retrieve-closure value state)) (Mvalue value compile-type instance state throw))
      (else (bind-instance-closure  value state))))) 
;;
; Mvalue: (expression: the parsed code segment, state: the current state of the program)
; example inputs:
; (+ x y) -> x + y,
; (-(+ x 5) z) -> (x + 5) - z
; x -> value of x
; Returns the value of the expression.
;;
(define Mvalue
  (lambda (expression compile-type instance state throw)
    (cond
      ((null? expression)                                                    ((error 'Mvalue "No assigned value")))
      ;((atom? expression)                                                  expression)
      ((number? expression)   expression)
      
      ; Maps the atom 'true to #t
      ((or (eq? expression 'true) (eq? expression #t))                       #t)
  
      ;; Maps the atom 'false to #f
      ((or (eq? expression 'false) (eq? expression #f))                      #f)

      ((eq? (check-declare expression state) #t)                             (instance-or-value (retrieveValue state expression) compile-type instance state throw)) 
      
      ;; Checks if it is a variable that has not been declared
      ((and (not (list? expression)) (not (check-declare expression state))) (error "Expression not declare"))
      
      
      ;; Computes a boolean expression corresponding to true
      ((eq? (Mboolean expression compile-type instance state throw) #t )                           (Mboolean expression state throw))
      
      ;; Computes a boolean expression corresponding to false
      ((eq? (Mboolean expression compile-type instance state throw) #f )                           (Mboolean expression state throw))
      
      ;; Retrieves the value of a variable
      ((eq? (check-declare expression state) #t)                             (Mvalue (retrieveValue state expression) compile-type instance state throw))
      
      ((eq? (operator expression) '+)                                        (+ (Mvalue (leftoperand expression) compile-type instance state throw) (Mvalue (rightoperand expression)  compile-type instance state throw))) 
      ((and (eq? (operator expression) '-)                                   (null? (null-val expression))) (- 0 (Mvalue(leftoperand expression) compile-type instance state throw)))  
      ((eq? (operator expression) '-)                                        (- (Mvalue (leftoperand expression) compile-type instance state throw) (Mvalue (rightoperand expression) compile-type instance state throw)))  
      ((eq? (operator expression) '*)                                        (* (Mvalue (leftoperand expression) compile-type instance state throw) (Mvalue (rightoperand expression) compile-type instance state throw))) 
      ((eq? (operator expression) '/)                                        (quotient (Mvalue (leftoperand expression) compile-type instance state throw) (Mvalue (rightoperand expression) compile-type instance state throw))) 
      ((eq? (operator expression) '%)                                        (remainder (Mvalue (leftoperand expression) compile-type instance state throw) (Mvalue (rightoperand expression) compile-type instance state throw)))
      ((eq? (operator expression) 'funcall)                                  (interpret-function (closure-name expression) (func-param expression) compile-type instance state throw))
      ((eq? (operator expression) 'dot)                                      (retrieveValue (caddr (retrieve-closure (leftoperand expression) state))  (rightoperand expression) ))
      (else                                                                  (error 'badop "Bad operator")))))

;;(bind-instance-closure 'B '(() (#&(class B () ((#&(closure set1 () () (return this))) (#&(closure main () () (var b (new B)) (return (dot this b))))) (#&(a 10))))))

 

(define return-instance
  (lambda (expression state)
    (cond
      (retrieve-closure (cadr expression) state))))

;;
; Mboolean: (if-cond: the condition to be evaulated, state: the current state of the program)
; Computes the boolean expression provided.
;;
(define Mboolean
  (lambda (if-cond compile-type instance state throw)
    (cond
      ((null? if-cond)                   (error 'Mboolean "Invalid Statement"))
      ((number? if-cond)                 (Mvalue if-cond compile-type instance state throw))
      ((eq? if-cond 'true)               #t) ;; converts the atom true to the value #t
      ((eq? if-cond 'false)              #f) ;; converts the atom false to the value #f
      ((eq? if-cond #t)                  #t)
      ((eq? if-cond #f)                  #f)
      ((check-declare if-cond state)     (Mvalue (retrieveValue state if-cond) compile-type instance state throw)) ;; retrieves the boolean variable value
      ((eq? (operator if-cond) '<)       (< (Mvalue (leftoperand if-cond)  compile-type instance state throw) (Mvalue (rightoperand if-cond) compile-type instance state throw))) 
      ((eq? (operator if-cond) '>)       (> (Mvalue (leftoperand if-cond) compile-type instance state throw) (Mvalue (rightoperand if-cond) compile-type instance state throw))) 
      ((eq? (operator if-cond) '<=)      (<= (Mvalue (leftoperand if-cond) compile-type instance state throw) (Mvalue (rightoperand if-cond) compile-type instance state throw))) 
      ((eq? (operator if-cond) '>=)      (>= (Mvalue (leftoperand if-cond) compile-type instance state throw) (Mvalue (rightoperand if-cond) compile-type instance state throw))) 
      ((eq? (operator if-cond) '==)      (eq? (Mvalue (leftoperand if-cond) compile-type instance state throw) (Mvalue (rightoperand if-cond) compile-type instance state throw))) 
      ((eq? (operator if-cond) '!=)      (not (eq? (Mvalue (leftoperand if-cond) compile-type instance state throw) (Mvalue (rightoperand if-cond) compile-type instance state throw)))) 
      ((eq? (operator if-cond) '||)      (or (valOrBoolean (leftoperand if-cond) state throw) (valOrBoolean (rightoperand if-cond) state throw))) 
      ((eq? (operator if-cond) '&&)      (and (valOrBoolean (leftoperand if-cond) state throw) (valOrBoolean (rightoperand if-cond) state throw)))
      ((eq? (operator if-cond) '!)       (not (Mboolean (leftoperand if-cond) state throw))))))

;;
; valOrBoolean : take the expression, the state, and throw
; return whether to execute MBoolean and MValue
;; 
(define valOrBoolean
  (lambda (exp compile-type instance state throw)
    (cond
      ((not (list? exp))                     (Mboolean exp compile-type instance state throw))
      ((eq? (operator exp) 'funcall)         (Mvalue exp compile-type instance state throw))
      (else                                  (Mboolean exp  compile-type instance state throw)))))


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
  (lambda (lis compile-type instance state throw)
    (cond
      ((null? lis)                                                                                empty-lis)
      ((eq? (check-declare (varName lis) (the-head state)) #t)                                    (error 'Mstate "Variable already declared"))
      ((list? (caddr lis))                    (if (eq? 'new (caaddr lis))
                                                 (add-bind state (varName lis) (box(cons 'instance (bind-instance-closure (car (cdaddr lis)) state))))
                                                   (empty-lis))) ;bind class declaration to name of class
      ((and (eq? (check-declare (varName lis) (the-head state)) #f) (null? (null-val lis)))       (add-bind state (varName lis) null)) 
      ((eq? (check-declare lis state) #f)                                                         (add-bind state (varName lis) (Mvalue (the-value lis) compile-type instance state throw)))
      (else                                                                                       (error 'declare "No Value")))))

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
      ((eq? (car name) 'dot)                                (set-box! (search-box (rightoperand name) (caddr(retrieve-closure (leftoperand name) state))) (list (rightoperand name) new-value)))
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
  (lambda (lis compile-type instance state return continue break throw)
    (cond
      ((null? lis)                                  (error 'if-stmt "Input expression is null"))
      
      ; Check the condition and change the state if condition is true
      ((Mboolean (cond-stmt lis) state throw)       (Mstate (stmt-one lis) compile-type instance state return continue break throw))
      
      ; Checks if the else statement exists
      ((null? (else-stmt lis))                       state)

      ; Checks if the else if statement exists
      (else                                          (Mstate (else-if-stmt lis) compile-type instance state return continue break throw))))) 
      
;;
; while-loop: (lis: the while expression, state: the current state of the program)
; Computes a while loop and changes the state accordingly
; while (cond) (stmt)
;;
(define while-loop
  (lambda (lis compile-type instance state return continue break throw)
    (call/cc (lambda (newbreak)
               (cond
                 ((null? lis)                                             (error 'while-loop "invalid while-loop"))
                 ((Mboolean (cond-stmt lis) state throw)                  (Mstate lis compile-type instance (call/cc (lambda (cont) (Mstate (while-body lis) compile-type instance state return cont newbreak throw))) return continue newbreak throw))
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
  (lambda (catch-exp compile-type instance state return continue break throw)
      (try-helper (the-rest catch-exp) (Mstate (first-element catch-exp) compile-type instance state return continue break throw) return continue break throw)))

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
; Reformats the return value.
;;
(define userFormat
  (lambda (exp)
    (cond
      ((eq? exp #t) 'true)
      ((eq? exp #f) 'false)
      (else exp)))) 

;;
; Executes the body given and updates the state.
;;
(define try-helper
  (lambda (exp compile-type instance state return continue break throw)
    (cond
      ((null? exp)                   state)
      (else                          (try-helper (the-rest exp) (Mstate (first-element exp) compile-type instance state return continue break throw) return continue break throw)))))

;;
; Returns values in the appropriate format.
;;
(define execute-return
  (lambda (expression compile-type instance state throw)
    (cond
      ((or (eq? expression 'true) (eq? (Mvalue expression compile-type instance state throw) #t))              #t)
      ((or (eq? expression 'false) (eq? (Mvalue expression compile-type instance state throw) #f))             #f)
      (else                                                                             (Mvalue expression compile-type instance state throw)))))
      
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
  (lambda (expression compile-type instance state return continue break throw)
    (cond
      ((null? expression)                 (next-s state))
      (else                               (beginScope-helper (the-rest expression) (Mstate (first-element expression)
                                                                                           compile-type instance state return continue break throw) return continue break throw)))))
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
      ((list? (first-element state)) (or (check-declare name (first-element state)) (check-declare name (the-rest state))))
      ((and (box? (first-element state)) (eq? (box-name (unbox (first-element state))) name) #t))
      (else (check-declare name (the-rest state))))))

;;
; take off the global state
;; 
(define remove-global
  (lambda (state)
    (cond
      ((null? state) '())
      ((null? (the-rest state)) '())
      (else (cons (the-head state) (remove-global (the-rest state)))))))
;;
; return the global state
;;
(define get-global
  (lambda (state)
    (cond
      ((null? state) '())
      ((null? (the-rest state)) state)
      (else (get-global (the-rest state))))))


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

;; return true or false whether it's an atom 
(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))

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

;; return the "return"
(define the-return
  (lambda (expression)
    (caar expression)))

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

;; return the body of main 
(define main-body
  (lambda (exp)
    (car (cdddar exp))))

;; return the closure's name               
(define closure-name
  (lambda (closure)
    (cadr closure)))

;; return the closure function's formal parameter      
(define closure-formal-param
  (lambda (closure)
    (caddr closure)))

;; return the closure function's body 
(define closure-body
  (lambda (closure)
    (cddddr closure)))

;; return the closure function's state  
(define closure-state
  (lambda (closure)
    (cadddr closure)))

;; the function actual parameters
(define func-param
  (lambda (exp)
    (cddr exp)))

;; return the function body 
(define func-body
  (lambda (func)
    (car(cddddr func))))

(define compile-type
  (lambda (func)
    (cadddr func)))


;;return the formal param
(define formal-param
  (lambda (func)
    (caddr func)))

;; return the function body
(define function-body 
  (lambda (func)
    (caar(cdddr func))))

(define test
  (lambda (filename class result number)
    (display (list "Test" number (eq? (interpret filename class) result)))))

;;((#&(this #&(instance A (#&(x 6)) (#&(closure setX (x) () (= (dot this x) x))))) #&(x 6))
;;(#&(this #&(instance A (#&(x 6)) (#&(closure setX (x) () (= (dot this x) x))))) #&(x 30))
;; (#&(a2 #&(instance A (#&(x 6)) (#&(closure setX (x) () (= (dot this x) x))))) #&(a1 #&(instance A (#&(x 6)) (#&(closure setX (x) () (= (dot this x) x))))))
;; ()
;; (#&(class A () ((#&(closure setX (x) () (= (dot this x) x))) (#&(closure add (a) () (return (+ (dot a x) (dot this x)))))
;;                                                              (#&(closure main () () (var a1 (new A)) (var a2 (new A)) (funcall (dot a1 setX) 30) (funcall (dot a2 setX) 6) (return (funcall (dot a1 add) a2)))))
;;      (#&(x 6)))))


(define testAll
  (lambda ()
  (values
  (test "test1.txt" 'A 15 1)
  (test "test2.txt" 'A 12 2)
  (test "test3.txt" 'A 125 3))))