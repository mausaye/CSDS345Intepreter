#lang racket
;;;;CSDS 345
;;;; Quyen Huynh
;;;; Tammy Lin
;;;; Nhan Truong
;;;; Part 1 

(require "simpleParser.rkt")
(require  "lex.rkt")

;; take in the filename 
(define intepreter
  (lambda (filename)
    (parseFile filename)))

;; return the value for the expression 
(define Mvalue
  (lambda (expression)
    (cond
      ((number? expression) (error 'Mvalue "Undefined"))
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
      ((eq? (operator expression) '+) (+ (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression))))
      ((eq? (operator expression) '-) (- (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression))))
      ((eq? (operator expression) '*) (* (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression))))
      ((eq? (operator expression) '/) (quotient (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression))))
      ((eq? (operator expression) '%) (remainder (Mvalue (leftoperand expression)) (Mvalue (rightoperand expression))))
      (else (error 'badop "Bad operator")))))

;; use this to parse file
(define parseFile
  (lambda (filename)
    (parser filename)))

;; take in an expression and a state -> return the type updated of the expression 
(define Mtype 
  (lambda (expression state)
    ((null? expression) expression)
    ((eq? (operator expression) 'var) (declare expression state));make a func. for declare
    ((eq? (operator expression) '=) (assign expression state))
    ((eq? (operator expression) 'while) (while-loop expression state))
    ((eq? (operator expression) 'return) (return expression state))
    ((eq? (operator expression) 'if) (if-loop expression state))
    (else 'Mtype "Not Valid Type")))

;;declare
(define declare
  (lambda (expression state)
    (cond
      ((null? expression) expression)
      ((check-declaare expression state) #t))))
    
    


    
;;********** helper *********;;
(define check-declare
  (lambda (expression state)
    ((null? state) #f)
    ((eq? (the-hhead state) expression) #t)
    (else (check-declare expression (next-s state)))))


   
;;************ Abstraction ************** ;;;;;
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
(define the-hhead caar)

    

