#lang racket
(define split
  (lambda (lis)
    (cond
      (split-helper lis #t))))
(define split-helper
  (lambda ( lis odd?)
          (cond
            ((null? lis) '(()()))
            (odd? (cons (car lis) (car (split-helper (cdr lis) #f))) (cdr (split-helper (cdr lis) #f)))
            (else (cons (car (split-helper (cdr lis) #t)) (cons (cons (car lis) (car (cdr (split-helper (cdr lis) #t))))))))))
(define split-cps
  (lambda (lis return)
    (cond
      ((null? lis) (return '() '()))
      (null? (cdr lis) (return lis '()))
      (else (split-cps (cddr lis) (lambda (v1 v2) (return  (cons (car lis )v1) (cons (cadr lis) v2))))))))

(define separate
  (lambda (lis return)
    (cond
      ((null? lis) (return '() '() '()))
      ((number? (car lis)) (separate (cdr lis) (lambda (v1 v2 v3) (return (cons (car lis) v1) v2) v3)))
      ((list? (car lis)) (separate (cdr lis) (lambda (v1 v2 v3) (return (cons (car lis) v1) v2) v3)))
      (else (separate (cdr lis) (lambda (v1 v2 v3) (return (cons (car lis) v1) v2) v3))))))
(define replaceatoms
  (lambda (lis return)
    ((null? lis))))

(define multiply
  (lambda (lis)
    (if (null? lis)
        0
        (* (car lis) (multiply (cdr lis))))))

(define multiply*
  (lambda (lis)
    (cond
      ((null? lis) 1)
      ((zero? (car lis)) 0)
      ((else (* (multiply (cdr lis))))))))
(define multiply-cps
  (lambda (lis return break)
    (cond
      ((null? lis) (return 1))
      ((zero? (car lis)) (break 0))
      (else

       ; scheme, denotation, syntax not ambiguous
