#lang racket
(define multiply
  (lambda (lis)
      (call/cc (lambda (break)
                       (multiply-with-break lis break)))))
(define multiply-with-break
  (lambda (lis break)
    (cond
      ((null? lis) 1)
      ((zero? (car lis)) (break 0))
      (else (* (car lis) (multiply-with-break (cdr lis) break))))))

(define mycontinuation #t)

(define factorial
  (lambda (n)
    (call/cc (lambda (k)
               (if (zero? n)
                   (begin
                    (set! mycontinuation k)
                    1)
                   (* n (factorial (- n 1))))))))


; indexof : take a list and an element and return the index of the element in the list

(define indexof
  (lambda (x lis)
    (call/cc (lambda (break)
               (indexof-helper x lis break)))))
(define indexof-helper
  (lambda (x lis break)
    (cond
      ((null? lis) (break -1))
      ((eq? x (car lis)) 1)
      (else (+ 1 (indexof-helper (cdr lis) break))))))
 