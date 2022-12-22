#lang racket

(define funny-sum
  (λ (ls)
    (cond [(null? ls) 0]
          [(number? (car ls)) (+ (car ls) (funny-sum (cdr ls)))]
          [(string? (car ls)) (+ (string-length (car ls)) (funny-sum (cdr ls)))]
          [#t (error "expected either an integer or a string")])))

(funny-sum (list 1 "hi"))


;;; Helper functions for construction
(define (Const i) (list 'Const i))
(define (Negate e) (list 'Negate e))
(define (Add e1 e2) (list 'Add e1 e2))
(define (Multiply e1 e2) (list 'Multiply e1 e2))

;;; Helper functions for Testing
(define (Const? x) (eq? 'Const (car x)))
(define (Negate? x) (eq? 'Negate (car x)))
(define (Add? x) (eq? 'Add (car x)))
(define (Multiply? x) (eq? 'Multiply (car x)))

;;; Helper functions to retrieve the contents
(define (Const-int e) (car (cdr e)))
(define (Negate-e e) (car (cdr e)))
(define (Add-e1 e) (car (cdr e)))
(define (Add-e2 e) (car (cdr (cdr e))))
(define (Multiply-e1 e) (car (cdr e)))
(define (Multiply-e2 e) (car (cdr (cdr e))))

(define (eval-exp e)
  (cond [(Const? e) e]
        [(Negate? e) (Const (- (Const-int (eval-exp (Negate-e e)))))]
        [(Add? e) (Const (+ (Const-int (eval-exp (Add-e1 e))) (Const-int (eval-exp (Add-e2 e)))))]
        [(Multiply? e) (Const (* (Const-int (eval-exp (Multiply-e1 e))) (Const-int (eval-exp (Multiply-e2 e)))))]
        [#t (error "eval-exp expects an exp")]))


(let* ([exp1 (Const 1)]
       [exp2 (Negate exp1)]
       [exp3 (Const 2)]
       [exp4 (Add exp2 exp3)]
       [exp5 (Multiply exp1 exp4)]
       [test-exp (Multiply (Negate (Add (Const 2) (Const 2))) (Const 7))])
  (eval-exp test-exp))
       
