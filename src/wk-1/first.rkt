#lang racket

(define (extract str)
    (substring str 4 7))

(define (echos str) str)

(define fact
  (λ (x) (if (= x 0)
                  1
                  (* x (fact (- x 1))))))

(define x 1)
(define y 2)

(define mod2
  (λ (x)
    (letrec
        ([even? (λ (x) (if (zero? x) #t (odd? (- x 1))))]
         [odd? (λ (x) (if (zero? x) #f (even? (- x 1))))])
      (if (even? x) 0 1))))

(define two_args
  (λ (x y) (+ x y)))

; sum of an int list
(define sum
  (λ (xs) (if (null? xs)
             0
             (+ (car xs) (sum (cdr xs))))))

(sum (list 1 2 3))

; summing the integers in any list
; could be a nested list too
(define sum1
  (λ (xs) (cond [(null? xs) 0]
                [(number? xs) xs]
                [(list? xs) (+ (sum1 (car xs)) (sum1 (cdr xs)))]
                [#t 0])))

(sum1 (list 5 6 (list 2 3 #t)))

(define (silly-double x)
  (let ([x (+ x 3)]
        [y (+ x 2)])
    (+ x y -5)))

(define (silly-double* x)
  (let* ([x (+ x 3)]
         [y (+ x 2)]) ; here the y is 3 + 2 + value of
                      ; argument, the x in scope is the one
                      ; defined in the previous line
    (+ x y -8)))
