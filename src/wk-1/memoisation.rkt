#lang racket


(define fibonacci-expoenential
  (λ (x) (if (or (= x 1) (= x 2))
             1
             (+ (fibonacci-expoenential (- x 1)) (fibonacci-expoenential (- x 2))))))

(fibonacci-expoenential 10)
(fibonacci-expoenential 30)

(define fibanocci-linear
  (λ (x) (letrec ([f (λ (acc1 acc2 y)
                       (if (= x y)
                           (+ acc1 acc2)
                           (f (+ acc1 acc2) acc1 (+ y 1))))])
           (if (or (= x 1) (= x 2))
                 1
                 (f 1 1 3)))))

(fibanocci-linear 11)
(fibanocci-linear 45)
(fibanocci-linear 20000)

(define fibonacci-memo
  (letrec ([memo null]
           [f (λ (x)
                (let ([ans (assoc x memo)])
                  (if ans
                      (cdr ans)
                      (let ([new-ans (if (or (= x 1) (= x 2))
                                         1
                                         (+ (f (- x 1)) (f (- x 2))))])
                        (begin
                          (set! memo (cons (cons x new-ans) memo))
                          new-ans)))))])
    f))
                        
                      
