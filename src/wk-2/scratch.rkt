
#lang racket


(define mod-5-negate
  (λ (n)
    (if (= 0 (remainder n 5))
        (- n)
        n)))

(mod-5-negate 10)
(mod-5-negate 11)

(cons (mod-5-negate 10) 11)


(define factorial
  (λ (n)
    (if (= n 1)
        1
        (* n (factorial (- n 1))))))
        
