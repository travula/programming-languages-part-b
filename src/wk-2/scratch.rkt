#lang racket


(define mod-5-negate
  (λ (n)
    (if (= 0 (remainder n 5))
        (- n)
        n)))

(mod-5-negate 10)
(mod-5-negate 11)

(cons (mod-5-negate 10) 11)

