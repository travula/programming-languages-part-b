#lang racket

(define (my-if-bad x y z)
  (if x (y) (z)))

(define fact-wrong
  (λ (x)
    (my-if-bad (= x 0)
               (λ() 1)
               (λ() (* x (fact-wrong(- x 1)))))))

(fact-wrong 4)


