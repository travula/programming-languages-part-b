#lang racket

(define ones
  (λ () (cons 1 ones)))

(ones)
(car (ones)) ;;; returns 1
(cdr (ones)) ;;; gives back the ones procedure


(define nats
  (letrec ([f (λ (n) (cons n (λ() (f (+ 1 n)))))])
    (λ () (f 1))))

(nats)
(car (nats))
(cdr (nats))
(car ((cdr (nats))))
(cdr ((cdr (nats))))
(car ((cdr ((cdr (nats))))))
(cdr ((cdr ((cdr (nats))))))
(car ((cdr ((cdr ((cdr (nats))))))))

(define powers-of-two
  (letrec ([pow-2 (λ (n) (if (= n 1)
                             2
                             (* 2 (pow-2 (- n 1)))))]
           [f (λ (n) (cons (pow-2 n) (λ() (f (+ 1 n)))))])
    (λ() (f 1))))

(powers-of-two)
(car (powers-of-two))
(cdr (powers-of-two))
(car ((cdr (powers-of-two))))
(cdr ((cdr (powers-of-two))))
(car ((cdr ((cdr (powers-of-two))))))


(define number-until
  (λ(stream tester) (letrec ([f (λ (stream x)
                                  (if (tester (car (stream)))
                                      x
                                      (f (cdr (stream)) (+ x 1))))])
                      (f stream 1))))
                      
(number-until nats (λ (y) (= y 3)))
(number-until powers-of-two (λ (x) (= x 32)))


