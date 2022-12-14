#lang racket

(define m-delay
  (λ (f)
    (mcons #f f)))

;;;th-dt is the think datatype
(define m-force
  (λ (th-dt)
    (if (mcar th-dt)
        (mcdr th-dt)
        (begin (set-mcar! th-dt #t)
               (set-mcdr! th-dt ((mcdr th-dt)))
               (mcdr th-dt)))))

(define m-mult
  (λ (x y-th)
    (cond [(= x 0) 0]
          [(= x 1) (y-th)]
          [#t (+ (y-th) (m-mult (- x 1) y-th))])))

(m-mult 0 (λ() 1))
(m-mult 1 (λ() 2))
(m-mult 2 (λ() 3))

(m-mult 5
        (let ([x (m-delay (λ() 3))])
            (λ () (m-force x))))

(define m-mult-p
  (λ (x y-promise)
    (cond [(= x 0) 0]
          [(= x 1) (m-force y-promise)]
          [#t (+ (m-force y-promise) (m-mult-p (- x 1) y-promise))])))

(m-mult-p 0 (m-delay (λ() 3)))
(m-mult-p 1 (m-delay (λ() 3)))
(m-mult-p 5 (m-delay (λ() 3)))
