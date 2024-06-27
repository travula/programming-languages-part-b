#lang racket

(define my-delay
  (λ (f)
    (mcons #f f)))

(define my-force
  (λ (th-ls)
    (if (mcar th-ls)
        (mcdr th-ls)
        (begin (set-mcar! th-ls #t)
               (set-mcdr! th-ls ((mcdr th-ls)))
               (mcdr th-ls)))))

(define my-mult
  (λ (e1 e2)
    (cond [(= 0 e1) 0]
          [(= 1 e1) e2]
          [#t (+ e2 (my-mult (- e1 1) e2))])))

(my-mult 1 (- 10 8))

(define my-mult-t
  (λ (e1 e2-t)
    (cond [(= 0 e1) 0]
          [(= 1 e1) (e2-t)]
          [#t (+ (e2-t) (my-mult-t (- e1 1) e2-t))])))

(my-mult-t 5 (λ() (- 10 8)))
(my-mult-t 5 (let ([e2-t (my-delay (λ() (- 10 8)))])
               (λ() (my-force e2-t))))

(define my-mult-promise
 (λ (e1 promise)
   (cond [(= 0 e1) 0]
         [(= 1 e1) (my-force promise)]
         [#t (+ (my-force promise) (my-mult-promise (- e1 1) promise))])))

(my-mult-promise 5 (my-delay (λ() (- 10 8))))

(define nats
  (letrec ([f (λ(x) (cons x (λ() (f (+ x 1)))))])
    (λ() (f 1))))

(define powers-of-2
  (letrec ([f (λ(x) (cons x (λ() (f (* x 2)))))])
    (λ() (f 2))))

(define stream-maker
  (λ(fn arg)
    (letrec ([f (λ(x) (cons x (λ() (f (fn x arg)))))])
      (λ() (f arg)))))

(define number-until
  (λ(stream tester)
    (letrec ([f (λ(stream ans)
                  (let ([pr (stream)])
                    (if (tester (car pr))
                        ans
                        (f (cdr pr) (+ ans 1)))))])
      (f stream 1))))

(number-until nats (λ(x) (= x 4)))
(number-until powers-of-2 (λ(x) (= x 16)))
                  
(define fibonacci-r
  (λ(n)
    (if (or (= n 1) (= n 2))
        1
        (+ (fibonacci-r (- n 1)) (fibonacci-r (- n 2))))))

(define fib-a
  (λ(x)
    (letrec ([f (λ(acc1 acc2 y)
                  (if (= x y)
                      (+ acc1 acc2)
                      (f acc2 (+ acc1 acc2) (+ 1 y))))])
      (if (or (= x 1) (= x 2))
          1
          (f 1 1 3)))))

(define fib
  (letrec ([memo null]
           [f (λ(x)
                (let ([ans (assoc x memo)])
                  (if ans
                      (cdr ans)
                      (let ([new-ans (if (or (= x 1) (= x 2))
                                         1
                                         (+ (f (- x 1))
                                            (f (- x 2))))])
                        (begin
                          (set! memo (cons (cons x new-ans) memo))
                          new-ans)))))])
    f))
                      
                
            
