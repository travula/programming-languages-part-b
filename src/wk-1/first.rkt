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

(define cube1
  (λ (x)
    (* x (* x x))))

(define pow
  (λ (x y)
    (if (= y 0)
        1
        (* x (pow x (- y 1))))))

(define cpow
  (λ (x)
    (λ (y)
      (if (= y 0)
          1
          (* x ((cpow x) (- y 1)))))))

(define ((ccpow x) y)
  (if (= y 0)
      1
      (* x ((ccpow x) (- y 1)))))

(define map
  (λ (f xs)
    (if (null? xs)
    null
    (cons (f (car xs)) (map f (cdr xs))))))

(define append
  (λ (xs ys)
    (if (null? xs)
        ys
        (cons (car xs) (append (cdr xs) ys)))))

(define sum2
  (λ (xs)
    (if (null? xs)
        0
        (+ (car xs) (sum2 (cdr xs))))))

(define check-sum
  (λ (xs)
    (if (null? xs)
        0
        (if (number? (car xs))
            (+ (car xs) (check-sum (cdr xs)))
            (+ (check-sum (car xs)) (check-sum (cdr xs)))))))

(check-sum (list 2 (list 4 5) (list (list 1 2) (list 6)) 19 (list 14 0)))

(define cond-sum
  (λ (xs)
    (cond [(null? xs) 0]
          [(number? (car xs)) (+ (car xs) (cond-sum (cdr xs)))]
          [#t (+ (cond-sum (car xs)) (cond-sum (cdr xs)))])))

(cond-sum (list 2 (list 4 5) (list (list 1 2) (list 6)) 19 (list 14 0)))

(define dynamic-sum
  (λ (xs)
    (cond [(null? xs) 0]
          [(number? xs) xs]
          [(list? xs) (+ (dynamic-sum (car xs)) (dynamic-sum (cdr xs)))]
          [#t 0])))

(dynamic-sum (list 2 #t (list 4 5) (list (list 1 2) (list 6)) 19 (list 14 0)))


(define (double-let* x)
  (let ([x (+ x 3)])
    (let ([y (+ x 2)])
      (+ x y -8))))
(double-let* 10)

(define (silly-double-let* x)
  (let* ([x (+ x 3)]
         [y (+ x 2)])
    (+ x y -8)))

(silly-double-let* 10)


(define (mod2 x)
  (letrec
      ([even? (λ (x)
                (if (zero? x)
                #t
                (odd? (- x 1))))]
       [odd? (λ (x)
               (if (zero? x)
               #f
               (even? (- x 1))))])
    (if (even? x)
        0
        1)))

(mod2 5)


(define b 5)
(define f
  (let ([b b])
    (λ (x) (* 1 (+ x b)))))

(define f!
  (λ (x) (* 1 (+ x b))))
(set! b 10)

;; (define (my-if-bad e1 e2 e3) (if e1 e2 e3))

;; (define (factorial-wrong x)
;;   (my-if-bad (= x 0)
;;              1
;;              (* x (factorial-wrong (- x 1)))))


(define (my-if e1 e2 e3) (if e1 (e2) (e3)))


(define (factorial x)
  (my-if (= x 0)
         (λ () 1)
         (λ () (* x (factorial (- x 1))))))
             

(define (my-mult x y)
  (cond [(= x 0) 0]
        [(= x 1) y]
        [#t (+ y (my-mult (- x 1) y))]))

(define (my-mult-t x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult-t (- x 1) y-thunk))]))

(my-mult-t 2 (λ () 3))

(define (my-delay f)
  (mcons #f f))

(define (my-force th)
  (cond [(mcar th) (mcdr th)]
        [#t (begin
              (set-mcar! th #t)
              (set-mcdr! th ((mcdr th)))
              (mcdr th))]))

(my-mult-t 2
         (let
             ([x (my-delay (λ () 3))])
             (λ () (my-force x))))
           
(define (my-mult-p x y-promise)
    (cond [(= x 0) 0]
        [(= x 1) (my-force y-promise)]
        [#t (+ (my-force y-promise) (my-mult-p (- x 1) y-promise))]))

(my-mult-p 2 (my-delay (λ () 3)))

(define ones (λ () (cons 1 ones)))

(define nats
  (letrec ([f (λ (x) (cons x (λ () (f (+ x 1)))))])
    (λ () (f 1))))



(define pow2
  (letrec ([f (λ (x) (cons x (λ () (f (* 2 x)))))])
    (λ () (f 2))))

;;; A higher order funtion to list a stream confirming to a tester
(define number-until
  (λ (stream tester)
    (letrec ([f (λ (stream ans)
                  (let ([pr (stream)])
                    (if (tester (car pr))
                        ans
                        (f (cdr pr) (+ 1 ans)))))])
      (f stream 1))))

(number-until nats (λ (x) (= x 10)))

(number-until pow2 (λ (x) (= x 64)))


(define stream-maker
  (λ (fn arg)
    (letrec ([f (λ (x) (cons x (λ () (f (fn x arg)))))])
      (λ () (f arg)))))

(define new-nats (stream-maker + 1))
(define new-pow2 (stream-maker * 2))
(define new-ones (stream-maker * 1))

(define fibonacci
  (λ (x)
    (cond [(= x 1) 1]
          [(= x 2) 1]
          [(+ (fibonacci(- x 1)) (fibonacci(- x 2)))])))

(define my-filter
  (λ (f xs)
    (cond [(null? xs) xs]
          [(if (f (car xs))
               (cons (car xs) (my-filter f (cdr xs)))
               (my-filter f (cdr xs)))])))

(define my-map
  (λ (f xs)
    (if (null? xs)
        xs
        (cons (f (car xs)) (my-map f (cdr xs))))))

(define my-double
  (λ (x)
    (* 2 x)))

(define greater-than-3
  (λ (x)
    (> x 3)))
    
               
(my-map my-double (list 1 2 3 4))

(my-filter greater-than-3 (list 1 2 3 4 5 6))


(define swap
  (λ (x y) (let ([x y]
                 [y x])
             (list x y))))
             
(define gcd
  (λ (x y) (if (= x y) x
               (if (< x y)
                   (gcd (- y x) x)
                   (gcd y x)))))
    

(define new-fact
  (λ (x)
    (if (= x 0)
        1
        (* x (new-fact (- x 1))))))
