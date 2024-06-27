
#lang racket

(struct const (int) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)
(struct bool (b) #:transparent)
(struct if-then-else (e1 e2 e3) #:transparent)
(struct eq-num (e1 e2) #:transparent)

(const? (const 10))
(negate (add (const 10) (const 2)))
(negate? (negate (add (const 10) (const 2))))
(multiply (add (const 10) (negate (const 10))) (const 10))

(define bad-ast (multiply (add (const 10) "hoo") (const 10)))

(define test1 (multiply (negate (add (const 2) (const 2)))
                        (const 7)))

(define test2 (multiply (negate (add (const 2) (const 2)))
                        (if-then-else (bool #f)
                                      (const 7)
                                      (bool #t))))

(define eval-exp-wrong
  (λ (e)
    (cond [(const? e) e]
           [(negate? e) (const (- (const-int (eval-exp-wrong (negate-e e)))))]
           [(add? e)
            (let ([v1 (const-int (eval-exp-wrong (add-e1 e)))]
                  [v2 (const-int (eval-exp-wrong (add-e2 e)))])
              (const (+ v1 v2)))]
           [(multiply? e)
            (let ([v1 (const-int (eval-exp-wrong (multiply-e1 e)))]
                  [v2 (const-int (eval-exp-wrong (multiply-e2 e)))])
              (const (* v1 v2)))]
           [(eq-num? e) (let ([v1 (const-int (eval-exp-wrong (eq-num-e1 e)))]
                              [v2 (const-int (eval-exp-wrong (eq-num-e2 e)))])
                          (bool (= v1 v2)))]
           [(bool? e) e]
           [(if-then-else? e) (if (bool-b (eval-exp-wrong (if-then-else-e1 e)))
                                  (eval-exp-wrong (if-then-else-e2 e))
                                  (eval-exp-wrong (if-then-else-e3 e)))]
           [#t (error "eval-exp expected an exp")])))


(define eval-exp
  (λ (e)
    (cond [(const? e) e]
          [(negate? e)
           (let ([v (eval-exp (negate-e e))])
             (if (const? v)
                 (const (- (const-int v)))
                 (error "negate applied to non-number")))]
           [(add? e)
            (let ([v1 (eval-exp (add-e1 e))]
                  [v2 (eval-exp (add-e2 e))])
              (if (and (const? v1) (const? v2))
                  (const (+ (const-int v1) (const-int v2)))
                  (error "add applied to non-number")))]
           [(multiply? e)
            (let ([v1 (eval-exp (multiply-e1 e))]
                  [v2 (eval-exp (multiply-e2 e))])
              (if (and (const? v1) (const? v2))
                  (const (* (const-int v1) (const-int v2)))
                  (error "multiply applied to non-number")))]
           [(eq-num? e)
            (let ([v1 (eval-exp (eq-num-e1 e))]
                  [v2 (eval-exp (eq-num-e2 e))])
              (if (and (const? v1) (const? v2))
                  (bool (= (const-int v1) (const-int v2)))
                  (error "eq-num applied to non-number")))]
           [(bool? e)
            e]
           [(if-then-else? e)
            (let ([v1 (eval-exp (if-then-else-e1 e))])
              (if (bool? v1)
                  (if (bool-b v1)
                      (eval-exp (if-then-else-e2 e))
                      (eval-exp (if-then-else-e3 e)))
                  (error "if-then-else 's first argument is not boolean")))]
           [#t (error "eval-exp expected an exp")])))

(eval-exp-wrong test1)
;;(eval-exp-wrong test2) ;; this will throw an error 

;;(eval-exp test2) ;; this will throw error
(eval-exp test1)

(define e1 (if-then-else (eq-num (const 10) (const 10)) (const 20) (const 30)))
(define e2 (if-then-else (eq-num (const 10) (const 1)) (const 20) (const 30)))
(eval-exp e1)
(eval-exp e2)



