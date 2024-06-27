#lang racket


(struct const (int) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)

(const-int (const 10)) ; returns 10
(add-e1 (add (const 10) (const 11))) ;; returns (const 10)

(define eval-exp
  (λ (e)
    (cond [(const? e)  e]
          [(negate? e) (const (- (const-int (eval-exp (negate-e e)))))]
          [(add? e) (let ([v1 (const-int (eval-exp (add-e1 e)))]
                          [v2 (const-int (eval-exp (add-e2 e)))])
                      (const (+ v1 v2)))]
          [(multiply? e) (let ([v1 (const-int (eval-exp (multiply-e1 e)))]
                               [v2 (const-int (eval-exp (multiply-e2 e)))])
                           (const (* v1 v2)))])))


(let* ([exp1 (const 1)]
       [exp2 (negate exp1)]
       [exp3 (const 2)]
       [exp4 (add exp2 exp3)]
       [exp5 (multiply exp1 exp4)]
       [test-exp (multiply (negate (add (const 2) (const 2))) (const 7))])
  (eval-exp test-exp))

