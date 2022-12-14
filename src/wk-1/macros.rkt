#lang racket

(define-syntax my-if
  (syntax-rules (then else)
    [(my-if e1 then e2 else e3)
     (if e1 e2 e3)]))


(my-if #t then 10 else 20)
(my-if #f then 10 else 20)

(define-syntax comment-out
  (syntax-rules ()
    [(comment-out e1 e2) e2]))

(define e1
  (λ () 10))

(define e2
  (λ () 11))

(comment-out e1 e2)

((comment-out e1 e2))

(define-syntax my-delay
  (syntax-rules ()
    [(my-delay f)
     (mcons #f (λ() f))]))

(my-delay (λ(x) (+ x 1)))
(mcar (my-delay (λ(x) (+ x 1))))
(mcdr (my-delay (λ(x) (+ x 1)))) ;;; returns the think
((mcdr (my-delay (λ(x) (+ x 1))))) ;;; return the f
(((mcdr (my-delay (λ(x) (+ x 1))))) 10) ;;; apply f with argument 10

(define-syntax my-force
  (syntax-rules ()
    [(my-force e)
     (if (mcar e)
         (mcdr e)
         (begin (set-mcar! e #t)
                (set-mcdr! e ((mcdr e)))
                (mcdr e)))]))
                
(let ([t (begin (print "hi") (my-delay (λ(x) (+ x 1))))])
  (my-force t))

;;; this print hi 5 times
(my-force (begin (print "hi") (my-delay (λ(x) (+ x 1)))))


;;; to correct the above behavior
(define-syntax my-force-no-side-effect
  (syntax-rules ()
    [(my-force e)
     (let ([x e])
       (if (mcar x)
           (mcdr x)
           (begin (set-mcar! x #t)
                  (set-mcdr! x ((mcdr x)))
                  (mcdr x))))]))

;;; this will print hi only once
(my-force-no-side-effect (begin (print "hi") (my-delay (λ(x) (+ x 1)))))

(define my-force-orig
  (λ(e)
    (if (mcar e)
        (mcdr e)
        (begin (set-mcar! e #t)
               (set-mcdr! e ((mcdr e)))
               (mcdr e)))))

(my-force-orig (begin (print "hi") (my-delay (λ(x) (+ x 1)))))

(define-syntax double4
  (syntax-rules ()
    [(double4 e)
     (let* [(zero 0)
            [x e]]
       (+ x x zero))]))

(let ([zero 17])
  (double4 zero))

;;; this is the syntactic equivalent version of double4
;;; but the resilts are different, since racket rewrites the
;;; variables withing the scope of the macro not to conflict with
;;; anything in the program
(let ([zero 17])
  (let* ([zero 0]
         [x zero])
    (+ x x zero)))

(define-syntax double3
  (syntax-rules ()
    [(double3 e)
     (let ([x e])
       (+ x x))]))

(double3 10)

(let ([+ *])
  (double3 17))

(let ([+ *])
  (let ([x 17])
    (+ x x)))

(define-syntax let2
  (syntax-rules ()
    [(let2 () body)
     body]
    [(let2 (var1 val1) body)
     (let ([var1 val1])
       body)]
    [(let2 (var1 val1 var2 val2) body)
     (let ([var1 val1])
       (let ([var2 val2])
           body))]))
           
(let2 () 10)
(let2 (x 10) (+ x x))
(let2 (x 10 y 20) (+ x y))


(define-syntax my-let*
  (syntax-rules ()
    [(my-let* () body)
     body]
    [(my-let* ([var1 val1]
               [var-rest val-rest] ...)
              body)
     (let ([var1 val1])
       (my-let* ([var-rest val-rest] ...)
                body))]))

(my-let* ([x 10]
          [y x])
         (+ x y))

;;; it is not 20 + 20, instead it is 20 + 10
(let ([x 10])
  (let ([x 20]
        [y x])
    (+ x y)))


(define-syntax for
  (syntax-rules (to do)
    [(for lo to hi body)
     (let ([l lo]
           [h hi])
       (letrec ([loop (λ (it)
                        (if (> it hi)
                            #t
                            (begin body (loop (+ 1 hi)))))])
         (loop l)))]))
                        
                            
(for 1 to 2 (print "hi"))

