;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist values)
  (foldr (lambda (value acc) (apair value acc)) (aunit) values))

(define (mupllist->racketlist values)
  (if (aunit? values)
      null
      (cons (apair-e1 values) (mupllist->racketlist (apair-e2 values)))))

;; problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]

        ;; all the values evaluates to itself
        [(or (int? e)
             (aunit? e)
             (closure? e)) e]
        
        ;; function evaluates to closure 
        [(fun? e) (closure env e)]

        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env)
                (eval-under-env (apair-e2 e) env))]

        [(fst? e)
         (let [(exp (eval-under-env (fst-e e) env))]
           (if (apair? exp)
               (apair-e1 exp)
               (error "MUPL fst applied to non-pair")))]

        [(snd? e)
         (let [(exp (eval-under-env (snd-e e) env))]
           (if (apair? exp)
               (apair-e2 exp)
               (error "MUPL snd applied to non-pair")))]

        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env))
             (int 1)
             (int 0))]
        
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        
        [(ifgreater? e)
         (let [(v1 (eval-under-env (ifgreater-e1 e) env))
               (v2 (eval-under-env (ifgreater-e2 e) env))]
           (cond [(not (and (int? v1)
                            (int? v2)))           (error "MUPL ifgreater applied to non-number")]
                 [(> (int-num v1) (int-num v2))   (eval-under-env (ifgreater-e3 e) env)]
                 [#t                              (eval-under-env (ifgreater-e4 e) env)]))]

        [(mlet? e)
         (let* [(v (eval-under-env (mlet-e e) env))
                (ext-env (cons (cons (mlet-var e) v) env))]
           (eval-under-env (mlet-body e) ext-env))]

        [(call? e)
         (let [(closure (eval-under-env (call-funexp e) env))
               (arg (eval-under-env (call-actual e) env))]
           (if (closure? closure)         
               (let* [(fun (closure-fun closure))
                      (env (closure-env closure))
                      (fun-name (fun-nameopt fun))
                      (parameter (fun-formal fun))
                      (body (fun-body fun))
                      (ext-env (cons (cons parameter arg) env))
                      (ext-env
                       (if fun-name
                           (cons (cons fun-name closure) ext-env)
                           ext-env))]
                 (eval-under-env body ext-env))
               (error "MUPL call applied to non-closure")))]
        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (foldr (lambda (pr body) (mlet (car pr) (cdr pr) body)) e2 lstlst))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x")
                    (var "_y")
                    e4
                    (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map
  (fun #f "f"
       (fun "map" "xs"
            (ifaunit (var "xs")
                     (aunit)
                     (apair (call (var "f")   (fst (var "xs")))
                            (call (var "map") (snd (var "xs"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (fun #f "xs" (call (call (var "map")
                                      (fun #f "x" (add (var "x") (var "i"))))
                                (var "xs"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (letrec
      [(compute-free-vars
        (lambda (e)
          (cond [(var? e) (cons e (set (var-string e)))]
                
                [(int? e) (cons e (set))]

                [(aunit? e) (cons e (set))]
                
                [(add? e)
                 (let* [(p1 (compute-free-vars (add-e1 e)))
                        (p2 (compute-free-vars (add-e2 e)))
                        (exp (add (car p1) (car p2)))
                        (freevars (set-union (cdr p1) (cdr p2)))]
                   (cons exp freevars))]
                
                [(ifgreater? e)
                 (let* [(p1 (compute-free-vars (ifgreater-e1 e)))
                        (p2 (compute-free-vars (ifgreater-e2 e)))
                        (p3 (compute-free-vars (ifgreater-e3 e)))
                        (p4 (compute-free-vars (ifgreater-e4 e)))
                        (exp (ifgreater (car p1) (car p2) (car p3) (car p4)))
                        (freevars (set-union (cdr p1) (cdr p2) (cdr p3) (cdr p4)))]
                   (cons exp freevars))]
                
                [(call? e)
                 (let* [(p1 (compute-free-vars (call-funexp e)))
                        (p2 (compute-free-vars (call-actual e)))
                        (exp (call (car p1) (car p2)))
                        (freevars (set-union (cdr p1) (cdr p2)))]
                   (cons exp freevars))]
                
                [(mlet? e)
                 (let* [(p1 (compute-free-vars (mlet-e e)))
                        (p2 (compute-free-vars (mlet-body e)))
                        (exp (mlet (mlet-var e) (car p1) (car p2)))
                        (freevars (set-remove (set-union (cdr p1) (cdr p2)) (mlet-var e)))]
                   (cons exp freevars))]
                
                [(apair? e)
                 (let* [(p1 (compute-free-vars (apair-e1 e)))
                        (p2 (compute-free-vars (apair-e2 e)))
                        (exp (apair (car p1) (car p2)))
                        (freevars (set-union (cdr p1) (cdr p2)))]
                   (cons exp freevars))]
                
                [(fst? e)
                 (let* [(p (compute-free-vars (fst-e e)))
                        (exp (fst (car p)))
                        (freevars (cdr p))]
                   (cons exp freevars))]
                
                [(snd? e)
                 (let* [(p (compute-free-vars (snd-e e)))
                        (exp (snd (car p)))
                        (freevars (cdr p))]
                   (cons exp freevars))]
                        
                [(isaunit? e)
                 (let* [(p (compute-free-vars (isaunit-e e)))
                        (exp (isaunit (car p)))
                        (freevars (cdr p))]
                   (cons exp freevars))]
                
                [(closure? e)
                 (let* [(p (compute-free-vars (closure-fun e)))
                        (exp (closure (closure-env e) (car p)))
                        (freevars (cdr p))]
                   (cons exp freevars))]
                
                [(fun? e)
                 (let* [(p (compute-free-vars (fun-body e)))
                        (freevars (set-remove (set-remove (cdr p) (fun-formal e)) (fun-nameopt e)))
                        (exp (fun-challenge (fun-nameopt e) (fun-formal e) (car p) freevars))]
                   (cons exp freevars))]
                
                [#t (error (format "bad MUPL expression: ~v" e))])))]
    (car (compute-free-vars e))))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]

        ;; all the values evaluates to itself
        [(or (int? e)
             (aunit? e)
             (closure? e)) e]
        
        ;; function evaluates to closure 
        ;[(fun-challenge? e) (closure (set->list (fun-challenge-freevars e)) e)]
        [(fun-challenge? e)
         (closure (filter (lambda (p) (set-member? (fun-challenge-freevars e) (car p))) env) e)]

        [(apair? e)
         (apair (eval-under-env-c (apair-e1 e) env)
                (eval-under-env-c (apair-e2 e) env))]

        [(fst? e)
         (let [(exp (eval-under-env-c (fst-e e) env))]
           (if (apair? exp)
               (apair-e1 exp)
               (error "MUPL fst applied to non-pair")))]

        [(snd? e)
         (let [(exp (eval-under-env-c (snd-e e) env))]
           (if (apair? exp)
               (apair-e2 exp)
               (error "MUPL snd applied to non-pair")))]

        [(isaunit? e)
         (if (aunit? (eval-under-env-c (isaunit-e e) env))
             (int 1)
             (int 0))]
        
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        
        [(ifgreater? e)
         (let [(v1 (eval-under-env-c (ifgreater-e1 e) env))
               (v2 (eval-under-env-c (ifgreater-e2 e) env))]
           (cond [(not (and (int? v1)
                            (int? v2)))           (error "MUPL ifgreater applied to non-number")]
                 [(> (int-num v1) (int-num v2))   (eval-under-env-c (ifgreater-e3 e) env)]
                 [#t                              (eval-under-env-c (ifgreater-e4 e) env)]))]

        [(mlet? e)
         (let* [(v (eval-under-env-c (mlet-e e) env))
                (ext-env (cons (cons (mlet-var e) v) env))]
           (eval-under-env-c (mlet-body e) ext-env))]

        [(call? e)
         (let [(closure (eval-under-env-c (call-funexp e) env))
               (arg     (eval-under-env-c (call-actual e) env))]
           (if (closure? closure)         
               (let* [(fun       (closure-fun closure))
                      (env       (closure-env closure))
                      (fun-name  (fun-challenge-nameopt fun))
                      (parameter (fun-challenge-formal fun))
                      (body      (fun-challenge-body fun))
                      (ext-env   (cons (cons parameter arg) env))
                      (ext-env
                       (if fun-name
                           (cons (cons fun-name closure) ext-env)
                           ext-env))]
                 (eval-under-env-c body ext-env))
               (error "MUPL call applied to non-closure")))]
        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))





















