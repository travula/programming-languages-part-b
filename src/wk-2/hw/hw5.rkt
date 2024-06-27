;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

(define (add-to-env env mappings)
  (append mappings env))
;; Problem 1

;; CHANGE (put your solutions here)
(define racketlist->mupllist
          (λ (rls)
            (if (null? rls)
                (aunit)
                (apair (car rls) (racketlist->mupllist (cdr rls))))))

;; (define mupllist->racketlist
;;   (λ (mls)
;;     (letrec ([helper (λ (ls ms)
;;                        (if (aunit? ms)
;;                            (reverse-ls ls `())
;;                            (helper (cons (apair-e1 ms) ls) (apair-e2 ms))))]
;;              [reverse-ls (λ (ls nls)
;;                            (if (null? ls)
;;                                nls
;;                                (reverse-ls (cdr ls) (cons (car ls) nls))))])
;;       (helper `() mls))))


(define mupllist->racketlist
  (λ (mls)
    (letrec ([build-ls (λ (ms)
                         (if (aunit? ms)
                             `()
                             (cons (apair-e1 ms) (build-ls (apair-e2 ms)))))])
      (build-ls mls))))


;; Problem 2

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
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL comparison applied to non-number")))]

        [(mlet? e)
         (eval-under-env (mlet-body e) (cons (cons (mlet-var e) (eval-under-env (mlet-e e) env)) env))]
         
        [(fun? e)
         (closure env e)]

        [(closure? e) e]
         
        [(call? e)
         (let ([closure-val (eval-under-env (call-funexp e) env)] 
               [argument-val (eval-under-env (call-actual e) env)]) 
           (if (closure? closure-val) 
               (let* ([closure-environ (closure-env closure-val)] 
                      [function-exp (closure-fun closure-val)]    
                      [function-name (fun-nameopt function-exp)]
                      [function-parameter (fun-formal function-exp)]
                      [function-body (fun-body function-exp)]
                      [extended-env (if function-name
                                        (add-to-env closure-environ (list (cons function-name closure-val)
                                                                          (cons function-parameter argument-val)))
                                        (add-to-env closure-environ (list (cons function-parameter argument-val))))])
                 (eval-under-env function-body extended-env))
               (error (format "MUPL call error. First argument must be a closure. Given: ~v" e))))]

        [(apair? e) (apair (eval-under-env (apair-e1 e) env)   
                           (eval-under-env (apair-e2 e) env))] 
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)]) 
           (if (apair? v)
               (apair-e1 v)
               (error (format "the argument to fst is not a pair: ~v" e))))]
        
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)]) 
           (if (apair? v)
               (apair-e2 v)
               (error (format "the argument to snd is not a pair: ~v" e))))]

        [(aunit? e) e]
        
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env))
             (int 1)
             (int 0))]
           
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))
  

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "x" e1) (cons "y" e2)) 
         (ifgreater e1 e2
                    e4
                    (ifgreater e2 e1
                               e4
                               e3))))

;; Problem 4

(define mupl-map
  (fun "map" "f"
       (fun "iterator" "mlist"
            (ifaunit (var "mlist")
                     (aunit)
                     (apair (call (var "f") (fst (var "mlist")))
                            (call (var "iterator") (snd (var "mlist"))))))))

;;A mupl function that takes an mupl integer i and
;; returns a mupl function that takes a mupl list of mupl integers and returns a new mupl list of
;; mupl integers that adds i to every element of the list

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun "mapAddN" "i"
             (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))


;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))



;;https://github.com/bmitc/coursera-programming-languages/blob/main/part-b/week-2/hw5.rkt
