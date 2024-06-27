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
(define (racketlist->mupllist racket-list)
  (if (empty? racket-list) (aunit)
      (apair (car racket-list) (racketlist->mupllist (cdr racket-list)))))

(define (mupllist->racketlist mupl-list)
  (if (aunit? mupl-list) empty
      (cons (apair-e1 mupl-list) (mupllist->racketlist (apair-e2 mupl-list)))))

;; CHANGE (put your solutions here)

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
        [(aunit? e) e]
        [(closure? e) e]
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([e1-eval (eval-under-env (ifgreater-e1 e) env)]
               [e2-eval (eval-under-env (ifgreater-e2 e) env)])
           (cond [(not (and (int? e1-eval) (int? e2-eval)))
                  (error "MUPL ifgreater applied to non-number")]
                 [(> (int-num e1-eval) (int-num e2-eval)) 
                  (eval-under-env (ifgreater-e3 e) env)]
                 [else (eval-under-env (ifgreater-e4 e) env)]))]
        [(mlet? e) 
         (let ([var-eval (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) var-eval) env)))]
        [(call? e)
         ; Evaluate call-funexp (closure) and call-actual (function's argument body)
         ; Error if funexp was not a closure
         ; With the function inside the closure, evaluate the body with an environment
         ; where the function is available as the closure (unless function name is #f)
         ; and argument is available with actual's evaluation
         (let* ([closure-eval (eval-under-env (call-funexp e) env)]
                ; Only evaluate if closure-v was a closure
                [fun-arg-eval (if (closure? closure-eval) (eval-under-env (call-actual e) env) #f)])
           ; closure? should be cheap enough to not care about re-evaulating
           (if (not (closure? closure-eval))
               (error "MUPL call, funexp evaluation was not a closure")
               (let* ([function (closure-fun closure-eval)]
                      [function-name (fun-nameopt function)]
                      [extended-env (if function-name
                                       (cons (cons function-name closure-eval)
                                             (closure-env closure-eval))
                                       (closure-env closure-eval))]
                      [extended-env (cons (cons (fun-formal function) fun-arg-eval) extended-env)])
                 (eval-under-env (fun-body function) extended-env))))]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env)
                (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([fst-eval (eval-under-env (fst-e e) env)])
           (if (apair? fst-eval) 
               (apair-e1 fst-eval)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([snd-eval (eval-under-env (snd-e e) env)])
           (if (apair? snd-eval) 
               (apair-e2 snd-eval)
               (error "MUPL snd applied to non-apair")))]
        [(isaunit? e) (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (empty? lstlst) e2
      (mlet (caar lstlst)
            (cdar lstlst)
            (mlet* (cdr lstlst) e2))))

; Using only mupl constructs
; (x > y) + (y > x)
; If both were not greater, they're equal, this gives back 0
; If one was greater, they're not equal, this gives back 1
; Comparing that to 0, if greater (they were not equal (1 > 0)) e4
; if not greater (they were equal (0 > 0)) e3
(define (ifeq e1 e2 e3 e4)
  (let ([x "_x"]
        [y "_y"])
    (mlet* (list (cons x e1) (cons y e2))
           (ifgreater (add (ifgreater (var x) (var y) (int 1) (int 0))
                           (ifgreater (var y) (var x) (int 1) (int 0)))
                      (int 0) e4 e3))))

;; Problem 4

; Take a function mapper,
; return a function that takes a mlist
; applies mapper onto every element of map-list
(define mupl-map
  (let ([mapper "__mapper__"]
        [map-fn "__map__"]
        [map-list "__list__"])
    (fun #f mapper
         (fun map-fn map-list
              (ifaunit (var map-list)
                       (aunit)
                       (apair (call (var mapper) (fst (var map-list)))
                              (call (var map-fn) (snd (var map-list)))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "n" (call mupl-map (fun #f "e" (add (var "n") (var "e")))))))

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
