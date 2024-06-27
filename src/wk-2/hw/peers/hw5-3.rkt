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

(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist(cdr xs)))))

  ; CHANGE (put your solutions here)

(define (mupllist->racketlist xs)
  (cond
    [(aunit? xs) null]
    [(apair? xs) (cons (apair-e1 xs)
                       (mupllist->racketlist (apair-e2 xs)))]
    [#t (error "Oh damn")]))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

; helper function
(define (add-to-env id val env)
  (cons (cons id val) env))

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
        [(apair? e)
         (apair
          (eval-under-env (apair-e1 e) env)
          (eval-under-env (apair-e2 e) env))]
        [(aunit? e) e]
        [(closure? e) e]
        [(fun? e)
         (closure env e)]
        [(fst? e)
         (let([temp (eval-under-env (fst-e e) env)])
         (if (apair? temp) 
              (apair-e1 temp)
              (error "MUPL fst applied to non-apair")))]
        
        [(snd? e)
         (let([temp (eval-under-env (snd-e e) env)]) 
         (if (apair? temp) 
              (apair-e2 temp)
              (error "MUPL fst applied to non-apair")))]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
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
         (let ([val (eval-under-env (mlet-e e) e)])
           (eval-under-env (mlet-body e) (add-to-env (mlet-var e) val env)))]
        [(call? e)
         (let ([cl (eval-under-env (call-funexp e) env)])
           (if (closure? cl)
               (let* ([c-fun (closure-fun cl)]
                      [fname (fun-nameopt c-fun)]
                      [param (fun-formal c-fun)]
                      [body (fun-body c-fun)]
                      [arg (eval-under-env (call-actual e) env)]
                      [c-env (closure-env cl)]
                      [c-env (add-to-env param arg c-env)]
                      [c-env (if fname (add-to-env fname cl (add-to-env param arg c-env)) c-env)])
                 (eval-under-env body c-env))
                 
               (error "First argument should be a closure")))]
                              
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

; Write a Racket function ifaunit that takes three mupl expressions e1, e2, and e3. It returns a
; mupl expression that when run evaluates e1 and if the result is mupl’s aunit then it evaluates e2
; and that is the overall result, else it evaluates e3 and that is the overall result. Sample solution:
; 1 line.
(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))


;Write a Racket function mlet* that takes a Racket list of Racket pairs ’((s1 . e1) . . . (si. ei)
;. . . (sn . en)) and a final mupl expression en+1. In each pair, assume si
;is a Racket string and  ei is a mupl expression.
;mlet* returns a mupl expression whose value is en+1 evaluated in an
;environment where each si
;is a variable bound to the result of evaluating the corresponding ei
;for 1 ≤ i ≤ n. The bindings are done sequentially, so that each ei
;is evaluated in an environment
;where s1 through si-1 have been previously bound to the values e1 through ei-1.
(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (caar lstlst) (cdar lstlst) (mlet* (cdr lstlst) e2))))



;Write a Racket function ifeq that takes four mupl expressions e1, e2, e3, and e4 and returns
;a mupl expression that acts like ifgreater except e3 is evaluated if and only if e1 and e2 are
;equal integers. Assume none of the arguments to ifeq use the mupl variables _x or _y. Use this
;assumption so that when an expression returned from ifeq is evaluated, e1 and e2 are evaluated
;exactly once each
(define (ifeq e1 e2 e3 e4) 
  (mlet* (list (cons "_x" e1)
               (cons "_y" e2))  
         (ifgreater (add (ifgreater (var "_x") (var "_y") (int 1) (int 0)) 
                         (ifgreater (var "_y") (var "_x") (int 1) (int 0)))
                    (int 0) e4 e3)))


;; Problem 4

;Bind to the Racket variable mupl-map a mupl function that acts like map (as we used extensively
;in ML). Your function should be curried: it should take a mupl function and return a mupl
;function that takes a mupl list and applies the function to every element of the list returning a
;new mupl list. Recall a mupl list is aunit or a pair where the second component is a mupl list.
(define mupl-map
  (fun #f "mproc"
       (fun "map" "xs" (ifaunit
                        (var "xs")
                        (aunit)
                        (apair
                         (call (var "mproc") (fst (var "xs")))
                         (call (var "map")   (snd (var "xs"))))))))
  

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i" 
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
