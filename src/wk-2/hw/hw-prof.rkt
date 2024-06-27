;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file
# Problem 1
;; General Instructions
;; Mutation, such as 
;; set!
;; set! or 
;; set-mcar!
;; set-mcar!, is generally poor style, so give at most a 3 to solutions using mutation.

;; Problem 1 
;; Here is a sample solution:
(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))))

(define (mupllist->racketlist xs)
  (if (aunit? xs)
      null
      (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))))

(define (eval-under-env e env)
  (cond [(var? e) ...]
        [(int? e) ...]
        [(add? e) ...]
        [(ifgreater? e) ...]
        [(fun? e) ...]
        [(call? e) ...]
        [(mlet? e) ...]
        [(apair? e) ...]
        [(fst? e) ...]
        [(snd? e) ...]
        [(aunit? e) ...]
        [(isaunit? e) ...]
        [(closure? e) ...]
        [#t ...]))


[(var? e) (envlookup env (var-string e))]

[(int? e) e]

[(aunit? e) e]

[(closure? e) e]

[(fun? e) (closure env e)]


         [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
             
        [(ifgreater? e) 
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]


        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env)
                (eval-under-env (apair-e2 e) env))]

        [(fst? e)
         (let ([pr (eval-under-env (fst-e e) env)])
           (if (apair? pr)
               (apair-e1 pr)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([pr (eval-under-env (snd-e e) env)])
           (if (apair? pr)
               (apair-e2 pr)
               (error "MUPL snd applied to non-pair")))]


        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v) (int 1) (int 0)))]


        [(mlet? e) 
         (let* ([v (eval-under-env (mlet-e e) env)]
                [newenv (cons (cons (mlet-var e) v) env)]) 
           (eval-under-env (mlet-body e) newenv))]


        [(call? e)
         (let ([cl  (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? cl)
               (let* ([fn (closure-fun cl)]
                      [bodyenv (cons (cons (fun-formal fn) arg)
                                     (closure-env cl))]
                      [bodyenv (if (fun-nameopt fn)
                                   (cons (cons (fun-nameopt fn) cl)
                                         bodyenv)
                                   bodyenv)])
                 (eval-under-env (fun-body fn) bodyenv))
               (error "MUPL function call with nonfunction")))]


(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* bs e2) 
  (cond [(null? bs) e2]
        [#t (mlet (car (car bs)) (cdr (car bs))
                  (mlet* (cdr bs) e2))]))

(define (ifeq e1 e2 e3 e4) 
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y")
                         e4
                         (ifgreater (var "_y") (var "_x")
                                    e4
                                    e3)))))

  (fun "map" "f"
       (fun #f "xs"
            (ifaunit (var "xs")
                     (aunit)
                     (apair (call (var "f") (fst (var "xs")))
                            (call (call (var "map") (var "f"))
                                  (snd (var "xs"))))))))

(define mupl-mapAddN
  (mlet "map" mupl-map 
        (fun #f "x"
             (call (var "map") (fun #f "y" 
                                    (add (var "x") (var "y")))))))

(define (compute-free-vars e)
  (struct res (e fvs)) ; result type of f (could also use a pair)
    (define (f e) 
      (cond [(var? e) (res e (set (var-string e)))]
            [(int? e) (res e (set))]
            [(add? e) (let ([r1 (f (add-e1 e))]
                            [r2 (f (add-e2 e))])
                        (res (add (res-e r1) (res-e r2))
                             (set-union (res-fvs r1) (res-fvs r2))))]
            [(ifgreater? e) (let ([r1 (f (ifgreater-e1 e))]
                                  [r2 (f (ifgreater-e2 e))]
                                  [r3 (f (ifgreater-e3 e))]
                                  [r4 (f (ifgreater-e4 e))])
                              (res (ifgreater (res-e r1) (res-e r2) (res-e r3)    (res-e r4))
                                  (set-union (res-fvs r1) (res-fvs r2) (res-fvs   r3) (res-fvs r4))))]
            [(fun? e) (let* ([r (f (fun-body e))]
                             [fvs (set-remove (res-fvs r) (fun-formal e))]
                             [fvs (if (fun-nameopt e) 
                                      (set-remove fvs (fun-nameopt e)) 
                                      fvs)])
                        (res (fun-challenge (fun-nameopt e) (fun-formal e) 
                                            (res-e r) fvs)
                            fvs))]
            [(call? e) (let ([r1 (f (call-funexp e))]
                             [r2 (f (call-actual e))])
                        (res (call (res-e r1) (res-e r2))
                             (set-union (res-fvs r1) (res-fvs r2))))]
            [(mlet? e) (let* ([r1 (f (mlet-e e))]
                              [r2 (f (mlet-body e))])
                         (res (mlet (mlet-var e) (res-e r1) (res-e r2))
                              (set-union (res-fvs r1) (set-remove (res-fvs r2)   (mlet-var e)))))]
            [(apair? e) (let ([r1 (f (apair-e1 e))]
                              [r2 (f (apair-e2 e))])
                          (res (apair (res-e r1) (res-e r2))
                             (set-union (res-fvs r1) (res-fvs r2))))]
            [(fst? e) (let ([r (f (fst-e e))])
                        (res (fst (res-e r))
                             (res-fvs r)))]
            [(snd? e) (let ([r (f (snd-e e))])
                        (res (snd (res-e r))
                             (res-fvs r)))]
            [(aunit? e) (res e (set))]
            [(isaunit? e) (let ([r (f (isaunit-e e))])
                            (res (isaunit (res-e r))
                                 (res-fvs r)))]))
    (res-e (f e)))
                      
(define (eval-under-env-c e env) 
  (cond 
        [(fun-challenge? e)
         (closure (set-map (fun-challenge-freevars e)
                           (lambda (s) (cons s (envlookup env s))))
                  e)]
         ; call case uses fun-challenge as appropriate
         ; all other cases the same
        ...)

(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
