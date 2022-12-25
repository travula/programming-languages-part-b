
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define sequence
  (λ (low high stride)
    (letrec ([f (λ (step)
                  (if (> (+ low (* stride step)) high)
                      null
                      (cons (+ low (* stride step)) (f (+ step 1)))))])
      (f 0))))


(define string-append-map
  (λ (xs suffix)
    (map (λ (x)
           (string-append x suffix)) xs)))

(define list-nth-mod
  (λ (xs n)
    (cond [(< n 0) (error "list-nth-mod: negative number")]
          [(null? xs) (error "list-nth-mod: empty list")]
          [#t (car (list-tail xs (remainder n (length xs))))])))

(define stream-for-n-steps
  (λ (s n)
    (letrec ([helper (λ (s n)
                       (cond [(= 0 n) '()]
                             [#t (let ([de-thunked (s)])
                                   (cons (car de-thunked) (helper (cdr de-thunked) (- n 1))))]))])
      (helper s n))))

(define funny-number-stream
  (letrec ([mod-5-negate (λ (n)
                           (if (= 0 (remainder n 5))
                               (- n)
                               n))]
           [f (λ (n)
                (cons (mod-5-negate n) (λ() (f (+ n 1)))))])
    (λ() (f 1))))


(define dan-then-dog
  (letrec ([get-next (λ (str) (if (string=? str "dog.jpg") "dan.jpg" "dog.jpg"))]
           [f (λ (str)
                (let* ([n-str (get-next str)])
                  (cons n-str (λ () (f n-str)))))])
    (λ () (f "dog.jpg"))))

(define stream-add-zero
  (λ (s)
    (letrec ([f (λ (s)
                  (let ([de-thunked (s)])
                    (cons (cons 0 (car de-thunked)) (λ () (f (cdr de-thunked))))))])
      (λ() (f s)))))

(define cycle-lists
  (λ (xs ys)
    (letrec ([get-el (λ (ls n)
                       (list-ref ls (remainder n (length ls))))]
             [helper (λ (n)
                       (cons (get-el xs n) (get-el ys n)))]
             [f (λ (n)
                  (cons (helper n) (λ() (f (+ n 1)))))])
      (λ() (f 0)))))

(define vector-assoc
  (λ (v vec)
    (letrec ([helper (λ (ref)
                       (cond [(not (vector? vec)) #f]
                             [(= ref (vector-length vec)) #f]
                             [(not (pair? (vector-ref vec ref))) (helper (+ 1 ref))]
                             [(equal? v (car (vector-ref vec ref))) (vector-ref vec ref)]
                             [#t (helper (+ 1 ref))]))])
      (helper 0))))

(define cached-assoc
  (λ (xs n)
    (letrec ([insert-record (λ (rec)
                              (vector-set! vec pos rec))]
             [vec (make-vector n)]
             [pos 0]
             [f (λ (v)
                  (if (number? n)
                      (let* ([cached-result (vector-assoc v vec)])
                        (if cached-result
                            (begin
                              (print "vassoc")
                              cached-result)
                            (let* ([register-result (assoc v xs)])
                              (if register-result
                                  (begin
                                    (print "assoc")
                                    (insert-record register-result)
                                    (set! pos (remainder (+ 1 pos) n))
                                    register-result)
                                  #f))))
                      #f))])
      f)))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([e1-val e1]
              [loop (λ ()
                      (let ([e2-val e2])
                        (if (< e2-val e1-val)
                            (loop)
                            #t)))])
       (loop))]))
