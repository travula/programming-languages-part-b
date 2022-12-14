---
layout: post
title:  Programming Part B Week 1
author: Thirumal
---


# What is learnt
- Do the same as in Part A but in a dynamically typed language -
  Racket
- Zero argument functions that lead to understanding the concept of
  streams.
- Macros - extend the syntax of a programming language


## Module Two
- Implement an interpreter
- Static vs Dynamic Typing

# Lazy Evaluation
Evaluation of arguments of a function are evaluated before the
function body is evaluated, which is `pass by value`.  It is different
for evaluation of `if` expression, the evaluation of if expression and
the else expression are evaluated based on the evaluation of the test
expression.

How variables are looked up in an environment - the bindings - is a
fundamental feature of a programming language.

Understanding the semantics of evaluation of subexpressions of a
programming language is paramount.  For example, in racket, given (e1
e2 ... en), e2 till en are evaluated only once before the function
body is evaluated, and give the function (lambda (...) ...), the body
is evaluated only when the function is called.

Therefore in the code snippet below,

```
(define (my-if-bad e1 e2 e3) (if e1 e2 e3))

(define (factorial-wrong x)
  (my-if-bad (= x 0)
             1
             (* x (factorial-wrong (- x 1)))))
```

the three arguments to the function `factorial-wrong` are evaluated
and therefore `factorial-wrong` never terminates.  The branching in a 

The evaluation of expressions in an `if` is different.  `(if e1 e2
e3)`, based on the result of evaluation of `e1`, either `e2` or `e3`
is evaluated.

To overcome the evaluation of the arguments, the property of
functions - body evaluated only upon function call - is exploited.
Therefore, to the above if, the arguments are passed as `0` argument
functions, called `thunks`.

```
(define (my-if x y z) (if x (y) (z)))

(define (factorial x)
  (my-if (= x 0)
         (λ () 1)
         (λ () (* x (factorial (- x 1))))))
```

# Delayed Evaluation

1. All the arguments are evaluated before the function body is
   evluated.  Use thunks to circumvent this.
   
   ```
   (define (my-if-bad e1 e2 e3) (if e1 e2 e3))
   (define (my-if e1 e2-th e3-th) (if e1 (e2-th) (e3-th)))
   ```
   
   In above code snippet, in `my-if-bad` e2 and e3 are evaluated
   before the function body though in the body, based upon the
   evaluation of e1, either e2 or e3 is evaluated.  This can lead to
   undesired consequences of non termination in a recursive call where
   the base case would never be reached, due to the evaluation of both
   e2 and e3.

   This is rectified in `my-if`, where e2 and e3 are thunked to create
   e2-th and e3-th where e2 and e3 are wrapped in `0` argument
   functions.
   
   ```
   (define e2-th (λ () e2)
   (define e3-th (λ () e3)
   ```
   
2. Using thunks design promises

   The need for promises is to avaoid repeated evaluations.  This is
   achieved by storing the value of the first evaluation and returning
   upon further evaluations.  The `delay` function returns a `promise`
   while the `force` function evalutes the function stored in the
   `promise` based upon the flag set in the `promise`.  The function
   stored in the promise is always a thunk.
   
   ```
   (define (my-mult-t x y-thunk)
     (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult-t (- x 1) y-thunk))]))
		
   (define (my-delay f)
     n(mcons #f f))

	(define (my-force th)
	  (cond [(mcar th) (mcdr th)]
        [#t (begin
              (set-mcar! th #t)
              (set-mcdr! th ((mcdr th)))
              (mcdr th))]))
   ```
   
   In the above code, the second argumet to the function `my-mult` is
   a thunk. Therefore, the `force` is wrapped in a thunk and passed to
   it.  The argument to the `force` function is the output of the
   `delay` which is a promise.
   
   
   ```
   (my-mult-t 2
         (let
             ([x (my-delay (λ () 3))])
             (λ () (my-force x))))
   ```
   
   This is generalised by the following implementaion
   
   ```
   (define (my-mult-p x y-promise)
    (cond [(= x 0) 0]
        [(= x 1) (my-force y-promise)]
        [#t (+ (my-force y-promise) (my-mult-p (- x 1) y-promise))]))

   (my-mult-p 2 (my-delay (λ () 3)))

   ```
   
   There is a need to observe the follwing facts:
   
   1. The second argument to `delay` is always a thunk to avoid
      evaluation by the `delay` itself. 
   2. The second argument to `mult` is also always a thunk before
      `promise` is used as the second argument.
	  
# Streams
 Streams do a division of labor. One part knows how to produce
 successive values in an infinite sequence but does not know how many
 will be needed, and/or what to do with them.  The generator just
 generates them but has no idea where to stop or what to do with them.
 The other part - consumer - exactly knows how many are needed and
 what to do with them but has no clue about the generation.  For
 example, as sequence user events on a browser: one part captures
 these events while the other part receives them and processes them.

 Therefore, the stream is a thunk when applied gives a pair containing
 the event (element) and the stream itself.  It is upto consumer what
 to do with the event and when to stop.
 
 
# let, let* and letrc

In `let` the previous binding is not available to the evaluation of
the expressions in the later bindings.  Each of the expressions
evaluate in an environment where all the bindings in the let are not
yet available.

```
(let ([b1 e1]
      [b2 e2]
	  ...
	  ...
	  [bn en])

```

In the evaluation of all the expressions e1...en, the bindings b1...bn
are not available.


While in `let*`, the environment available for evaluation of
expressions e1...en, contains the previous bindings in `let*`, i.e.,
b1 is in the environment for expressions from e2 till en, b2 is in the
environment for expressions from e3 till en etc.

Both in let and let*, the later bindings are not availble in the
environment of the previous expressions.

Also, in `let*`we can multiple bindings on the same variable and the
last binding takes effect, while on the other hand such multiple
binding to a sigle variable is not allowed in `let`

```
;;; This iw wrong
(let ([hd 0]
      [hd 1])
  hd)

;;; This is allowed and returns 1
(let* ([hd 0]
       [hd 1])
  hd)
```

# Macros

The macros should be wary of:
1. Tokenization: This is the process by which the program is split
   into tokens for parsing by the interpreter or the compiler.
   Therefore implementation of macros should understad how the
   programming languages break the program into tokens.
   
2. Parenthesis: When the evaluation of an expression depends on the
   parenthesis around them.
   
3. Variable Bindings: The macro expansion in variable bindings impact
   the bindings.  In the below snippet, when the replacement for `tl`
   is `hd`, then the `let` would fail since we cannot bind twice the
   same variable.  Therefore the design choice is to mask the macros
   from local bindings.
   
   ```
   (let ([hd 0]
         [tl 1])
	 hd)
   ```




	



