---
layout: post
title:  Programming Part B Week 1 Notes
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
 
 
# let, let* and letrec

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

Also, in `let*` we can have multiple bindings on the same variable
and the last binding takes effect, while on the other hand such
multiple binding to a sigle variable is not allowed in `let`

```
;;; This is wrong
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
   
## Hygiene

There are two parts to macro hygiene

1. Free variable in the use of macro ends up in the scope of a
   variable in the macro definition.
   
   ```
   (define-syntax double4
    (syntax-rules ()
      [(double4 e)
       (let* [(zero 0)
              [x e]]
         (+ x x zero))]))
   ```
   
   This is the definition of macro `double4`.  This macro is used this
   way.
   
   ```
   (let ([zero 17])
    (double4 zero))
   ```

   The free variable `zero` in the macro use is in the scope of the
   variable `zero` in the macro definition and the macro expands to
   syntactically to

   
   ```
   (let ([zero 17])
    (let* ([zero 0]
           [x zero])
      (+ x x zero)))
   ```
   
   giving us the result `0` instead of `34`
   
   This is resolved by racket by rewriting the definition with
   variables that are fresh and do not conflict with the varaibles in
   the program.
   
2. The second part to hygiene is when a free varible in the definition
   of the macro is in the scope of a local varible in the macro use.
   
   ```
   (define-syntax double3
     (syntax-rules ()
       [(double3 e)
         (let ([x e])
           (+ x x))]))

   (let ([+ *])
     (double3 17))
   ```

   Here, the free variable `+` in the macro definition is in the scope
   of the variable `+` in the macro use. 

   The syntactic expansion results in
   
   ```
   (let ([+ *])
    (let ([x 17])
      (+ x x)))
   ```
   
   producing `17*17` instead of `34` which is not desired.
   
   This is tackled by racket by ensuring that free variables in the
   macro definition always refer to the environment within the macro
   definition.
