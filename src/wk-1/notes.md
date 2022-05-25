Notes on Week 1
=================

# What is learnt
- Do the same as in Part A but in a dynamically typed
  language - Racket
- Zero argument functions that lead to understanding the
  concept of streams.
- Macros - extend the syntax of a programming language


## Module Two
- Implement an interpreter
- Static vs Dynamic Typing

# Lazy Evaluation
- Evaluation of arguments of a function are evaluated before
  the function body is evaluated, which is `pass by value`.
- It is different for evaluation of `if` expression, the
  evaluation of if expression and the else expression are
  evaluated based on the evalation of the test expression.
- Therefore use `thunnks` to delay the evaluation.
  
