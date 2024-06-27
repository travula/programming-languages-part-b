---
layout: post
title:  Programming Part B Week 2 Notes
author: Thirumal
---

# What is learnt
- Data Type Programming in a Dynamically Typed Language


# Datatype Programming without Datatypes


## What does a datatype binding gives in ML:

- construction of the expression

- testers, what is the type of the expression

- accessors, to retrieve the contents of the expression

  
In a dynamically typed language, these are to built by the programmer.


## The algorithm for the `eval-exp` procedure:

- Make recursive calls,
- Get the underlying data
- Perform some operation on it
- Make an expression for the result

## How to get a the functionality of a datatype binding

- The constructor creates a list, with the first element
  being the symbol(string) representing the type and the
  value in the rest of the list.
- The checkers then work on this construction, extract the
  first element of the list and compare the first element. 
- The extrators retrieve from the rest of the list.

## Interpreters and Compilers
- An interpreter in language A takes a program in language B and
  produces answer.
- A compiler in language A takes a program in language B and produces
  equivalent program in a language Z and then uses pre-existing
  implementation of C to produce an answer.
- Interpreted vs compiler is a feature of particular
  programming-language implementation. It is not a feature of the
  programming language. 
  


# Homework

## fun
- (fun nameopt argument body), where 
   - nameopt is the name of the function represented by racket string
   - argument is  also a racket  and is the argument's name
   - body is the body of the function
   - fun gets evaluates to a closure

## closure
- (closure env fun), where
  - env is a racket list 
  - fun is MUPL function
  - closure is an argument to `call`

## call
- (call e1 e2), where
  - e1 is closure
  - e2 evaluates the value
