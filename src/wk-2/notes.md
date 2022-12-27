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
- The extrators retrieve the from the rest of the list.

