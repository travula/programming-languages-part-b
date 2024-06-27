# Week 3, Type Checking

## Soundness
- A type system that ensures a program is not accepted which does X,
  for example, out of bound array references etc.
- for example, a type system is sound if it never accepts a program
  that when run with some input lookup an undeclared variable.
- A type system is sound when it prevents what it is supposed to
  prevent. That is program is flagged saying X - a property that is
  not wished - exists.
- A false negative is when the type system says it cannot accept the
  program with a certain property - X but in reality the type system
  does accept the program with such a property.
- A sound type system should not have false negatives


## Completeness
- A type system, as long as a property that is not intended is not
  exhibited, never rejects such a program. 
- A false positive is when a property that is not intended is not
  there but the type system says it exists, and therefore rejects the
  program.
- A complete type system should not have false positives.
  
