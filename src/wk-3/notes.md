# Week 3, Type Checking

## Soundness and Completeness
- A sound type checker rejects a program when a property is
  met
- A complete type checker accepts a program as long as a
  given property is not exhibited.

## Soundness
- A type system that ensures a program is not accepted which
  does X, for example, out of bound array references etc.
- for example, a type system is sound if it never accepts a
  program that when run with some input looks up an
  undeclared variable.
- A type system is sound when it prevents what it is
  supposed to prevent. That is program is flagged saying X -
  a property that is not wished - exists.
- A false negative is when the type system should say it
  cannot accept the program because of the presence of a
  certain property - X but in reality the type system does
  accept the program with such a property. This happens when
  the codition for rejection is present but the type system
  does a false negative, saying the condition is not
  present.
- A sound type system should not have false negatives


## Completeness
- A type system, as long as a property that is not intended
  is not exhibited, never rejects such a program.
- A false positive is when a property that is not intended
  is not there but the type system says it exists, and
  therefore rejects the program.
- A complete type system should not have false positives.

