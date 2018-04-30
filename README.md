# Failing Gracefully with EitherT

Code to accompany the Lambda Jam 2018 talk, 'Failing Gracefully with EitherT'.

## Presentation structure

### Motivation

Exceptions are an important part of how GHC Haskell works. It is inevitable that
you will have to deal with them at some point. But there are several reasons why
this can be tricky:

* It can be hard to intuitively grasp how exceptions work in Haskell, so it is
  easy to make mistakes when writing exception-handling code. This is mitigated
  somewhat by Michael Snoyman's `safe-exceptions` package.
* Exceptions do not show up in type signatures. You usually need to either read
  the source or experiment to work out what exceptions a function can throw. The
  compiler will not help you if you miss exceptions, or handle execptions of the
  wrong type.
* Exceptions only provide local error context. If you want exceptions to add
  contextual information from different locations (i.e. from nested function
  calls), then you'll need to re-throw and catch them in several places, adding
  opportunities to make mistakes.
* Because GHC doesn't always provide stack traces with exeptions, debugging
  unhandled exceptions can be difficult.

### Solution (EitherT)

In Haskell, we generally want to build software out of total functions, using
the type system to help us as much as possible. This kind of software tends to
be easiest to understand and reason about. That means that we need to find a way
to achieve the same purpose as exceptions using only functions and values.

We suggest that the first component of this approach involves designing values
to represent all of the different errors that might occur. These values can be
nested, so that higher-level errors can wrap lower-level errors to add context.
(Side note - does thinking of errors as DAGs provide any insight?)

The second component of this approach involves abstracting away the handling of
errors so that we only need to deal with it where necessary.  This abstraction
ends up taking the form of a Monad, the implementation of which will be shown in
the following section.

The third component involves translating between code that uses exceptions and
our new error handling functions.

### What is EitherT?

* Recap the Monad instance for Either, just for reference.
* Start with `newtype IOEither e a = IOEither (IO (Either e a))`
** write Monad instance.
** Diff against instance for Either.
** If needed, show an example step-by-step evaluation.
* Parametrise references to `IO` to turn it into a monad transformer.

### What does EitherT look like in practice?

* Build up example application.
* Show exception handling at boundaries.
* Show how to lift `IO` and pure code into `EitherT IO`.
* Show how to combine values with different error types.

### What about writing tests?

* Ideally, side-effecting code should be so thin as to not require tests. It
  should generally just be an exception-handling wrapper around the unsafe
  library function.
* If you do want to test the wrapper code, consider factoring out the unsafe
  function as a parameter to the wrapper function. Then you can pass in a
  pathological function as a parameter to test it.
* Testing code in `Either` is straightforward.
* One of the benefits of trying to write testable code in this style is that it
  forces you to push IO to the edges of the application.

### Part of a principled approach

* No partial functions! This bit me recently via a function in `base` which
  called `chr :: Int -> Char`. `chr` is pure but not total. This function was
  being used to render a string error message for an exception type that I was
  trying to handle, so printing the exception actually caused another exception
  to be thrown! 

### Limitations and drawbacks

* You still need to handle exceptions correctly at the boundaries.
* You need to be familiar with the combinators and patterns to be productive.
* You need to come up with descriptive error types for different code regions.

### Comparison with other approaches

* Free
** This generally results in you placing the exception handling code in
   your natural transformations.
** Doesn't necessarily replace EitherT - you can have your primitives returning
   Eithers so that you have the option of error handling, then use `EitherT
   Free`.

### References

* `transformers-either`
* `transformers-bifunctors`
* `safe-exceptions`
