# Introduction

  This program implements an interpreter for the lambda calculus.

# Installation

  Use 'make all' to compile the the project. The executable file called "lambda"
  in this directory is the interpreter.

# Usage

  To run the interpreter, use:

    $ ./lambda [OPTIONS] FILE

  where FILE is the name of the file that the interpreter will
  run. All terms in the file are evaluated and printed to screen
  upon running the program.

  By default, the interpreter uses normal order evaluation. This has the nice
  property that all strongly normalizing terms will normalize. However, this may
  also enter an infinite loop for some terms. The option "-n" along with an
  argument can be used to change the evaluation scheme. The argument "hnf" or
  "whnf" terminates evaluation at a head normal form or weak head normal form
  respectively and they terminate for strictly more terms. The argument "app"
  can be used to specify applicative order evaluation which is used by many
  real-world programming languages.

# Syntax

  Single line comments start with a "#" and span to the end of
  the current line. These are ignored.

  A program is a sequence of definitions and terms terminated by
  a ";".

  A definition has the syntax "x := t" where x is an identifier
  and t is a term. A definition binds the identifier x to the
  term t and allows x to be used in subsequent terms in the file.

  Identifiers are non-empty sequences of alpha-numeric characters
  and '?', '*', '$', and '. Ex. "0", "0a", "a0", "x'" are valid
  identifiers.

  A lambda term has the syntax "\x. t" where x is a variable and
  t is a term.

  An application has the syntax "t u" where t and u are terms.
  Application is left-associative and binds tighter than lambda.
  i.e., "t u v" means apply t to u and then its result to v and
  "\x. x x" is the lambda term that applies x to itself.  The
  precedence can be changed by using paranthesis. For example, "t
  (u v)" applies u to v and then applies t to the result. Also,
  "(\x. x) t" applies the identity function to t.

# Evaluation

  The interpreter tries to find the normal form by using normal order
  reductions. This may not terminate for some terms. The evaluation scheme can
  be changed by passing the option "-n" with an argument "hnf", "whnf" for head
  normal form and weak head normal form respectively. The argument "app" uses
  applicative order evaluation.

# Examples

  See the files in 'sample/'.
