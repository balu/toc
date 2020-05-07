# Introduction

  This program is a Turing machine simulator that simulates the given TM on the
  given input.

# Installation

  Use 'make all' to compile the project. The executable 'tm' is the Turing
  machine simulator.

# Usage

  To run the TM in sample.tm on input string "aabb" use:

    $ ./tm -i "aabb" sample.tm

  The interpreter terminates when the machine halts or when the timeout is
  reached. The output will look as follows:
  
    Machine halted.
    q#$$.1011$

  which indicates that the machine halted with the configuration where state is
  `q`, the tape contains `1011` with the head over the first `1`. The `$`
  indicates the blank symbol.
  
  Run
  
    $ ./tm --help

  to see a list of all options.

# Syntax

  Each transition has the form (s, t) -> (s', t', d) where s, s' are states; t,
  t' are symbols and d is the direction. Symbols have to be single characters
  and '$' is used for the blank symbol. States are identifiers and the direction
  is '<' for left, '>' for right, and '_' for stay. See 'sample/' for
  descriptions of various TMs.
