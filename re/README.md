# Introduction

  This project contains two executables: (1) 're' for working with regular
  expressions and (2) 'fa' for working with finite automatons.

# Installation

  Run 'make all' to create all executables.

# Usage

  Both commands take an operation as the first argument and its parameters as
  subsequent arguments. For example,

    $ ./re eq '(a*b*)*' '(a+b)*'

  tests whether the two regular expressions are equivalent. Run the following to
  see a list of all commands supported by these programs.

    $ ./re help
    $ ./fa help

  The symbol 'e' stands for the null string (epsilon). For example,

    $ ./re eq 'a*' 'e+aa*'
    Yes.
