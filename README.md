# Implementation of Owners-as-Dominators

The goal of this project is to implement a language with owners-as-dominators support,
as described in the original [paper](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.23.2115) by Clarke et al.

The implemented language is based on S-expressions.
This table shows the correspondence between the language implemented here and described in the paper.

|                | Syntax      | Type system    | Execution environment | Reduction rules |
|----------------|-------------|----------------|-----------------------|-----------------|
| Implementation | `Parser.hs` | `TypeCheck.hs` | `ExecEnvironment.hs`  | `Eval.hs`       |
| Paper          | Page 52     | Figure 4       | Figure 6              | Figure 5        |

## Set Up
1. Install the [Haskell Platform](https://www.haskell.org/platform/).
2. Clone this project and navigate into the project directory.
3. Run `make cabal`. This will update [Cabal](https://www.haskell.org/cabal/) and install the
   [Alex](https://www.haskell.org/alex/) lexer generator package.

## Compile
For the first time, compile with `make`. This will create the lexer.
If the lexer is already created you can compile with `make compile`.

## Test
The `test-programs` directory contains the tests `car.s`, `pair.s`, and `link.s`
  for the programs in Figures 1, 2, and 3.

A good way to understand the language syntax is to see its definition
in the paper and to look at the comments in `car.s`.

To run a program `name.s`, run `./owni test-programs/name.s`.

The example programs in the paper illustrate the restrictiveness of ownership types
by including statements that would be allowed in a language like Java, but will
fail in the presence of the ownership type system.
Those lines are commented out in the test programs here, and you can uncomment them
to see that the type checking works.

## Language Notes
The example programs in the paper use syntax that is not supported by the language
described in the paper. In particular, there are no `void` methods, `null` type checking is not complete,
there are no conditional statements and boolean operations.

This language differs from the one in the paper in that it aims to support typing for `null` objects
and for `void` methods.
`void` methods must end with the `end` keyword; note that `void` is called `Unit` in the syntax.

To invoke a field `x` inside an object it is necessary to explicitly use `this.x`.
