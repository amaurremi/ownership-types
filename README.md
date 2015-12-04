# Implementation of Owners-as-Dominators

The goal of this project is to implement a language with owners-as-dominators support,
as described in the original [paper](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.23.2115) by Clarke et al.

The implemented language is based on S-expressions.
This table shows the correspondence between the language implemented here and described in the paper.

|                | Syntax      | Type system    | Execution environment | Reduction rules |
|----------------|-------------|----------------|-----------------------|-----------------|
| Paper          | Page 52     | Figure 4       | Figure 6              | Figure 5        |
| Implementation | `Parser.hs` | `TypeCheck.hs` | `ExecEnvironment.hs`  | `Eval.hs`       |

The only difference is that this implementation aims to support typing for `null` and for `void` methods.

## Set Up
1. Install the [Haskell Platform](https://www.haskell.org/platform/).
2. Check out this project and navigate into the project directory.
3. Run `make cabal`. This will update [Cabal](https://www.haskell.org/cabal/) and install the
   [Alex](https://www.haskell.org/alex/) lexer generator package.

## Compile
For the first time, compile with `make`. This will create the lexer.
If the lexer is already created you can compile with `make compile`.

## Test
The `test-programs` directory contains the tests `car.s`, `pair.s`, and `link.s`
  for the programs in Figures 1, 2, and 3.