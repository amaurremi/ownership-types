# Implementation of Owners-as-Dominators

The goal of this project is to implement a language with owners-as-dominators support,
as described in the original [paper](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.23.2115) by Clarke et al.

The implemented language is based on S-expressions.
This table shows the correspondence between the language implemented here and described in the paper.

|                | Syntax      | Type system    | Reduction rules | Garbage collection (start) |
|----------------|-------------|----------------|-----------------|----------------------------|
| Implementation | `Parser.hs` | `TypeCheck.hs` | `Eval.hs`       | `Eval.hs`                  |
| Paper          | Page 52     | Figure 4       | Figure 5        |                            |

## Set Up
1. Install the [Haskell Platform](https://www.haskell.org/platform/).
2. Navigate into the project directory.
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

Note that the test programs also slightly differ from the paper to avoid null pointer exceptions.

### Type checking
The example programs in the paper illustrate the restrictiveness of ownership types
by including statements that would be allowed in a language like Java, but will
fail in the presence of the ownership type system.

The lines with the failing statements are commented out in the test programs, and you can uncomment them
to see that the type checking works.

### Reduction
According to the syntax, a program consists of a list of classes, a list of local variables,
and a main expression to be evaluated.
When running a program, the interpreter will output the contents of the store after
the expression has been evaluated.

## Notes on the language
The example programs in the paper use syntax that is not supported by the language
described in the paper. In particular, there are no `void` methods, `null` type checking is not complete,
there are no conditional statements and boolean operations.

This language differs from the one in the paper in that it aims to support typing for `null` objects
and for `void` methods.
`void` methods must end with the `end` keyword; note that `void` is called `Unit` in the syntax.

To invoke a field `x` inside an object it is necessary to explicitly invoke it on `this`.

## Garbage collection
Garbage collection can be turned on and off with the `doCollect` flag in `Eval.hs`.
It is turned off by default.

This version of ownership types is not very amicable to garbage collection but
Gregor had the following idea of taking advantage of this type system for automatic
memory management:

Each object is assigned a _stickiness_ degree. The stickiness is an overapproximation of the number
 of references to the object.
If the object is sticky we can only free it if the object's
owner context is `rep` and the object's owner was freed.

Stickiness forms a partial order as follows:

`new-object` < `single-variable-assignment` < `sticky`

and

`new-object` < `single-field-assignment` < `sticky`

- When an object is created its stickiness is set to `new-object`.
- Whenever there is an assignment to a local variable in which the object appears
  on the right-hand side, the object stickiness is increased.

An object can be freed if
- its stickiness is `single-variable-assignment` and the variable
  it is assigned to got popped from the stack;
- its owner got freed.



The rest still needs to be implemented, and the current implementation needs to be tested.