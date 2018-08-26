dog
================

A [Datalog](https://.org/) toolkit providing the following:
  - Alex [Lexer](https://docs.racket-lang.org/datalog/datalog.html) of the `Datalog` lexical specification.
  - Happy [Parser](https://docs.racket-lang.org/datalog/datalog.html) of the `Datalog` BNF Grammar.
  - [Pretty printer](http://hackage.haskell.org/package/prettyprinter) of the `Datalog` abstract syntax tree (AST) for human consumption.
  - [QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html) generators for creating random AST fragments
  - QuasiQuoter providing inline definitions of `Program`.

## Table of Contents
- [Example](#example)
- [Maintainers](#maintainers)
- [Credit](#credit)
- [License](#license)

## Example

```haskell
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Datalog.QQ (datalog)

main :: IO ()
main = print program

program :: Program
program = [datalog|
  edge(a, b). edge(b, c). edge(c, d). edge(d, a).
  path(X, Y) :- edge(X, Y).
  path(X, Y) :- edge(X, Z), path(Z, Y).
  path(X, Y)?
|]

```

## Roadmap

- Magic sets implementation

## Maintainers

- [@dmjio](https://github.com/dmjio)

## Credit

- `Alex` and `Happy` lexing & parsing inspired by [config-value](https://github.com/glguy/config-value)

## License

[BSD3](LICENSE) 2018-2019 Urbint Inc.