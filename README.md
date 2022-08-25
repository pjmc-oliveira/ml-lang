# Unnamed Programming Language

This is the implementation of a strongly typed, pure, and lazy, functional programming language for educational purposes.

## Running

- Install OCaml and Dune (https://ocaml.org/docs/up-and-running)
  - OCaml 4.14
  - Dune 3.3.1
- Run program with:

```
dune exec ml_lang <path_to_file>
```

- Run tests with:

```
dune test
```

## Features and Examples

### Built-in functions

```
-- Int arithmetic
add : Int -> Int -> Int
sub : Int -> Int -> Int
mul : Int -> Int -> Int
div : Int -> Int -> Int

-- Int comparison
eq : Int -> Int -> Bool
ne : Int -> Int -> Bool
le : Int -> Int -> Bool
ge : Int -> Int -> Bool
lt : Int -> Int -> Bool
gt : Int -> Int -> Bool

-- Bool operations
not : Bool -> Bool
and : Bool -> Bool -> Bool
or  : Bool -> Bool -> Bool
```

### Factorial

```
module Main = {
  def main = fact 5

  def fact = \n
    if le n 0 then
      1
    else
      mul n (fact (sub n 1))
}
```

- if-expressions
- recursion

### Head

```
module Main = {
  type Maybe a = None | Some a
  type List a = Nil | Cons a (List a)

  def main = head Nil

  def head = \list
    match list with
    | Nil -> None
    | Cons x xs -> Some x
    end
}
```

- custom algebraic datatypes
- (non-nested) pattern matching

### Count

```
module Main = {
  type List a = Nil | Cons a (List a)

  def main = take 5 (count 0)

  -- Infinite list
  def count = \n
    Cons n (count (add n 1))

  def take = \n \list
    if eq n 0 then
      Nil
    else
      match list with
      | Nil -> Nil
      | Cons x xs -> Cons x (take (sub n 1) xs)
      end
}
```

- lazy evaluation

### Fibonacci

```
module Fib = {
  type List a =
    | Nil
    | Cons a (List a)

  def main = nth 6 fibs

  def fibs = Cons 0 (Cons 1 (zip_with add fibs (drop 1 fibs)))

  type Maybe a =
    | None
    | Some a

  def zip_with = \f \xs \ys
    match xs with
    | Nil -> Nil
    | Cons x xs ->
      match ys with
      | Nil -> Nil
      | Cons y ys -> Cons (f x y) (zip_with f xs ys)
      end
    end

  def drop = \n \list
    if le n 0 then
      list
    else
      match list with
      | Nil -> Nil
      | Cons x xs -> drop (sub n 1) xs
      end

  def nth = \n \list
    match list with
    | Nil -> None
    | Cons x xs ->
      if le n 0 then
        Some x
      else
        nth (sub n 1) xs
    end
}
```

- Efficient laziness, only computes an expression at most once

## Known issues

### Unification of bound type-variables

```
module Main = {
  def main = fake_identity False
  def fake_identity : forall a. a -> a
    = \x True
}
```

In this example the type variable 'a' will be unified with 'Bool', and the type of the function 'fake_identity' will be inferred as 'Bool -> Bool', disregarding the type annotation. This the same behaviour as OCaml, but not one I intended to support.

### Does not check for 'main' function

```
module Main = {
  -- 'main' is not defined
  -- def main = 0
}
```

The type checker does not require a 'main' function to be defined, despite the interpreter requiring it to run. This was done because I haven't on a way to do IO for this language, so the type checker can't check for a specific type for the 'main' function.

### Segmentation fault for large recursive functions

```
module Main = {
  type List a =
    | Nil
    | Cons a (List a)

  def main = sum (range 1 1000000)

  def sum = fold_right add 0

  def fold_right = \f \base \list
    match list with
    | Nil -> base
    | Cons x xs -> f x (fold_right f base xs)
    end

  -- creates a list from 'start' to 'stop' exclusive
  def range = \start \stop
    if lt stop start then
      Nil
    else
      Cons start (range (add start 1) stop)
}
```

The code above, with a large enough input will cause a segmentation fault. This is of course very bad, and somewhat surprising as we're not dealing with raw pointers in OCaml.

### Only compares types by name

```
module Main = {
  type Int = NotAnInt Bool

  def main = 1 : Int
}
```

In this example we define an 'Int' type locally and annotate an integer literal with the local type. The type checker should reject this, but it does not.

## Missing features and potential improvements

- A REPL (currently the user can only execute files)
- A way for programs to span multiple files
- Support for IO
- Currently the runtime environment stores all variables by their string name
- Improve parsing errors
- Better interpretation of pattern matching
