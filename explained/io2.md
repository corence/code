
# `IO`, `>>`, `>>=`, `do`, and `return` without Monads (by analogy with Java)
## because if you can write Java, you should be able to write Haskell without reading a bunch of Monad tutorials

Let's just look into the syntax of how to deal with `IO`! (because when you know Haskell syntax, monad tutorials are _so much_ easier to read.)

## 1) What `IO` is
> `IO a` is a generic type. Like any generic type, it can be specialised: `IO String`, `IO Integer`, and `IO (IO Float)` are concrete types.
If it were Java, we'd express it like this:
> `IO<T>` is a generic type. Like any generic type, it can be specialised: `IO<String>`, `IO<Integer>`, and `IO<IO<Float>>` are concrete types.

An `IO String` is literally "a `String` that i had to do some input/output to obtain."
The `IO` part has a record of the input/output that you did.
The `String` part is the value that you got from the outside world.

## 2) What you need to do, to make `IO` happy
> When your `main` function ends, you must return a _single_ `IO` object that has a record of every bit of input and output you did.
Until you do this, Haskell will be a hassle.

## 3) Examples of `IO`
In Java:
```java
Stream input = new Stream(System.in);

input.nextLine(); // takes no arguments; returns a String
System.out.println("omg"); // takes one argument; returns a void
```

Ported to Haskell:
```haskell
readLn -- readLn takes no arguments; it returns an "IO String" (see? it's "a string that we had to do some I/O to obtain")
putStrLn "omg" -- putStrLn takes one argument -- a String -- and returns an "IO ()"
```
A few notes about Haskell syntax:
 - To call a method with no arguments, just write its name (like `readLn`)
 - To call a method with multiple arguments, they need to be separated by spaces (not by parentheses and commas, as in Java)

The first line (`input.nextLine()` or `readLn`) is getting the user to type in a line of input. Here, we're returning an `IO String`.
Remember:
> An `IO String` is literally "a `String` that i had to do some input/output to obtain."

The next line is a bit interesting -- Haskell is returning an `IO ()` -- that's a weird type.
The type `()` in Haskell is similar in meaning to type `void` in Java -- it represents "no value".
The type `IO ()` means "there's no value, but I did some input or output to obtain it".
Why would `IO ()` be useful? Because remember:
> When your `main` function ends, you must return a _single_ `IO` object that has a record of every bit of input and output you did.

Even though it doesn't have a value associated with it, the `IO` is also storing the record of the input/output that we did. So we need to make good use of our `IO ()`.

## 4) Doing multiple `IO` actions in a row

In Java:
```java
// a) most basic example
public static void main(String[] args) {
    System.out.println("Sonic ");
    System.out.println("The ");
    System.out.println("Hedgehog ");
}
```

```haskell
-- a) most basic example
main :: IO ()
main =
    putStrLn "Sonic " >>
    putStrLn "The " >>
    putStrLn "Hedgehog "
}
```

The `>>` operator in Haskell is doing pretty much the same thing as the `;` operator in Java.

But let's get really specific about what it's doing:
> The `>>` operator mixes two `IO a` objects together. It _throws away_ the value from the first one, and _keeps_ the value from the second one. But the `IO` context from both is blended together and kept.

We can say the same thing in Java too!
> The `;` operator mixes two expressions together. It _throws away_ the value from the first one, and _keeps_ the value from the second one. But the side-effects that have happened so far are kept.

Examples!
```java
public static void main(String[] args) {
    System.out.println("Sonic ");
    "hello" + "world";
    System.out.println("The ");
    3 + 5;
    System.out.println("Hedgehog ");
}
```

```haskell
-- haskell interspersed with junk -- this _almost_ compiles
main = 
    putStrLn "Sonic " >>
    "hello" ++ "world" >>
    putStrLn "The " >>
    3 + 5 >>
    putStrLn "Hedgehog "
    
-- this one compiles
main = 
    putStrLn "Sonic " >>
    pure ("hello" ++ "world") >>
    putStrLn "The " >>
    pure (3 + 5) >>
    putStrLn "Hedgehog "
```

In Java, the side-effects from expressions are propagated by the `;` operator down to the end of `main`.
An expression with no side-effects has nothing for `;` to propagate, so the value is simply thrown away.
(This is a super-rough interpretation of Java syntax. It's not true, but it's approximately true.)

In Haskell, the `IO a` objects have the record of their input/output effects propagated by the `>>` operator down to the end of `main`.
An expression with no side-effects has nothing for `>>` to propagate, so the value is simply thrown away.

## 5) Why using values is hard

That's cool as long as our values are `IO ()`. But `readLn` is returning an `IO String` - in this case, throwing away the value is totally useless.

In java this is easy -- just do this:
```java
public static void main(String[] args) {
    System.out.println(input.nextLine);
}
```

but if we try that in Haskell:

```haskell
-- this doesn't compile
main = putStrLn readLn
```

Nope, that can't happen -- because now we're trying to pass an `IO String` to a function that wants a `String`. It's not the right type.

It would be nice to be able to get the value out of the `IO` object:

```haskell
-- this is imaginary, and doesn't compile
main = putStrLn (getValue readLn)
```

(expressing this in Java-like syntax would be: `putStrLn(getValue(readLn()))`)

But if such a method existed, it'd be a *massive problem*. Remember:
> When your `main` function ends, you must return a _single_ `IO` object that has a record of every bit of input and output you did.

This hypothetical `getValue` method would get the value _and throw away the IO record_. We cannot do that without causing a huge hassle (we'll come to that later), so this method _does not exist_.

## 6) How to use values: `>>=`

So the solution is a bit weird but by golly, it solves a *lot*.

There's another operator called `>>=`. It's a little more complicated:

> `>>`: it has an `IO a` on the left, and an `IO b` on the right. It throws away the value from the left one, and keeps the value from the right one.
> `>>=`: it has an `IO a` on the left, and a _function_ that _returns_ an `IO b` on the right. It takes the value from the left one, and *passes that value to the function on the right*. Then it keeps the result of that function as the final value.

Another way to look at it is: `>>=` will push an `IO String` into a function that wanted a `String`.

Examples!

```haskell
main = 
    readLn >>
    putStrLn "omg" -- this _threw away_ the value of readLn and then we supplied our own argument to putStrLn. (This is a dumb/useless program.)
```

```haskell
main = 
    readLn >>=
    putStrLn -- we didn't supply an argument to putStrLn this time. The String that will be given to putStrLn is the one we got from readLn. And the IO record from both will be mixed. Cool!
```

```haskell
main = 
    pure (3 + 5) >> -- 1, 2
    readLn >>= -- 3
    putStrLn >> -- 4
    pure "raspberries" >>=
    putStrLn
```

Let's go line-by-line in that last Haskell example:
 1) We use `pure` to turn (3 + 5) from an `Integer` to an `IO Integer`. It needs to be of type `IO a` or else the compiler won't use it with the `>>` operator
 2) `pure (3 + 5)` got thrown away by `>>`
 3) `readLn` is returning an `IO String`, as usual
 4) We called `putStrLn` without any arguments -- this is because the String argument got fed in by `>>=`
 5) 

OK, some info:
 - `return` in Haskell is _totally_, *totally* unrelated to `return` in Java. It's a coincidence that they use the same word, but they do totally different things.
 - `return` takes any value and turns it into an `IO` value. For example, `return (3 + 5)` takes the result of `3 + 5`, which is an `Integer`, and turns it into an `IO Integer`. Why would we want to do that? Because `>>` only works with `IO` types.
 - Note how the `;` operator in Java throws away the value of `3 + 5`. In Haskell, `>>` throws away the value of `3 + 5` too.
 - The `;` operator throws away the return value of the expression _but keeps the IO context_. Thus, the `float pi` statement retains some context _in Java's `IO` context_. It's not a contradiction of the idea of "throws away the value".
 
 



## Getting values from `IO`

The thing that happens here is `putStrLn` requires a `String`, but `readLn` is returning an `IO String`.

How can we solve that?

### Guess 1: get the value out of the `IO`
This is what I'd expect to be the solution. Let's say this was Java (but with the Haskell libraries) -- here's what I'd be trying to do:

```java
// imaginary solution to getting data out of IO

IO<String> result = readLn();
String string = result.getValue();
putStrLn(string);
```

That seems to make sense, right? Get the string out of the IO so we can pass it to `putStrLn`.

But that doesn't work at all. The problem with that would be -- we'd be throwing away the `IO` record and just getting the value of out it -- but we're not allowed.
Let's revise the one rule of `IO`:
> When your `main` function ends, you must return a _single_ `IO` object that has a record of every bit of input and output you did.

So the `IO` type is designed to make it _difficult_ for you to accidentally lose one of your `IO` records.

Let's try something else.

### Solution 1: Tell `IO` to call the function for us -- with `>>`

Let's introduce a new operator. It's called `>>`.

This operator can be used to call multiple `IO` operations in a row.

```haskell
-- this works
putStrLn "Sonic " >> putStrLn "The " >> putStrLn "Hedgehog "
```

This compiles.









For example, in Java:
```java
public static String myName(String firstName, String lastName) {
    return firstName + " " + lastName;
}

public static void main(String[] args) {
    System.out.println(myName("Sonic", "Hedgehog"));
}

// The type signature of myName is:
//  - it takes 2 arguments -- both Strings
//  - it returns a String

// The type signature of System.out.println looks like this:
public void println(String string);
// this means:
//  - it takes one argument (a String)
//  - its return type is void

```

but when we port this to Haskell, it introduces `IO`:
```haskell
myName :: String -> String -> String
myName firstName lastName = firstName ++ " " ++ lastName

main :: IO ()
main = putStrLn (myName "Sonic" "Hedgehog")

-- This is the Java program, ported to Haskell. Function arguments are separated by spaces in Haskell, rather than parentheses and commas as in Java.

-- The type signature of myName is the same in Java and Haskell.
-- myName :: String -> String -> String
-- the first two String elements are the arguments -- this is a function that takes 2 arguments.
-- the last String is the return type.
-- (note: this is a convenient half-truth. In your future Haskell studies you'll learn about currying and partial function application. But for now, just go with it.)

-- putStrLn has a type signature that looks like this:
putStrLn :: String -> IO ()
-- This is almost exactly the same type signature as the Java one.
-- We read this as a function that takes one argument (a String) and returns an object of type "IO ()".
-- OK, so "()" in Haskell is the type analogous to Java's "void" -- a type that holds zero information.
-- Therefore, "IO ()" would be expressed in Java as "IO<void>".
-- It means "no data, but I had to do some input or output to get it".
-- In Haskell, you need to track anywhere that you did input/output, even if you're not keeping the results!
-- This seems cumbersome at first but it becomes as 
```

