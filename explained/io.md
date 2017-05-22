
# `IO`, `>>`, `>>=`, `do`, and `return` without Monads (by analogy with Java)
## also: how to write imperative style programs

When learning Haskell, a huge pain point is the chicken-and-egg problem of `IO`:
 - Q: how do you know how to use `IO`? A: You first need to learn Monads.
 - Q: how do you learn about Monads? A: it's difficult to express Monads in other languages, so they tend to be expressed in Haskell syntax.
 - Q: what problem will I run into when learning haskell syntax? A: trying to get `IO` to compile.

This tutorial is intended to get you up and running with Haskell's `IO` -- without requiring any Haskell knowledge as a prerequisite. Each bit of Haskell syntax in this document will be associated with the analogous Java code.

In particular, most Haskell tutorials indicate that you need to start by learning Monads. We're gonna skip that; you just need enough information to understand the rules of `IO` so you can get your code to compile. Later, when you're more familiar with Haskell syntax, you can start jumping into Monad tutorials.

If you can read Java, then soon you'll be fluent in Haskell IO.

## 1) What's `IO`, in programming terms?
> `IO a` is a generic type. Like any generic type, it can be specialised: `IO String`, `IO Integer`, and `IO (IO Float)` are concrete types.
If it were Java, we'd express it like this:
> `IO<T>` is a generic type. Like any generic type, it can be specialised: `IO<String>`, `IO<Integer>`, and `IO<IO<Float>>` are concrete types.

The _meaning_ of `IO String` is literally: "a String that I had to do some input or output to obtain".
The _meaning_ of `IO Integer` is: "an Integer that I had to do some input or output to obtain".
The _meaning_ of `IO (IO Float)` is: "an "IO Float" that I had to do some input or output to obtain". (alternatively: a Float that I had to do input/output twice to obtain")

The `IO` type records what input/output actions you took. In order to get your code to compile and run correctly, you need to follow one rule:
> When your `main` function ends, you must return a _single_ `IO` object that has a record of every bit of input and output you did.

If you do this correctly, then `IO` will Just Work, and Haskell won't be a hassle.

Some I/O examples in both languages:
```java
Stream input = new Stream(System.in);

Math.sqrt(4.5); // takes one argument -- a double -- and returns a double
Math.sqrt(Math.sqrt(4.5)); // calculates the sqrt of 4.5 and returns a double, which is passed to Math.sqrt, which returns a second double
input.nextLine(); // takes no arguments; returns a String
System.out.println("omg"); // takes one arguments; returns a void
System.out.println(input.nextLine()); // the inner method takes no arguments and returns a String; the outer method takes that String and returns a void
```

In the Java code above, we could say that the `IO` type is implicitly attached to each variable.

Here's how we do those things in Haskell:
```haskell
sqrt 4.5 -- takes one argument -- a Double, and returns a Double.
sqrt (sqrt 4.5) -- we can do nested method calls in Haskell just like we did in Java -- we just need to wrap the inner method call in parentheses so there's no ambiguity about which method gets the arguments
readLn -- readLn takes no arguments; returns an "IO String" (see? it's "a string that we had to do some I/O to obtain")
putStrLn "omg" -- putStrLn takes one argument -- a String -- and returns an "IO ()". The type called "()" in Haskell is analogous to "void" -- it communicates the absence of any value
putStrLn readLn -- this is a compile error! :'(
```
The first two lines aren't doing any input or output -- they are here to demonstrate method calls in Haskell.

A few notes about Haskell syntax:
 - To call a method with no arguments, just write its name (like `readLn`)
 - To call a method with multiple arguments, they need to be separated by spaces (not by parentheses and commas, as in Java)
 - Parentheses can optionally be added to resolve ambiguity. We needed to use them in line 2 (because `sqrt sqrt 4.5` is ambiguous -- which instance of `sqrt` is taking the `4.5`?)

OK, let's go through each line in turn.

The first one calls `sqrt` -- this is just to show a normal method call in Haskell.
The second one calls `sqrt` on the value returned by `sqrt` -- again, just to demonstrate normal methods (the ones that don't return `IO`).
The third line is getting the user to type in a line of input. Here, we're returning an `IO String`.

The fourth line is a bit interesting -- Haskell is returning an `IO ()` -- that's a weird type.
The type `()` in Haskell is similar in meaning to type `void` in Java -- it represents "no value".
The type `IO ()` means "there's no value, but I did some input or output to obtain it". We need to track input and output in an `IO` object, even if there's no value returned.

Finally, we try to read a line of input from the user, and print that out. But this fails! We're going to need to cooperate with the type system to get this to compile.

## Calling multiple `IO` actions

Let's take a detour and look at something _else_ that doesn't easily work in Haskell -- printing multiple times.

```haskell
-- this doesn't work
main = (putStrLn "Sonic ") (putStrLn "The ") (putStrLn "Hedgehog ")

-- this also doesn't work
main = (putStrLn "Sonic ")
    (putStrLn "The ")
    (putStrLn "Hedgehog ")

-- this works!
main = (putStrLn "Sonic ") >>
    (putStrLn "The ") >>
    (putStrLn "Hedgehog ")

-- and we don't need the parentheses, because `>>` has low precedence
main = putStrLn "Sonic " >>
    putStrLn "The " >>
    putStrLn "Hedgehog "

-- this works too, but we'll come back to it later
main = do
    putStrLn "Sonic "
    putStrLn "The "
    putStrLn "Hedgehog "
```

The `>>` operator is _extremely similar_ to the `;` operator in Java. It "throws away" the value of the previous expression but retains the `IO` context.

Look at this Java code:

```java
public static void main(String[] args) {
    System.out.println("Sonic ");
    float pi = 3.14;
    System.out.println("The ");
    3 + 5;
    System.out.println("Hedgehog ");
}
```

The behaviour of this program matches the Haskell code. Actually, let's intersperse some expressions:

```haskell
-- haskell interspersed with junk -- this _almost_ compiles
main = 
    putStrLn "Sonic " >>
    let pi = 3.14 in ()
    putStrLn "The " >>
    3 + 5
    putStrLn "Hedgehog "
    
-- this one compiles
main = 
    putStrLn "Sonic " >>
    return (let pi = 3.14 in ())
    putStrLn "The " >>
    return (3 + 5)
    putStrLn "Hedgehog "

-- actually this one is probably more correct with the Java semantics -- as `pi` has a lifetime until the end of the block it's defined in
main = 
    putStrLn "Sonic " >>
    return (let pi = 3.14 in (
        putStrLn "The " >>
        return (3 + 5)
        putStrLn "Hedgehog "))
```

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

