
# `IO`, `>>`, `>>=`, `do`, and `return` without Monads (by analogy with Java)
## because if you can write Java, you should be able to write Haskell without reading a bunch of Monad tutorials

Let's just look into the syntax of how to deal with `IO`! (because when you know Haskell syntax, monad tutorials are _so much_ easier to read.)

## 1) What `IO` is
> `IO a` is a generic type. Like any generic type, it can be specialised: `IO String`, `IO Integer`, and `IO (IO Float)` are concrete types.
If it were Java, we'd express it like this:
> `IO<T>` is a generic type. Like any generic type, it can be specialised: `IO<String>`, `IO<Integer>`, and `IO<IO<Float>>` are concrete types.

An `IO String` is literally "a `String` that i had to do some input/output to obtain."
The `IO` part has a record of the input/output that you asked for.
The `String` part is the value that you got from the outside world.

Or alternatively:
> An `IO String` is an input/output action that will give me a `String` whenever it's executed.

We'll refer to a value wrapped in an `IO` object, as an "`IO` action", to emphasise that it's an object that is keeping track of all the input/output that you've asked for.

## 2) What you need to do, to perform input and output
In Java, you just have to call a function:
```java
public static void main(String[] args) {
    System.out.println("omg!");
}
```
will immediately do some output. Boom. Done.

In Haskell, you must _create_ an `IO` action, and _return_ it from your `main` function in order to execute it:
```haskell
main :: IO ()
main = two
    where one = putStrLn "woah!"
          two = putStrLn "omg!"
          three = putStrLn "yeah!"
```

Because of lazy evaluation (and other reasons), Haskell will only print `omg!` when this program is run.

Therefore, the rule for working with `IO` is:

> To end your `main` function, you must return a _single_ `IO` action that has a record of every bit of input and output you want to do.
Until you do this, Haskell will be a hassle.

## 3) Examples of `IO`
Let's build some small helpers in Java so that our Haskell and Java code look more similar.
```java
Stream input = new Stream(System.in);
String readLn() {
    return input.readLine();
}

void putStrLn(String string) {
    System.out.println(string);
}

void putStr(String string) {
    System.out.print(string);
}
```

Cool, let's assume those are included in all of the Java examples.

```java
input.nextLine(); // takes no arguments; returns a String
readLn(); // same as above

System.out.println("omg"); // takes one argument; returns a void
putStrLn("omg"); // same as above

System.out.print("omg"); // takes one argument; returns a void
putStr("omg2"); // same as above
```

Ported to Haskell:
```haskell
readLn -- readLn takes no arguments; it returns an "IO String" (see? it's "a string that we had to do some I/O to obtain")
putStrLn "omg" -- putStrLn takes one argument -- a String -- and returns an "IO ()"
putStrLn "omg2"
```

The first line (`readLn`) is getting the user to type in a line of input. Here, we're returning an `IO String`.
Remember:
> An `IO String` is "a `String` that i had to do some input/output to obtain."

The other parts are writing `"omg"` or `"omg2"` to standard output.
`putStrLn` returns an `IO ()`, which is a weird type.
`()` in Haskell is an empty tuple. It's similar to `void` in Java: it represents "no value".
The type `IO ()` means "there's no value, but I did some input or output to obtain it".
Why would `IO ()` be useful? Because remember:
> When your `main` function ends, you must return a _single_ `IO` action that has a record of every bit of input and output you did.

Even though it doesn't have a value associated with it, the `IO` is also storing the record of the input/output that we did. So we need to make sure we don't lose that `IO ()`.

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
> The `>>` operator mixes two `IO a` actions together. It _throws away_ the value from the first one, and _keeps_ the value from the second one. But the `IO` context from both is blended together and kept.

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

In Haskell, the `IO a` actions have the record of their input/output effects propagated by the `>>` operator down to the end of `main`.
An expression with no side-effects has nothing for `>>` to propagate, so the value is simply thrown away.

The `pure` function takes any expression and turns it into an `IO` action - of course, it will have no input/output in its record.
It's useful because you can then call `>>` (which needs both its left side and right side to have that `IO` context).

## 5) Why using values is hard

That's cool as long as our values are `IO ()`. But `readLn` is returning an `IO String` - in this case, throwing away the value is totally useless:
```haskell
main =
    readLn >>
    putStrLn "why are we bothering with readLn if we're just going to throw the value away?" >>
    readLn >>
    putStrLn "we could delete the readLn lines from this program and it wouldn't change"
```

So we want to make use of the `String` that the user typed into `readLn`, somehow.

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

It would be nice to be able to get the value out of the `IO` action:

```haskell
-- this is imaginary, and doesn't compile
main = putStrLn (getValue readLn)
```

(expressing this in Java-like syntax would be: `putStrLn(getValue(readLn()))`)

But if such a method existed, it'd be a *massive problem*. Remember:
> When your `main` function ends, you must return a _single_ `IO` action that has a record of every bit of input and output you did.

This hypothetical `getValue` method would get the value _and throw away the IO record_. We cannot do that without causing a huge hassle (we'll come to why, at the end), so this method _does not exist_.

## 6) How to use values: `>>=`

So the solution is a bit weird but by golly, it solves a *lot*.

There's another operator called `>>=`. This operator will push an `IO String` into a function that wanted a `String`.
Then, like `>>`, it'll combine their `IO` context so that you don't lose any information.

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

We didn't supply any arguments to `putStrLn`, but the compiler accepted it anyway. That's because:
 - `readLn` got an `IO String`
 - `>>=` passed the `String` from that into `putStrLn` as an argument
 - `putStrLn` returned an `IO ()`, which only has a record of the `putStrLn`
 - finally, `>>=` combines the `IO` context from its left and right sides, creating a new `IO ()`, which has a record of both the `readLn` *and* the `putStrLn`
 
This can happen indefinitely -- and it's much more efficient than it sounds.
We can string together a lot of operations, as long as `>>` and `>>=` are combining the `IO` context together:

```haskell
main =
    putStrLn "Hi it's nice to meet you, " >>
    readLn >>=
    putStr >>
    putStrLn "!!" >>
    putStrLn "I will echo your next word." >>
    readLn >>=
    putStrLn
```
The final `putStrLn` returns an `IO ()` with all of the records. Therefore, this is a good/working program.

If you prefer, you can cut some linebreaks. The same program might be clearer like this:
```haskell
main =
    putStrLn "Hi it's nice to meet you, " >>
    readLn >>= putStr >> putStrLn "!!" >>
    putStrLn "I will echo your next word." >>
    readLn >>= putStrLn
```

But remember that if you want to use non-`IO` expressions with `>>` you need to call `pure` to wrap them up:

```haskell
main = 
    pure (3 + 5) >> -- "pure" is needed to turn (3 + 5) from "Integer" to "IO Integer". Then its value is thrown away by ">>"
    readLn >>= -- read a string and pass it to the `putStrLn` on the next line
    putStrLn >>
    pure "raspberries" >>= -- now we're using "pure" to pass a String into putStrLn. This is not necessary, but it's valid.
    putStrLn
```

And here's a Java port of the above code:

```java
Stream input = new Stream(System.in);

public static void main(String[] args) {
    (3 + 5);
    System.out.println(input.readLine());
    System.out.println("raspberries");
```

You can also reorder the Haskell code using the `=<<` operator - it's just like `>>=` but reversed:

```haskell
main =
    pure (3 + 5) >>
    putStrLn =<< readLn
    putStrLn =<< pure "raspberries" -- or simply: `putStrLn "raspberries"`
```

Cool. Now there's a few more items to cover before we're done:
 - how to use variables
 - how to use "for" loops
 - how to use "do" notation
 - *why* we have to return a record of all `IO`
 - *why* the `IO` type exists - what's the benefit to all this extra bookkeeping?
 - list of lies and generalizations in this document (aka: further learning topics)

## 7) How to use variables

In Java, although expression values are thrown away by `;`, we can use variables to save values:

```java
public static void main(String[] args) {
    putStrLn("What's your name?");
    String name = readLn();
    putStr("Hi it's nice to meet you, ");
    putStr(name);
    putStrLn("!!");
}
```

In our Haskell-based line of thinking, we say that Java is storing its variables in the `IO` context so that they won't be thrown away by `;`.

We could do that in Haskell too, but it's not normally how we do things. If we port the above Java to Haskell, we'd end up with a few possible implementations:

```haskell
main :: IO ()
main =
    putStrLn "What's your name?" >>
    let name = readLn in (
        putStr "Hi it's nice to meet you, " >>
        putStr name >>
        putStrLn "!!")
```

```haskell
main :: IO ()
main =
    putStrLn "What's your name?" >>
    readLn >>= (\name ->
        putStr "Hi it's nice to meet you, " >>
        putStr name >>
        putStrLn "!!")
```

```haskell
main :: IO ()
main = do
    putStrLn "What's your name?"
    name <- readLn
    putStr "Hi it's nice to meet you, "
    putStr name
    putStrLn "!!"
```

Hey, that last method looks pretty similar to the Java. We're going to work toward that.

First: you should know that Haskell uses functions for just about _everything_.
