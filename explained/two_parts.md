# Monads - a two-part definition
## Preface

This document is aimed at you if:
 - you want an intuitive understanding of Monads
 - you don't (yet) know Haskell syntax
 - you can read Java and Javascript

## Definition

In my experience, most monad tutorials start with one of these three phrases:
> A monad is *like*...  
> A monad is *for*...  
> In category theory...

but I haven’t come across one with a succinct definition of what a monad **is**, in the context of programming languages.

So -- here's an attempt at a definition:

> A monad is a **wrapper type** with a specific set of **conversion functions**.

Let's break this further down, and explore:
- what a wrapper type is
- therefore, what monads are for
- what the conversion functions **do**
- what the conversion functions are for

## Wrapper types
### What they are

A wrapper type is a type that wraps another type.

Examples in Java:
 - `List<String>` (`List<T>` is the wrapper type; `String` is the wrapped type)
 - `Optional<Integer>`
 - `Iterator<Boolean>`
 - `Comparable<String>`

Examples in Haskell:
 - `[String]` (equivalent to Java's `List<String>`)
 - `Maybe Integer` (equivalent to Java's `Optional<Integer>`. `Maybe a` is the wrapper type; `Integer` is the wrapped type)
 - `IO String` (this type represents "a `String` that I had to do some input/output to obtain")

Note that an instance of a wrapper type does not necessarily contain an instance of the wrapped type -- for example, `Comparable<String>` does not contain an instance of `String`.

### What monads are for (considering them as wrapper types)
Answer: The same set of problems that any other wrapper type can solve.

Remember, a monad is just a **wrapper type** with a specific set of **conversion functions**.

## Conversion functions
### What they do
The conversion functions help you to call a function that accepts an **unwrapped** type, even if all you have is the **wrapped** type.

Consider the **map** function (it exists in most languages; even Java 8 has it now). It allows you to call a function that accepts an `Integer`, even if all you have is a `List<Integer>`. This is a great example of a conversion function.

Javascript
```javascript
[1, 3, 4, 5].map(x => x * 2); // returns [2, 6, 8, 10]
```

Haskell
```haskell
fmap (\x -> x * 2) [1, 3, 4, 5] -- returns [2, 6, 8, 10]
```

In Haskell, `fmap` doesn't just work on Lists -- it works on **every Monad type**. This means that there's a sensible default implementation of `fmap` for every Monad.

Some examples of fmap, applied to other types:

```haskell
fmap (\x -> x * 2) (Just 3) -- returns Just 6 (this is a Maybe type with a value)
fmap (\x -> x * 2) (Nothing) -- returns Nothing (this is a Maybe type with no value)
fmap (\x -> x * 2) (randomIO) -- gets a random number, then doubles it. Note that random numbers in Haskell will be wrapped in the IO monad type, because "talking to the random number generator" is considered a form of I/O
```

There are a bunch of other conversion functions -- but they all follow the basic theme of "I want to call a function, but my mix of wrapped and unwrapped types isn't right".

### What they're for, part 1: Chaining Method Calls

You can chain method calls, as long as you pick the correct conversion function each time. This really cuts down on the amount of boilerplate/safety code you'd need to copy-paste.

Example — Null-checking vs Chaining

Suppose you want to know the name of the country that a person is in.
You could say (in java):

```java
String getCountryName(Dude dude) {
    return dude.getCity().getCountry().getName();
}
```
but that’ll throw a NullPointerException if the person is in international waters, or the ISS.

To correct that, we need to check for null:
```java
String getCountryName(Dude dude) {
    if(dude != null) {
        City city = dude.getCity();
        if(city != null) {
            Country country = city.getCountry();
            if(country != null) {
                return country.getName();
            }
        }
    }
    return null;
}
```

Or, you could use exceptions instead:
```java
String getCountryName(Dude dude) {
    try {
        return dude.getCity().getCountry().getName();
    } catch(NullPointerException ex) {
        return null;
    }
}
```

So in Java you need to make the choice between
- function chaining (easy to read, but doesn’t cover edge cases)
- null-checking (hard to read, but good implementation)
- exceptions (easy to read, but it might be catching exceptions from unexpected places deeper in the call stack)

now in Haskell, you aren’t *allowed* to ignore the nulls like in the first Java version — so it is very similar to the Java null-checking implementation:

(in Haskell, there’s no `null`. The `Maybe` wrapper type allows the analogous value `Nothing` to be used.)
```haskell
getCountryName :: Maybe Dude -> Maybe String
getCountryName maybeDude
  = case (maybeDude) of
      Just dude -> case (getCity dude) of
          Just city -> case (getCountry city) of
              Just country -> getName country
              Nothing -> Nothing
          Nothing -> Nothing
      Nothing -> Nothing
```

but if you use the Monad conversion function called `>>=`, it will make the default assumption that a Nothing should be carried through, and we get:
```haskell
getCountryName :: Maybe Dude -> Maybe String
getCountryName maybeDude = maybeDude >>= getCity >>= getCountry >>= getName
```

This final Haskell version is about as short as the first Java version — but it behaves like the null-safe versions.

### What they're for, part 2: Familiar Syntax
Each time you come across a new Monad type, you'll have a head-start in learning how to use it.

Most Haskell libraries make heavy use of Monads. (Why? To take advantage of the three benefits we're listing here.)

Haskell also provides some direct language support for any Monad type, in the form of `do` notation.

### What they're for, part 3: Higher-level programming

Monads allow you to draw connections that that seem bizarre and mind-blowing, but can be really useful.

For example: a very handy feature in programming languages is a function to flatten a list. In Haskell, it's called `join`:

```haskell
join [[1,2,5],[3,4],[2]]
-- returns: [1,2,5,3,4,2]
```

But actually, `join` can flatten any monad -- not just lists. So you can also do this:

```haskell
join (Just (Just 3)) -- returns: Just 3
join (Just (Nothing)) -- returns: Nothing
join (Nothing) -- returns: Nothing

join (*) -- "joins" the multiplication function, to create a new function, that squares its argument
(join (*)) 3 -- returns: 9
```

`join` is a super-flexible function. Yet the implementation is embarrassingly simple: (not necessarily simple to understand -- but simple in that it has few elements)
```haskell
join thing = thing >>= id
```

See? Nothing about lists, Maybes, or functions here :D

Another Haskell example is the function called `>>`.

When you have values in the `IO` monad, `>>` is pretty similar to the `;` operator in Javascript:

```javascript
function printMyName() {
    console.log('sonic');
    console.log('the');
    return console.log('hedgehog');
}
```

`console.log` is a function that takes a `String`, and returns `undefined`.
The `;` operator **throws away** the `undefined`, but **keeps** the IO side-effects.

```haskell
printMyName =
    putStrLn "sonic" >>
    putStrLn "the" >>
    putStrLn "hedgehog"
```

`putStrLn` is a function that takes a `String` and returns `IO ()`. `()` is an empty/blank value, like `undefined` in javascript or `void` in Java.
So, `IO ()` means "an empty/blank value, that I had to do some IO to obtain".
The `>>` operator **throws away** the `()`, but **keeps** the IO side-effects.

But `>>` works with *any* monad, not just IO.

```haskell
Just "sonic" >> Just "the" >> Just "hedgehog" -- returns Just "hedgehog"
Just "sonic" >> Nothing >> Just "hedgehog" -- if there's a Nothing anywhere in the chain, it'll return Nothing. Remember that `>>` is only throwing away the wrapped type's value, but it's retaining the monadic context.

[1,2,3] >> [4,5] -- returns [4,5,4,5,4,5]. The first list has 3 instances, so it keeps the "3 instances" part of the first list and mixes it with the monad context of the second!
[1] >> [5,6,7] -- returns [5,6,7]
[] >> [5,6,7] -- returns []
```

These conversion functions are the **only** thing that separate monads from wrapper types in general.

## How to find conversion functions


## Further reading
http://www.google.com/url?q=http%3A%2F%2Fadit.io%2Fposts%2F2013-04-17-functors%2C_applicatives%2C_and_monads_in_pictures.html&sa=D&sntz=1&usg=AFQjCNGNGa2b94jgrzPd7l7FIJQoUoO44g
This is the best monad tutorial in history. It's useful if:
 - you'd like a better conceptual understanding of Monads
 - you'd like to see the distinction between Functor, Applicative and Monad that I totally glossed over

http://blog.tmorris.net/posts/20-intermediate-haskell-exercises/
Complete these 20 exercises, and you will understand everything there is to know about Monads.
You'll need a good understanding of Haskell syntax to undertake this -- including partially-applied functions, lambdas, and type signatures.


