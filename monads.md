# Monads - a two-part definition
## Preface

This document could be useful if you want to be effective with Monads and:
 - you're still seeking an intuitive understanding of their form and purpose
 - you want to learn about Monads without having to learn Haskell or Scala syntax at the same time
 - you know Java syntax (or something similar, such as TypeScript, C#, or C++)

## Definition

In my experience, most monad tutorials start with one of these three phrases:
> A monad is *like*...
> A monad is *for*...
> In category theory...

but I haven’t come across one with a succinct definition of what a monad **is**, in the context of programming languages.

So -- here's my definition:

> A monad is a **wrapper type** with a specific set of **conversion functions**.

So let's break this further down, and explore:
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
 - `[String]` (equivalent to `List<String>`)
 - `Maybe Integer` (equivalent to `Optional<Integer>`. `Maybe a` is the wrapper type; `Integer` is the wrapped type)
 - `IO String` (this type represents "a `String` that I had to do some input/output to obtain")

Note that an instance of a wrapper type does not necessarily contain an instance of the wrapped type -- you can't get a `String` from a `Comparable<String>`.

### What monads are for (considering them as wrapper types)
Answer: The same set of problems that any other wrapper type can solve.

Remember, a monad is just a **wrapper type** with a specific set of **conversion functions**.

## Conversion functions
### What they do
Monads (and the related types -- Functor and Applicative) take a function that accepts an **unwrapped** type, and call it as if it were a function that takes a **wrapped** type.

**map** is an example of a conversion function. It exists across many programming languages. (In Java 8, it's in `java.util.stream.Streams`.)

It allows you to call functions that **don't know about your wrapper type** (in this case -- array or list), without having to deal with unwrapping your data.

#### Quick examples:

Javascript
```javascript
[1, 3, 4, 5].map(x => x * 2); // returns [2, 6, 8, 10]. Note that our function, "x => x * 2", doesn't know or care what an Array is
```

Haskell -- 
```haskell
-- Some examples of map -- analogous to the javascript above
map (\x -> x * 2) [1, 3, 4, 5] -- returns [2, 6, 8, 10]. This is the array-specific special case of map.
fmap (\x -> x * 2) [1, 3, 4, 5] -- fmap is a more general version of map -- that works on **every** Monad type, not just Lists.
```
The above code is the same as the javascript code.

Some examples of fmap, applied to other types:

```haskell
fmap (\x -> x * 2) (Just 3) -- returns 6
fmap (\x -> x * 2) (Nothing) -- returns Nothing
fmap (\x -> x * 2) (randomIO) -- gets a random number, then doubles it. Note that random numbers in Haskell will be wrapped in the IO monad type, because "talking to the random number generator" is considered a form of input (and possibly output)
```

Almost all wrapper types already have some way to convert data between the wrapped and the unwrapped forms. But the monad conversion functions allow us to write general code that works across **all** monads.

For example, in Haskell it's possible to write a function that can do **all** of these things:
 - convert a 
 - convert a list of IO objects -- for example, an `[IO Integer]` -- to an IO object wrapping a list -- eg, an `IO [Integer]`


These conversion functions are the **only** thing that separate monads from wrapper types in general.

### What can we do with conversion functions



jobjob [1:31 PM] 
Java is dead (and probably also the top language on tiobe)

corence [1:43 PM] 
First example — Null-checking vs Chaining

suppose you want to know the name of the country that a person is in
you could say (in java):

```String getCountryName(Dude dude) {
    return dude.getCity().getCountry().getName();
}
```
but that’ll throw an exception if i’m in the ISS (so I don’t have a country)

to correct that, we need to check for null:
```String getCountryName(Dude dude) {
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

or you could use exceptions instead:
```String getCountryName(Dude dude) {
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
```getCountryName :: Maybe Dude -> Maybe String
getCountryName maybeDude
  = case (maybeDude) of
      Just dude -> case (getCity dude) of
          Just city -> case (getCountry dude) of
              Just country -> getName country
              Nothing -> Nothing
          Nothing -> Nothing
      Nothing -> Nothing
```

but if you use the Monad function called `>>=` then you can tell it “if you get a `Nothing`, just give me back a `Nothing`” — and then it’ll look like this:
```getCountryName :: Maybe Dude -> Maybe String
getCountryName maybeDude = maybeDude >>= getCity >>= getCountry >>= getName
```

This final Haskell version is about as short as the first Java version — but it behaves like the null-safe versions. (edited)

