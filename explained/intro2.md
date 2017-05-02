Most monad tutorials start with
> A monad is *like*….
or
> A monad is *for*…
or
> In category theory…

but I haven’t come across one with a succinct definition of what a monad *is*, in the context of programming languages.

A monad is a *wrapper type* with a specific set of *conversion functions*.

So this tutorial covers:
- what is a wrapper type
- what are monads for
- what are the monad conversion functions for


*Wrapper type*

A wrapper type is a type that *does stuff with another type*. This is a vague definition so let’s get straight to examples:

1) in Java, a `List<T>` is a wrapper type. To make an instance of it, you pass it another type — like `List<Integer>`. Note that:
- an instance of `List<Integer>` does not _necessarily_ wrap an instance of `Integer`. All it means is that this wrapper type can do _something_ with the wrapped type.
- List has _no knowledge_ of the Integer type — it can’t do arithmetic or increment the Integer values. It can only deal with its wrapped type as an opaque black box.


2) in javascript, a Promise also wraps an arbitrary type. (If you’re using TypeScript, a promise is `Promise<string>` — but even if you’re in pure Javascript and your types aren’t explicit in your code, the concept still exists.)


*What are Monads for*

What problems are Monads capable of solving? Answer: *The same set of problems that any other wrapper type can solve.*


*Conversion Functions*

The standard Monad conversion functions are for dealing with any situations where:
- you have an instance of the wrapped type, but you need an instance of the unwrapped type
- you have an instance of the unwrapped type, but you need an instance of the wrapped type

These are the *only* thing that separate monads from wrapper types in general.

Note that in most cases, *the conversion functions are purely for convenience*.
For most Monads, you can achieve results without touching their monad functions — it’ll just take a lot of code and be ugly to read and write.

(unfortunate note for the Haskellers: you’ll need to use some kind of Monad functions when dealing with `IO`! they’re not just convenience in that context.) (edited)

jobjob [1:24 PM] 
nice, can you flesh out the what are monads for section with an example or two

corence [1:25 PM] 
yeah.

the main thing i want to emphasise is that any questions about monads such as
“what is a monad”?
or
“what are monads for”?

has 2 answers — one about wrapper types, and one about conversion functions

jobjob [1:25 PM] 
yeah it's very good, I like it

corence [1:30 PM] 
probably the examples will all follow this format:

- some unsafe/shit java code
- some safe/correct java code — that is really long and hard to read
- some equivalent safe/correct Haskell code — that is really long and hard to read
- some Haskell code using conversion functions — that is really short and sweet and nice to read

jobjob [1:30 PM] 
cool beans

corence [1:30 PM] 
the main purpose of the conversion functions is so that you can chain method calls without having to do checks in between

jobjob [1:30 PM] 
use javascript?

corence [1:30 PM] 
possibly yes for javascript, but it’d have to be typescript i think

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
