Method chaining is _so cool_. It makes code readable and delightful to write.

```java
// Java 1
String fullName() {
    return "Sonic" + " The" + " Hedgehog";
}
```

But doing successive write operations on an object isn't like that -- it's verbose. Full of boilerplate and additional elements (each one, a potential source of typo bugs).

```java
// Java 2

String fullName() {
    StringBuilder sb = new StringBuilder();
    sb.append("Sonic");
    sb.append(" The");
    sb.append(" Hedgehog");
    return sb.toString();
}
```

So some smarty figured out that instead of returning `void`, write operations can return a reference to object that was just modified -- which allows chaining again:

```java
// Java 3

String fullName() {
    return new StringBuilder().append("Sonic").append(" The").append(" Hedgehog");
}
```

Cool!

The rest of this article is based on the assumption that having the _option_ to chain methods is better than not having the option:
Here are my arguments in favour of method chains:
 - It's convenient to write. Sometimes we'll write the more convenient idiom if the correct/efficient/complete one is more complicated to write.
 - It's (sometimes) easier to read.
 - It has less elements -- therefore, it has less elements _that can go wrong_. It's less vulnerable to typo-bugs.

We've restored the _option_ to do this operation in one or two lines. It's not as succinct as the original, but it's close (with all of the additional functionality that the object can provide.)

Additional functionality is the key here. For example, let's make a NameBuilder with some additional functionality from StringBuilder:
 - it can generate its final string with different modes (such as uppercase)
 - it will auto-insert the spaces for us (because those are tedious and easy to make a minor bug with)

```java
// Java 4

class NameBuilder {
    private ArrayList<String> names = new ArrayList<String>();
    private boolean uppercase = false;
    
    public NameBuilder append(String name) {
        names.add(name);
        return this;
    }

    public NameBuilder setUppercase(boolean uppercase) {
        this.uppercase = uppercase;
    }
    
    public String toString() {
        // Transform the names
        ArrayList<String> transformedNames;
        if(uppercase) {
            transformedNames = new ArrayList<String>();
            for(String name : names) {
                transformedNames.add(name.toUppercase());
            }
        } else {
            transformedNames = names;
        }
        return implode(transformedNames);
    }

    public static String implode(ArrayList<String> strings) {
        ArrayList<String> result = new ArrayList<String>();
        for(String string : strings) {
            result.append(string).append(" ");
        }
        return result.substring(0, result.length() - 1); // strip the final space
    }
}

String fullName() {
    NameBuilder nb = new NameBuilder();
    nb.append("Sonic");
    nb.append("The");
    nb.append("Hedgehog");
    nb.setUppercase(true);
    return nb.toString();
}

String fullNameChained() {
    return new NameBuilder().append("Sonic").append("The").append("Hedgehog").setUppercase(true).toString();
}
```

This is chainable too.
However, this chaining only works on *setters* -- since we've taken over the intended purpose of Java's return value from methods, we can't use this idiom when the function really needs to return a value:

```java
// Java 5

// reusing the NameBuilder from Java 4

String fullName() {
    NameBuilder nb = new NameBuilder();
    nb.append("Sonic");
    String result1 = nb.toString();
    nb.append("The");
    nb.append("Hedgehog");
    nb.setUppercase(true);
    String result2 = nb.toString();
    return "first name: " + result1 + " and full name: " + result2;
}

String fullNameChained() {
    NameBuilder nb = new NameBuilder().append("Sonic");
    String result1 = nb.toString();
    nb.append("The").append("Hedgehog").setUppercase(true);
    return result1 + nb.toString();
}
```

OK, now let's try similar exercises in Haskell.

```haskell
-- Haskell 1 -- corresponds to Java 1
fullName :: String
fullName = "Sonic" ++ " The" ++ " Hedgehog"
```

OK, so that is super similar to Java 1.
Trying to reproduce Java 2 in Haskell is prety pointless because we can't mutate our local variables, but let's see how close we can get:

```haskell
-- Haskell 2 -- corresponds to Java 2
fullName :: String
fullName = nb3
    where nb3 = nb2 ++ " Hedgehog"
          nb2 = nb1 ++ " The"
          nb1 = "Sonic"
```

Now let's go ahead and create the NameBuilder.

```haskell
-- Haskell 3 -- corresponds to Java 4

import Data.List.Extra

data NameBuilder = NameBuilder {
    names :: [String],
    uppercase :: Bool
}
emptyNameBuilder = NameBuilder { names = [], uppercase = false }

append :: String -> NameBuilder -> NameBuilder
append name nb = nb { names = names nb ++ [name] } -- forget about efficiency in these examples; we're just looking for java/haskell similarity
-- The arguments are defined backward from how we'd expect in Java (normally, the object being operated on comes *first*; in Haskell we leave it to *last*. This helps us do method chaining.

setUppercase :: Bool -> NameBuilder -> NameBuilder
setUppercase uppercase nb = nb { uppercase = uppercase }

toString :: NameBuilder -> String
toString nb = implode transformedNames
    where transformedNames = if uppercase nb
                                 then map upper (names nb)
                                 else names nb

implode :: [String] -> String
implode [] = ""
implode (string:[]) = string
implode (string:strings) = string ++ " " ++ implode strings

implode1a :: [String] -> String
implode1a strings = init (foldr (\string result -> result ++ string ++ " ") "" strings) -- this looks SUPER weird but "foldr" is actually pretty similar to a for-each loop, if you squint. "init" gets every element in the list except the last one. This isn't meant to be efficient!

implode2 :: [String] -> String
implode2 strings = init (concat (map (++ " ") strings))

implode3 :: [String] -> String
implode3 strings = tail (concat (map (' ' :) strings)) -- this is what i would actually write -- it's reasonably efficient

fullName :: String
fullName = toString nb4
    where nb4 = setUppercase True nb3
          nb3 = append "Hedgehog" nb2
          nb2 = append "The" nb1
          nb1 = append "Sonic" nb0
          nb0 = emptyNameBuilder

fullNameChained :: String
fullNameChained = toString (setUppercase True (append "Hedgehog" (append "The" (append "Sonic" emptyNameBuilder)))))
```

This works! `fullName` and `fullNameChained` behave just like in Java.
But - our objective was chainable methods. And this `fullNameChained` isn't really chained at all. Actually, it's horrifically unreadable.
Problems with this so-called "chaining" are:
 - it's in reverse order
 - it has 5 levels of nested parentheses
 - method names and arguments are sprayed out willy-nilly

The easiest way to read this chained method call is to expand it out into multiple lines. That's clearly a failure.

OK, so let's add an infix operator to chain these things. This is gonna take advantage of some slightly more advanced Haskell syntax.

```haskell
-- Haskell 4 -- adding a chaining operator to Haskell 3

-- include everything from Haskell 3 except fullNameChained

(-->) :: NameBuilder -> (NameBuilder -> NameBuilder) -> NameBuilder
(-->) nb function = function nb

fullNameChained :: String
fullNameChained = emptyNameBuilder --> append "Sonic" --> append "The" --> append "Hedgehog" --> setUppercase True --> toString
```

OK, so this operator we've created, called "-->", gives us method chaining. Cool!

fullNameChained = emptyNameBuilder --> append "Sonic" --> append "The" --> append "Hedgehog" --> setUppercase True --> toString

But, what? This is using a bunch of new syntax and language features:
 - functions in type declarations
 - custom operators, operator precedence, and operator associativity
 - currying and partially-applied functions

I'm going to *skip all of those topics* for now -- as there's quite a lot to cover. For now, all I want to demonstrate here is that it's _possible_ to add method chaining as a normal function definition in Haskell.

(As a side note: if you want method chaining in this form, you can `import Control.Lens.Operators` and get an operator called `&` -- which works just like my `-->` operator but it works on *every* type, not just on NameBuilders. The definition is shorter, too!)

Let's aim for the *goal* of this article -- replicating Java 5 and trying to add Method chaining.

```haskell
-- Haskell 5 -- adding method chaining to Java 5
-- reusing everything except the fullName and fullNameChained methods from Haskell 3

fullName :: String
fullName = "first name: " ++ toString nb1 ++ " and full name: " ++ toString nb4
    where nb4 = setUppercase True nb3
          nb3 = append "Hedgehog" nb2
          nb2 = append "The" nb1
          nb1 = append "Sonic" nb0
          nb0 = emptyNameBuilder
          
fullNameChainedMultiLine :: String
fullNameChainedMultiLine = "first name: " ++ toString nb1 ++ " and full name: " ++ toString nb4
    where nb1 = emptyNameBuilder --> append "Sonic"
          nb4 = nb1 --> append "The" --> append "Hedgehog" --> setUppercase True

fullNameChainedOneLine = emptyNameBuilder --> append "Sonic" --> (\nb1 -> nb1 --> append "The" --> append "Hedgehog" --> setUppercase True --> (\nb2 -> "first name: " ++ toString nb1 ++ " and full name: " ++ toString nb4))

fullNameChainedAcrossLines =
    emptyNameBuilder -->
    append "Sonic" -->
    (\nb1 -> nb1 -->
        append "The" -->
        append "Hedgehog" -->
        setUppercase True -->
        (\nb2 -> "first name: " ++ toString nb1 ++ " and full name: " ++ toString nb4))
```

In fullName we had a bit of an advantage -- that we can refer to old copies of the NameBuilder.
As we can't overwrite state, we are keeping all of the state changes -- which happens to be convenient in this case.
(Again, don't be concerned about the efficiency here -- GHC can turn this into efficient code that'll be almost unrecognizable, and full of mutable state too.)

The fullNameChainedMultiLine function is pretty similar to the one in Java 5. It's nice, but we had to break up our method chain -- the more interstitial results we need, the less useful this pattern is going to be.

Trying to execute everything in a single chain is _possible_, but _horrific_ - now we've used nested anonymous functions to fit our style choice, and it's backfired horribly - it's totally unreadable.

The third example is trying to get a bit closer to the _non-chained_ Java syntax. Wait what is our goal again?

Once again, we have the problem wherein 

```haskell
-- Haskell 6

fullNameChained :: String
fullNameChained = "first name: " ++ toString nb1 ++ " and full name: " ++ toString nb4
    where nb1 = emptyNameBuilder >> append "Sonic" >>= (\
          nb4 = nb1 --> append "The" --> append "Hedgehog" --> setUppercase True
