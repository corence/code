
# Haskell Syntax: `>>` and `>>=` explained

In this post, we're aiming to create an intuitive understanding of Haskell's `>>` and `>>=` functions, by analogy to Java's `;` and method chaining.

# Reasons to learn this
 - Many Haskell code samples make use of them -- and they're totally unreadable until you have an intuitive understanding of these operators
 - Method chaining with immutable types -- that's cool!

# Suggested prerequisites
Functions and operators
Interfaces and type classes

# Method chaining
Suppose you're arch-emperor of Planet Earth. Clearly, you need the ability to exile people from nations. Let's define a class and a datatype for that.

```java
import java.util.Set;

class ImmutableNation {
    private final Set<String> people = new Set<String>();

    public ImmutableNation(Set<String> people) {
        this.people = people;
    }

    public ImmutableNation add(String name) {
        ImmutableNation result = new ImmutableNation(this.people.clone());
        result.people.add(name);
        return result;
    }
    
    public void exile(String name) {
        ImmutableNation result = new ImmutableNation(this.people.clone());
        result.people.remove(name);
        return result;
    }
}
```

```haskell
import Data.Set

data ImmutableNation = ImmutableNation (Set String)

add :: String -> ImmutableNation -> ImmutableNation
add name (ImmutableNation people) = ImmutableNation (insert name people)
```

These are pretty much the same thing.
```java
import java.util.Set;

class MutableNation {
    private final Set<String> people = new Set<String>();

    public void add(String name) {
        people.add(name);
    }
    
    public void exile(String name) {
        people.remove(name);
    }
}
```

