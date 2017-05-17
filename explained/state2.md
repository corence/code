
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
import Control.Monad.State  

type Stack = [Int]

pop :: State Stack Int  
pop = state $ \(x:xs) -> (x,xs)  

push :: Int -> State Stack ()  
push a = state $ \xs -> ((),a:xs)  

stackManip :: State Stack Int  
stackManip = do  
    push 3  
    x <- pop  
    pop


```java
import java.util.TreeMap;

public class Citizen {
    public final String name;
    public final String[] family;

    public Citizen(String name, String[] family) {
        this.name = name;
        this.family = family;
    }
}

public class Nation {
    private final TreeMap<String, Citizen> people = new TreeMap<String, Citizen>();

    public void add(Citizen citizen) {
        people.put(citizen.name, citizen);
    }
    
    public void exile(String name) {
        people.remove(name);
    }
}
```

```haskell
import Data.Map

data Nation = Nation (Map String Citizen) -- all citizens, indexed by name
data Citizen = Citizen String [String] -- name, and the names of their family

emptyNation :: Nation
emptyNation = Nation empty -- "empty" is a method of Data.Map; it gives us an empty Data.Map

add :: Citizen -> Nation -> Nation
add (Citizen name family) (Nation people) = Nation (insert name (Citizen name family) people)

add citizen (Nation people) = Nation (insert name citizen people)
    where Citizen name family = citizen

evict :: String -> Nation -> Nation
evict name (Nation people) = Nation (delete name people)
```

These are pretty much the same thing -- except the Java type is mutable, and the Haskell type is immutable.
The haskell syntax is a little clumsy in the `add` method, versus Java's `object.member` syntax, but it's not too bad.

Let's setup a sample nation.

```java
function makeNation1() {
    Citizen fred = new Citizen("Fred", new String[] {"Wilma", "Pebbles"});
    Citizen wilma = new Citizen("Wilma", new String[] {"Fred", "Pebbles"});
    Citizen pebbles = new Citizen("Pebbles", new String[] {"Fred", "Wilma"});
    Nation nation = new Nation();
    nation.add(fred);
    nation.add(wilma);
    nation.add(pebbles);
    return nation;
}

function makeNation2() {
    Nation nation = new Nation();
    nation.add(new Citizen("Fred", new String[] {"Wilma", "Pebbles"}));
    nation.add(new Citizen("Wilma", new String[] {"Fred", "Pebbles"}));
    nation.add(new Citizen("Pebbles", new String[] {"Fred", "Wilma"}));
    return nation;
}
```

```haskell
makeNation1 :: Nation
makeNation1
  = let fred = Citizen "Fred" ["Wilma", "Pebbles"]
        wilma = Citizen "Wilma" ["Fred", "Pebbles"]
        pebbles = Citizen "Pebbles" ["Fred", "Wilma"]
    in add pebbles (add wilma (add fred (emptyNation)))
```

This Java method is great. Apart from Java's clumsy array literal syntax, it's clear what's happening, and it'd be easy to intersperse some other calls in here.
Both methods are doing the same thing; the first one just uses extra variables.

We're aiming to write some Haskell that looks similar.

The first Haskell implementation suffers from massive nesting. It's also in reverse order (the rightmost thing happens first).
This would be really confusing if we were adding a few more people -- and it'd be especially confusing if we were intermixing `add` with other function calls.
It's not great.

We _could_ initialise the `Data.Map` directly -- but that'd be poor coding style and wouldn't work in every situation. We don't really need it.

Fortunately, there's a better way...

```haskell
makeNation2 :: Nation
makeNation2 = return emptyNation >> add fred >> add wilma >> add pebbles
```

```java
    nation.add(new Citizen("Barney", new String[] {"Betty", "Bam-Bam"}));
    nation.add(new Citizen("Betty", new String[] {"Barney", "Bam-Bam"}));
    nation.add(new Citizen("Bam-Bam", new String[] {"Barney", "Betty"}));
```

