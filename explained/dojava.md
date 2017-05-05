
# Haskell syntax in one page
## if you know Java, now you know Haskell

Hi! This guide is for you. You're:
 - able to read java
 - intending to learn Haskell
 - ...but you're getting compile errors when dealing with IO
 - ...or having trouble reading code examples online (as they keep swapping between `do` blocks and regular function calls)
 - and you've discovered that in order to read these code blocks, you'll need to learn about monads
 - ...but most monad tutorials **are written in Haskell syntax**

Endless loop detected!

This article will show you how to read and write basic Haskell syntax, to help you get kick-started and bring you up to your productivity in other programming languages.

It'll walk through some of the syntax errors you're likely to run into, and how to resolve them.

# Outcomes

Here are the final forms of the program that we're working toward. We'll explain every element in these two Haskell programs.
If you can see how the following three code blocks are almost identical, and you know how to write each of them, then this article will be of no use to you. :)

```java
public class Triangle {
    public double edge1;
    public double edge2;
    public double edge3;
    public Triangle(double edge1, double edge2, double edge3) {
        this.edge1 = edge1; this.edge2 = edge2; this.edge3 = edge3;
    }
}

double hypotenuse(double edge1, double edge2) {
    return Math.sqrt(edge1 * edge1 + edge2 * edge2);
}

Triangle makeRightAngleTriangle() {
    double edge1 = randomInt();
    console.log("give me a number");
    double edge2 = readIntFromStdin();
    double edge3 = hypotenuse(edge1, edge2);
    return new Triangle(edge1, edge2, edge3);
}
```

```haskell
data Triangle = Triangle Double Double Double

hypotenuse :: Double -> Double -> Double
hypotenuse edge1 edge2 = sqrt (edge1 * edge1 + edge2 * edge2)

makeRightAngleTriangle :: IO Triangle
makeRightAngleTriangle = do
    edge1 <- randomIO
    putStrLn "give me a number"
    edge2 <- parse readStrLn
    edge3 <- hypotenuse edge1 edge2
    return (Triangle edge1 edge2 edge3)
```

```haskell
data Triangle = Triangle {
    edge1 :: Double,
    edge2 :: Double
    edge3 :: Double
}

hypotenuse :: Double -> Double -> Double
hypotenuse edge1 edge2 = sqrt (edge1 * edge1 + edge2 * edge2)

makeRightAngleTriangle :: IO Triangle
makeRightAngleTriangle =
    randomIO >>= (\edge1 -> 
    putStrLn "give me a number" >>
    parse readStrLn >>= (\edge2 ->
    hypotenuse edge1 edge2 >>= (\edge3 ->
    return (Triangle edge1 edge2 edge3))))
```

# 1) Starting out
```java
public class Triangle {
    public double edge1;
    public double edge2;
    public double edge3;
    public Triangle(double edge1, double edge2, double edge3) {
        this.edge1 = edge1; this.edge2 = edge2; this.edge3 = edge3;
    }
}

Triangle makeRightAngleTriangle() {
    return new Triangle(3, 4, 5);
}
```

```haskell
data Triangle = Triangle Double Double Double

makeRightAngleTriangle :: Triangle
makeRightAngleTriangle = Triangle 3 4 5
```

Note: `return` in Java and `return` in Haskell are **completely** unrelated. It's a coincidence that they appear at the same place in the functions above.
We'll cover its functionality later -- but for now, we don't need it.

Note: Haskell function calls look pretty different to Java's function calls because they don't use `(` or `,`. You just write the function name (in this case, the constructor called `Triangle`) -- then list the arguments, separated by spaces. This has a cool advantage (partial function application) but we're not covering that today.

Note:
```haskell
makeRightAngleTriangle :: Triangle
```
This is a **type annotation**. It means the same as `Triangle makeRightAngleTriangle()` -- no arguments, and `Triangle` as the return type.

This function does what it says on the tin, but it's pretty useless. :) Let's bind some local variables before we go more complicated.

# 2) Variables
```java
public class Triangle {
    public double edge1;
    public double edge2;
    public double edge3;
    public Triangle(double edge1, double edge2, double edge3) {
        this.edge1 = edge1; this.edge2 = edge2; this.edge3 = edge3;
    }
}

double makeRightAngleTriangle() {
    double edge1 = 3;
    double edge2 = 4;
    double edge3 = 5;
    return new double[] {edge1, edge2, edge3};
}
```

Now we're binding variables.

We're going to look at a few different ways to express this in Haskell:

```haskell
data Triangle = Triangle Double Double Double

makeRightAngleTriangle :: Triangle
makeRightAngleTriangle
    = let
        edge1 = 3
        edge2 = 4
        edge3 = 5
        in Triangle edge1 edge2 edge3
```

`let` is a scoped block. Consider in Java:
```java
for(int i = 0; i < 3; i++) {
}
i = 4; // this is a syntax error!
```
the `i` doesn't live beyond the scope of the `for` block.
The same rule applies to `let` and `in`.

Note: we don't need to specify that `edge2` is of type Double.
Haskell's type system infers that it **has** to be Double or else the code won't compile.

Note -- the newline before the first `=` is optional -- I added it for readability.
Indentation rules in Haskell are roughly like this:
 - if you start a new line with a bigger indent than the previous line, it means you're continuing the line above
 - if you start a new line with the same indent as the previous line, it means you're starting a new line at the same scope
 - if you start a new line with a smaller indent than the previous line, it means you're ending a scope
This is *roughly* correct, but has a bunch of loopholes. If you have indentation issues, it'll almost certainly cause a compile error, so you can experiment until the compiler stops complaining.

```haskell
data Triangle = Triangle Double Double Double

makeRightAngleTriangle :: Triangle
makeRightAngleTriangle
    = Triangle edge1 edge2 edge3
          where edge1 = 3
                edge2 = 4
                edge3 = 5
```

This one is using `where` instead of `let`. They're pretty similar.
The variables in the `where` are scoped to their parent expression (in this case, the `Triangle edge1 edge2 edge3`).

We'll use `let` for the remainder of this article.

# 3) Function calls
```java
public class Triangle {
    public double edge1;
    public double edge2;
    public double edge3;
    public Triangle(double edge1, double edge2, double edge3) {
        this.edge1 = edge1; this.edge2 = edge2; this.edge3 = edge3;
    }
}

double hypotenuse(double edge1, double edge2) {
    return Math.sqrt(edge1 * edge1 + edge2 * edge2);
}

double makeRightAngleTriangle() {
    double edge1 = 3;
    double edge2 = 4;
    double edge3 = hypotenuse(edge1, edge2);
    return new double[] {edge1, edge2, edge3};
}
```

Now we're calling a function.

```haskell
data Triangle = Triangle Double Double Double

hypotenuse :: Double -> Double -> Double
hypotenuse edge1 edge2 = sqrt (edge1 * edge1 + edge2 * edge2)

makeRightAngleTriangle :: Triangle
makeRightAngleTriangle
    = let
        edge1 = 3
        edge2 = 4
        edge3 = hypotenuse edge1 edge2
        in Triangle edge1 edge2 edge3
```

The **type signature** of the `hypotenuse` function says: "This function takes 2 arguments -- both of type Double -- and it returns a Double."
The type after the rightmost `->` is the return type of the function.

Note: the above 2 lines are not quite accurate -- but they're an *analogy* to help us map the concepts with Java. :)
In reality, Haskell functions are more flexible than this -- but we're not covering partial function application in this article, so for now just pretend that what I've said is true!

You'll note that the Haskell syntax for defining a function is *extremely* terse.
