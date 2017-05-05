
# A quick intro to Haskell syntax
## covering `do`, `>>`, `>>=`, `IO`, and `return`

Hi! This guide is for you. You're:
 - able to read javascript
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

```javascript
function hypotenuse(edge1, edge2) {
    return Math.sqrt(edge1 * edge1 + edge2 * edge2);
}

function makeRightAngleTriangle() {
    const edge1 = randomInt();
    console.log('give me a number');
    const edge2 = readIntFromStdin();
    const edge3 = hypotenuse(edge1, edge2);
    return [edge1, edge2, edge3];
}
```

```haskell
hypotenuse :: Double -> Double -> Double
hypotenuse edge1 edge2 = sqrt (edge1 * edge1 + edge2 * edge2)

makeRightAngleTriangle :: IO [Double]
makeRightAngleTriangle = do
    edge1 <- randomIO
    putStrLn "give me a number"
    edge2 <- parse readStrLn
    edge3 <- hypotenuse edge1 edge2
    return [edge1, edge2, edge3]
```

```haskell
hypotenuse :: Double -> Double -> Double
hypotenuse edge1 edge2 = sqrt (edge1 * edge1 + edge2 * edge2)

makeRightAngleTriangle :: IO [Double]
makeRightAngleTriangle =
    randomIO >>= (\edge1 -> 
    putStrLn "give me a number" >>
    parse readStrLn >>= (\edge2 ->
    hypotenuse edge1 edge2 >>= (\edge3 ->
    return [edge1, edge2, edge3])))
```

# 1) Starting out
```javascript
function makeRightAngleTriangle() {
    return [3, 4, 5];
}
```

```haskell
makeRightAngleTriangle :: [Double]
makeRightAngleTriangle = [3, 4, 5]
```

Note: `return` in Javascript and `return` in Haskell are **completely** unrelated. It's a coincidence that they appear at the same place in the functions above.
We'll cover its functionality later -- but for now, we don't need it.

Note:
```haskell
makeRightAngleTriangle :: [Double]
```
This is a **type annotation**. If you've used FlowType or TypeScript then you've seen similar stuff.
This one says "the function called `makeRightAngleTriangle` takes no arguments, and returns a list of `Double` (`Double` is analogous to the `Number` type in Javascript).

This function does what it says on the tin, but it's pretty useless. :) Let's compute the hypotenuse from the other two.

# 2) Function calls
```javascript
function hypotenuse(edge1, edge2) {
    return Math.sqrt(edge1 * edge1 + edge2 * edge2);
}

function makeRightAngleTriangle() {
    var edge1 = 3;
    var edge2 = 4;
    var edge3 = hypotenuse(edge1, edge2);
    return [edge1, edge2, edge3];
}
```

```haskell
hypotenuse :: Double -> Double -> Double
hypotenuse edge1 edge2 = sqrt (edge1 * edge1 + edge2 * edge2)

makeRightAngleTriangle :: [Double]
makeRightAngleTriangle = [3, 4, 5]
```

Breaking this down:
```haskell
hypotenuse :: Double -> Double -> Double
```

hypotenuse is a 

