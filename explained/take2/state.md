
# Control.Monad.State

## Prerequisites

 - polymorphic types
 
## Helpful

 - IO
 - Partial function application

## Synopsis

A State object is a wrapper for a function that can read and write some state.

For example, if we're simulating a card game, then we could consider each player's hand to be a bit of state that we can read and write during the course of the game.

Control.Monad.State is designed to wrap the functions that *modify* each player's hand.

## What's it for?

What the State type allows us to do is *combine* several state-modifying and state-querying functions into one.

Why we'd want to do that: What this allows us to do is, to do several state operations in a row.

Example: let's say we have a 

```haskell

removeHighest

```
