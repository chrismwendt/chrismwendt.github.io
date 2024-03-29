---
title: The cost of enforcing purity in Haskell
date: 20200527T00:00Z
---

["Effect Tracking Is Commercially Worthless"](https://degoes.net/articles/no-effect-tracking) argues that enforcing purity is not commercially valuable, and this struck a chord with with me because I had already recently started to question the usefulness of enforcing purity in the context of a commercial project [CodeWyng](http://codewyng.io/) that I'm working on.

Looking past the distractions (the tongue-in-cheek sales rhetoric, the conspicuous advertisement for the ZIO project, and the vacuous is-Haskell-really-pure debate), there are some compelling arguments against enforcing purity that I've never heard before. The novelty of the counterarguments has drawn my attention to studying this subject further.

Because "effect tracking" is ambiguous and causes confusion, these terms might be useful in discussion:

- **Enforcing purity** prevents the use of functions that interact with the world to be used in any context that doesn't allow it. The mechanism by which purity is enforced in Haskell is a combination of the type system (e.g. you can't use `IO Int` in a function like `sum :: [Int] -> Int`) and the specific interface to `IO` exposed to the programmer (i.e. all functions that interact with the world return something wrapped in `IO` and there is no `IO a -> a` to circumvent the intended interface, except for the backdoor `unsafePerformIO`).
- **Dependency injection** is a technique where the implementation of dependencies of a function can be changed at the call site. For example, the dependency on the database can be swapped out for a mock implementation in tests. Some mechanisms by which dependency injection can be performed include: passing an explicit argument `runApp :: DB -> IO ()`, adding a type class constraint `runApp :: (HasDB m, MonadIO m) => m ()`, or mutating the global DB variable to set it to a mock implementation prior to running tests.

As an aside, I don't think John is saying that the dependency-injection-like property of effects systems is worthless, but rather that any restriction-of-side-effects property (just like `IO` has) is worthless.

**Original motivation for `IO` in Haskell:** ensure that expressions that describe interactions with the world are evaluated in a deterministic order (specified by the programmer) even under lazy evaluation and in the presence of an optimizing compiler, while maintaining purity. Paper for reference: [Imperative functional programming](https://www.microsoft.com/en-us/research/wp-content/uploads/1993/01/imperative.pdf)

**Mechanism by which `IO` enforces purity:** the `IO` data type is essentially the state monad where the world is the state `data IO a = IO (World -> (a, World))` and the `>>=`+`return` interface exposed to the programmer only enables chaining `IO` values together such that each value of type `World` is used exactly once, passing it from one `IO` action to the next in sequence. In other words, each function that takes a `World` (e.g. `getLine :: IO String`, `getLine :: World -> (String, World)`) is never called with the same `World` value twice. All function calls that interact with the world take a different `World` value, and therefore it's impossible to observe the same function applied to the same arguments returning a different value. That's precisely the definition of referential transparency, A.K.A. purity.

**Motivation for enforcing purity:** I found less information on this topic, so I'm mostly going off of intuition here.

- STM is guaranteed to be correct
- Equational reasoning always holds (i.e. factoring out common code into a binding never changes program behavior)
- The type of a function documents whether or not it interacts with the world
- Programs tend to be written in such a way that interactions with the world are more separated from logic than they would be without enforced purity
- It's the only lazy+pure language and a lot of learning has come out of programming with that mind-bending combination of properties (myself included)
- There's a pervasive undercurrent of "it's good for you" in a "slavery is freedom" sort of way, and that programmers lack the discipline required to architect large programs without the enforcement of purity
- Restrictions like purity or linear types enable mathematical reasoning by way of connection to logic, category theory, and algebra. [Constraints Liberate, Liberties Constrain](https://www.youtube.com/watch?v=GqmsQeSzMdw) explores this tradeoff.

**Motivation for not enforcing purity:** Starting with John's reasons, with a few I thought of:

- If it were worthwhile, annotation processors would be in widespread use for mainstream languages and have IDE support (proof by contradiction)
- Tooling alone using static call tree analysis would achieve the same goal without placing a burden on the programmer
- The [Checker framework](https://checkerframework.org/manual/#purity-checker) has no commercial traction
- PureScript went from pure+tags to pure (still enforces purity, but with less granularity)
- Almost every other aspect of FP has been widely adopted while enforced purity has not: first-class and anonymous functions (e.g. in Java 8), lexical scoping (e.g. arrow functions for `this` and `let` vs `var` in JavaScript), immutable annotations (e.g. `const` vs `let` in JavaScript), option chaining (e.g. C#'s and TypeScript's `?.` operator), the `Maybe` data type (e.g. `Optional` in Java), parser combinators, QuickCheck, STM, list comprehensions, pattern matching, parametric polymorphism (A.K.A. generics), type inference, the list goes on...
- Programmers already know using intuition with a high degree of certainty whether or not a function interacts with the world. Functions that violate that intuition are avoided and are quickly rectified or worked around (e.g. [`java.net.URL#hashCode/equals`](https://news.ycombinator.com/item?id=21765788)
- Enforcing purity places burdens of boilerplate and ceremony on programmers (e.g. when to use `let` vs `<-`, doing acrobatics with `<$>`/`fmap`/`=<<`/`>>=`/`<*>`/etc. to satisfy the type checker, the need to add an import of `Debug.Trace` and use special functions to be able to inspect the execution of functions that definitely don't interact with the world, and the need to create a new variable binding for many trivial values such as `count <- readIORef countRef`)
- Telling beginners "don't worry, you don't need to understand what `IO` is or how it works, just use these combinators you won't understand for the first 20 hours of learning Haskell" is Haskell's equivalent of "oh just ignore all that `public static void main String[] args` stuff for now and don't forget to wrap your hello world in a class"
- I can't remember ever accidentally running IO at all or in the wrong order in any language (not just Haskell). It just doesn't seem to happen. I've been bitten by mutation quite a few times, but they were always a conscious choice of tradeoffs or something silly like JavaScript's `array.reverse()` which both mutates the array and returns a reversed shallow copy.

**Experiment with strict impure Haskell** To get another data point, I'm considering modifying the Haskell backend of CodeWyng to use `LANGUAGE Strict` and insert `unsafePerformIO` in a bunch of places, especially at usages of `IORef`s and `MVar`s to see 1) if it works 2) if there are any bugs 3) if it's easier or more intuitive to program that way. Will post back here if/when I do. **Update:**

- Adding `{-# LANGUAGE Strict #-}` worked right away, I didn't notice any bugs
- While replacing `IO` functions with impure non-`IO` versions, I replaced `withMVar` with an impure version and doing so caused an infinite loop, so I reverted that one
- I found some dead code that was doing the equivalent of `x <- readMVar xVar`
- Inlining variables into pure expressions helped align the code and prettify it
- Some control functions like `unlessM` broke in unexpected ways
- All I could think about for the last 25% was "man, all these tricks with `>>=`/`<-`/`<$>`/`=<<` to retain purity are completely unnecessary"

**Value of enforcing purity in CodeWyng** The first version of the part of CodeWyng that is now written in Haskell was originally written in TypeScript, and I switched exclusively for Haskell's vastly superior runtime support for concurrency and the Haskell ecosystem's stream parsing libraries. I definitely didn't switch in order to prevent myself from inadvertently mixing pure/impure code, and if it were to be rewritten in some other language (TypeScript/Go/etc.), I wouldn't consider the loss of enforcement of purity a downside.

Conversation on [Zulip](https://funprog.srid.ca/haskell/effect-tracking-is-worthless.html)
