---
layout: post
title: What if runST didn't use existential quantification?
categories:
- blog
---

Here's the [`ST`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Monad-ST.html#t:ST) API:

```haskell
data ST s a

runST :: (forall s. ST s a) -> a
newSTRef :: a -> ST s (STRef s a)
readSTRef :: STRef s a -> ST s a
writeSTRef :: STRef s a -> a -> ST s ()
```

Let's drop `forall s` and see what happens:

```haskell
data ST s a

runST :: ST s a -> a
newSTRef :: a -> ST s (STRef s a)
readSTRef :: STRef s a -> ST s a
writeSTRef :: STRef s a -> a -> ST s ()
```

Well, now `ST` can be eliminated by calling `runST` after any other operation (e.g. `runST . newSTRef`):

```haskell
newSTRef :: a -> STRef s a
readSTRef :: STRef s a -> a
writeSTRef :: STRef s a -> a -> ()
```

At the type level, `writeSTRef` looks useless, but it still has the side effect of writing a value to an `STRef`.

Now the order of evaluation matters, and purity goes out the window:

```haskell
let ref = newSTRef "1"
    _ = writeSTRef ref "2"
in readSTRef ref -- Does this evaluate to "1" or "2"?
```

`forall s` is the linchpin that enforces sequential reads/writes in the `ST` monad.
