---
layout: post
title: What if runST didn't use existential quantification?
categories:
- blog
---

Here's the ST API:

```haskell
data ST s a

runST :: (forall s. ST s a) -> a
newSTRef :: a -> ST s (STRef s a)
readSTRef :: STRef s a -> ST s a
writeSTRef :: STRef s a -> a -> ST s ()
```

What if instead, `runST :: ST s a -> a`? Then you could easily make a new API:

```haskell
newSTRef' :: a -> STRef s a
readSTRef' :: STRef s a -> a
writeSTRef' :: STRef s a -> a -> ()
```

This makes the output dependent on order of evaluation and whether or not code is optimized away:

```haskell
let
  n = newSTRef' 0
in
  print (writeSTRef' n 1, readSTRef' n) -- prints ((), 0) or ((), 1)?
```

It also breaks equational reasoning:

```haskell
let { n = newSTRef' 0; _ = writeSTRef' n 1; } in readSTRef' n
-- is not the same as
let { n = newSTRef' 0; } in readSTRef' n
-- which is not the same as
readSTRef' (newSTRef' 0)
-- which is not the same as
0
```

The existential type variable `s` in `runST :: (forall s. ST s a) -> a` seems to prevent operations on references from being performed outside of a monad, which enforces evaluation order.