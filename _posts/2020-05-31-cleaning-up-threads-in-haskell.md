---
layout: post
title: Cleaning up threads in Haskell
categories:
- blog
---

When your code gets reloaded in ghcid, only the main thread is killed (via a [`UserInterrupt`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Exception.html) exception). All other spawned threads continue to run. If you don't keep track of your threads and kill them when the main thread receives an exception, you can accidentally hold on to resources that should be released. Unfortunately, [GHCi cannot kill all threads](https://stackoverflow.com/questions/24999636/is-there-a-way-to-kill-all-forked-threads-in-a-ghci-session-without-restarting-i), so it's up to you the programmer to clean up.

Here's how to clean up threads at a high level:

- Keep track of all currently-running threads in an `IORef (Set (Async a))`
- When an exception occurs in any thread, hand it to the main thread with `putMVar exceptionVar exception`
- Have the main thread wait for an exception then kill all currently-running threads

Implementation:

```haskell
main = do
  safeAsync $ runApp

  dieOnException

-- NOINLINE ensures that there is only one MVar
{-# NOINLINE errorVar #-}
errorVar = unsafePerformIO $ newEmptyMVar :: MVar SomeException

-- Set of currently-running threads
{-# NOINLINE asyncsRef #-}
asyncsRef = unsafePerformIO $ newIORef Set.empty :: IORef (Set (Async a))

-- Waits for an exception the kills all threads
dieOnException = (readMVar errorVar >>= print) `finally` (do
  mapM_ cancel =<< readIORef asyncsRef
  putStrLn "exiting entire Haskell process"
  exitFailure)

-- A version of `async` that additionally stores itself in the currently-running threads.
-- It also notifies the main thread when there's an exception.
safeAsync a = do
  asyncValue <- async a
  forkIO $ do
    (wait asyncValue `finally` (mod_ asyncsRef (Set.delete asyncValue))) `catches`
      [ Handler $ \(e :: AsyncCancelled) -> return ()
      , Handler $ \(e :: SomeException) -> putMVar errorVar e
      ]
  mod_ asyncsRef (Set.insert asyncValue)
  return asyncValue
```
