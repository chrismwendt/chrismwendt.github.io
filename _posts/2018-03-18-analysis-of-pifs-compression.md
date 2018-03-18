---
layout: post
title: Analysis of pifs "compression"
categories:
- blog
---

Issue [#10][2] of [pifs][1] (a file system that encodes files as their index in pi) asks about the expected amount of space that the index would take, and I became curious about this and did some research:

The index in pi of a file with `n` bits is practically random. It's [geometrically distributed][4] with probability `1/(2^n)` (the probability that the file appears at a given index), which means that the average index is `2^n`. The index requires an average of `n` bits to represent, which is the same amount of space that the original file takes.

Since the distribution of indexes is practically random, pifs isn't really a [compression algorithm][5] (which usually targets redundant information).

Here's a histogram of indexes for 2-digit files in random sequences of digits:

```
# NumSamples = 10000; Min = 0.00; Max = 500.00
# 67 values outside of min/max
# Mean = 98.814100; Variance = 9846.521741; SD = 99.229641; Median 68.000000
# each ∎ represents a count of 31
    0.0000 -    25.0000 [  2353]: ∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎
   25.0000 -    50.0000 [  1687]: ∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎
   50.0000 -    75.0000 [  1284]: ∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎
   75.0000 -   100.0000 [  1035]: ∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎
  100.0000 -   125.0000 [   822]: ∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎
  125.0000 -   150.0000 [   581]: ∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎
  150.0000 -   175.0000 [   521]: ∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎∎
  175.0000 -   200.0000 [   392]: ∎∎∎∎∎∎∎∎∎∎∎∎
  200.0000 -   225.0000 [   294]: ∎∎∎∎∎∎∎∎∎
  225.0000 -   250.0000 [   246]: ∎∎∎∎∎∎∎
  250.0000 -   275.0000 [   178]: ∎∎∎∎∎
  275.0000 -   300.0000 [   135]: ∎∎∎∎
  300.0000 -   325.0000 [   111]: ∎∎∎
  325.0000 -   350.0000 [    82]: ∎∎
  350.0000 -   375.0000 [    61]: ∎
  375.0000 -   400.0000 [    54]: ∎
  400.0000 -   425.0000 [    34]: ∎
  425.0000 -   450.0000 [    29]:
  450.0000 -   475.0000 [    23]:
  475.0000 -   500.0000 [    11]:
```

This was generated using [histogram.py][6] and this Haskell code:

```haskell
import Data.List
import Data.Maybe
import System.Random
import Control.Monad
import qualified Data.Algorithms.KMP as KMP
import System.Environment
import Control.Error
import Control.Monad.State
import System.IO.Error

main = flip catchIOError (const (return ())) $ do

  args <- getArgs

  let
    fileLength = args `atMay` 0 >>= readMay ?: 2
    seed = args `atMay` 1 >>= readMay ?: 0
    mkFile = replicateM fileLength (state $ randomR ('0', '9'))
    digits = sequence . repeat $ (state $ randomR ('0', '9'))
    indexOf file = (head . KMP.match (KMP.build file)) <$> digits
    gens = map mkStdGen $ randoms (mkStdGen seed)

  mapM_ print $ map (evalState (mkFile >>= indexOf)) gens

  -- Usage: stack runghc pifs.hs | head -n 10000 | histogram.py -x 500 -b 20
```

[1]: https://github.com/philipl/pifs
[2]: https://github.com/philipl/pifs/issues/10
[4]: https://en.wikipedia.org/wiki/Geometric_distribution
[5]: https://en.wikipedia.org/wiki/Data_compression
[6]: https://github.com/bitly/data_hacks#histogrampy