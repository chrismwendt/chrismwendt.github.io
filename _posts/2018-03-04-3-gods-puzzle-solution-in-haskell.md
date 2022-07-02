---
title: 3 gods puzzle solution in Haskell
date: 20180304T00:00Z
---

# The Puzzle

Here's the prompt from [Wikipedia][wikipedia]:

> Three gods A, B, and C are called, in no particular order, True, False, and Random. True always speaks truly, False always speaks falsely, but whether Random speaks truly or falsely is a completely random matter. Your task is to determine the identities of A, B, and C by asking three yes-no questions; each question must be put to exactly one god. The gods understand English, but will answer all questions in their own language, in which the words for yes and no are da and ja, in some order. You do not know which word means which.

# My solution

The puzzle can be simplified by eliminating redundancies masquerading as complications:

- It doesn't matter that the gods speak a different language because you can interpret "da" as "yes" by asking the god to negate their response if "da" means "no".
- It doesn't matter that one god lies because you can ask the god to negate their response if they are False.

It's clear that you can't ask each god a question because since one responds randomly, you'll only get 2 bits of information (which is only enough to enumerate 4 outcomes, and there are 6 permutations of gods). Therefore, you have to spend a question to find a non-random god to ask the remaining questions.

```
Ask A: Is B Random?
  Yes -> Ask C: Is A Random?
    Yes -> Ask C: Is C True?
      Yes -> RFT
      No -> RTF
    No -> Ask C: Is C True?
      Yes -> FRT
      No -> TRF
  No -> Ask B: Is A Random?
    Yes -> Ask B: Is B True?
      Yes -> RTF
      No -> RFT
    No -> Ask B: Is B True?
      Yes -> FTR
      No -> TFR
```

This code performs 10 sample runs with a random permutation each time (which only gives probabilistic evidence that it's correct).

<!-- {% embed https://gist.github.com/chrismwendt/a48cf10fd90d2092b0760a6df6d75438 %} -->

[wikipedia]: https://en.wikipedia.org/wiki/The_Hardest_Logic_Puzzle_Ever
