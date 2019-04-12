---
layout: post
title: Altnerative Applicative and Monad formulations
categories:
- blog
---

There's a little known alternative class definitions of Applicative and Monad that I haven't seen presented in an article before, so I thought I'd share:

```haskell
class Functor f => Applicative f where
  pure :: f ()
  (**) :: f a -> f b -> f (a, b)

-- Left identity : pure ** v     ≅ v
-- Right identity: v    ** pure  ≅ v
-- Associativity : u ** (v ** w) ≅ (u ** v) ** w
-- (≅ means isomorphism)

class Applicative m => Monad m where
  join :: m (m a) -> m a

-- join . fmap join = join . join
-- join . fmap pure = join . pure = id
```

See:

- [https://stackoverflow.com/questions/45829110/monad-laws-expressed-in-terms-of-join-instead-of-bind/45829556](https://stackoverflow.com/questions/45829110/monad-laws-expressed-in-terms-of-join-instead-of-bind/45829556)
- [https://wiki.haskell.org/Typeclassopedia#Alternative_formulation](https://wiki.haskell.org/Typeclassopedia#Alternative_formulation)
- [https://www.reddit.com/r/haskell/comments/2lompe/where_do_the_applicative_laws_come_from/clxfblk/](https://www.reddit.com/r/haskell/comments/2lompe/where_do_the_applicative_laws_come_from/clxfblk/)