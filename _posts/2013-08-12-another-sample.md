---
layout: post
title: Another sample
categories:
- blog
---

Tattooed roof party *vinyl* freegan single-origin coffee wayfarers tousled, umami yr 
meggings hella selvage. Butcher bespoke seitan, cornhole umami gentrify put a bird 
on it occupy trust fund. Umami whatever kitsch, locavore fingerstache Tumblr pork belly
[keffiyeh](#). Chia Echo Park Pitchfork, Blue Bottle [hashtag](#) stumptown skateboard selvage 
mixtape. Echo Park retro butcher banjo cardigan, seitan flannel Brooklyn paleo fixie 
Truffaut. Forage mustache Thundercats next level disrupt. Bicycle rights forage tattooed
chia, **wayfarers** swag raw denim hashtag biodiesel occupy gastropub!

---

# It's all in the game.

## You come at the king, you best not miss.

### Be subtle with it, man. You know what subtle means?

VHS post-ironic cred **bespoke** banjo. Yr wayfarers literally gentrify, flexitarian fap 
dreamcatcher plaid cornhole Intelligentsia paleo. Beard try-hard direct trade, shabby chic 
Helvetica `look ma, I can code`. Lo-fi American Apparel tattooed [Vice](#) tofu, yr vinyl. 
Williamsburg butcher hella mumblecore fixie mlkshk, cliche wolf keytar mixtape kitsch banh mi 
salvia. High Life Odd Future *chambray* kale chips hoodie, cray pop-up. Helvetica narwhal 
iPhone try-hard jean shorts.

> This is a quote from someone famous about productivity


Syntax highlighting with Solarized theme.

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

import Test.QuickCheck
import Data.DeriveTH
import Data.Binary

class Functor f => Applesauce f where
  unit :: f ()
  merge :: f a -> f b -> f (a, b)

class Applesauce m => Monkfish m where
  yoin :: m (m a) -> m a

ali fa = fmap snd (unit `merge` fa) == fa
ari fa = fmap fst (fa `merge` unit) == fa

--yoin . fmap yoin = yoin . yoin
--yoin . fmap return = yoin . return = id
--yoin . fmap (fmap f) = fmap f . yoin

newtype ZipList a = ZipList [a] deriving (Show, Functor, Eq)

"hi" 's' 23 2.3 Just 3

$(derive makeArbitrary ''ZipList)

instance Applesauce ZipList where
  unit = ZipList []
  merge (ZipList xs) (ZipList ys) = ZipList $ zip xs ys

instance Monkfish ZipList where
  yoin (ZipList xs) = ZipList $ concatMap (\(ZipList ys) -> ys) xs
```
