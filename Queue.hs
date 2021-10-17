{-
---
fulltitle: "In class exercise: Purely Functional Queues"
---

Today's technical challenge is to implement a persistent *queue* data structure.
-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Queue where

import Data.List as List
{-
You should use quickcheck to test this module. If you want to use additional
library operations to complete this exercise, you may import them here. (For
example, our solution uses at least one function from the `Data.Maybe`
library.)

-}

import Test.QuickCheck

{-
1. Define an *interface* for a purely functional Queue (FIFO).  Your interface
  must (at least) define some data structure, called `Q`, include a
  representation of an empty queue (queue), a way to add an element to the end
  of the queue (enq) and a way to remove the element at the beginning of the
  queue (deq), if the queue is nonempty. The queue must be polymorphic over
  the type of elements that it stores. You may include additional operations
  in your interface, if you wish.
-}

-- replace these definitions with something more appropriate
data Q a = Q [a] [a] deriving (Eq, Show)

empty :: Q a
enq :: Q a -> a -> Q a
deq :: Q a -> (a, Q a)
{-
2. Now define some properties that your queue should satisfy. (Note: if you want
to add additional operations to your queue interface to help with stating
these properties, you may.)
-}

{-
3. Implement your interface.
-}

empty = Q [] []

enq (Q enQ deQ) x =
  Q (x : enQ) deQ

deq (Q [] []) = error "Empty Queue"
deq (Q enQ []) =
  let (h : hs) = List.reverse enQ
   in (h, Q [] hs)
deq (Q x (y : end)) = (y, Q x end)

{-
4. Make an arbitrary instance.
-}
instance Arbitrary a => Arbitrary (Q a) where
  arbitrary :: Gen (Q a)
  arbitrary = Q <$> arbitrary <*> arbitrary

  shrink :: Q a -> [Q a]
  shrink (Q l1 l2) = Q <$> shrink l1 <*> shrink l2

{-
5. Run your tests.
-}
-- Validity
-- Preserves order. If we implement fromList and toList.

-- Post condition
-- Metamorphic
-- Model based