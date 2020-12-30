{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
-- |
-- Module: Util.Combinations
-- --
-- Generating combinations of elements (no repetition, order doesn't matter).

module Util.Combinations where

import Data.Bifunctor
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.List (tails)

-- | From a list prefix of n elements, generates all combinations of k
-- elements, paired with the remaining n - k elements.
combinations
    :: Int  -- ^ Length of the list prefix from which the elements will be
            -- drawn.
    -> Int  -- ^ Size of the combinations to be generated.
    -> [a]
    -> [([a], [a])]
-- Asking for the list prefix length is not terribly elegant, but with short
-- input lists it shouldn't be a big deal to compute the length upfront.
combinations n k as = go n k (take n as)
    where
    go 0 _ _ = [([], [])]
    go n 0 as = [([], as)]
    go _ _ [] = [([], [])]
    go n k (a : as)
        | n < k = [([], [])]
        | n == k = [(a : as, [])]
        | otherwise = map (first (a :)) (go (n - 1) (k - 1) as)
            ++ map (second (a :)) (go (n - 1) k as)

bnom n k = product [(n-k+1)..n] `div` product [1..k]

data Rose a = Leaf | Flower a | Branch a [Rose a]
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
makeBaseFunctor ''Rose


combForest :: Int -> [a] -> [Rose a]
combForest k as = ana nextLevel <$> ((k,) <$> tails as)
    where
    nextLevel :: (Int, [a]) -> RoseF a (Int, [a])
    nextLevel = \case
        (0, _) -> LeafF
        (_, []) -> LeafF
        (1, a : _) -> FlowerF a
        (k, a : as) -> BranchF a ((k-1,) <$> tails as)

makeCombs :: Rose a -> [[a]]
makeCombs = cata $ \case
    LeafF -> []
    FlowerF a -> [[a]]
    BranchF a asz -> (a :) <$> filter (not . null) (concat asz)


-- >$> (length $ combinations 12 6 [1..12]) == bnom 12 6
--
-- $> length (concatMap makeCombs $ combForest 12 [1..20]) == bnom 20 12

