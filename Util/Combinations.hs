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
import Data.List (tails, foldl')
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

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

-- | A rose tree, adapted for generating combinations
data CombRose z a = Leaf | Flower a z | Branch a [CombRose z a]
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
makeBaseFunctor ''CombRose


combForest :: Int -> [a] -> [CombRose () a]
combForest k as = ana nextLevel <$> ((k,) <$> tails as)
    where
    nextLevel :: (Int, [a]) -> CombRoseF () a (Int, [a])
    nextLevel = \case
        (0, _) -> LeafF
        (_, []) -> LeafF
        (1, a : _) -> FlowerF a ()
        (k, a : as) -> BranchF a ((k-1,) <$> tails as)

makeCombs :: CombRose () a -> [[a]]
makeCombs = cata $ \case
    LeafF -> []
    FlowerF a _ -> [[a]]
    BranchF a asz -> (a :) <$> filter (not . null) (concat asz)

data SP a b = SP !a !b
    deriving (Eq, Ord, Show)

data STr a b c = STr !a !b !c
    deriving (Eq, Ord, Show)

hyloCombs :: Int -> [a] -> [[a]]
hyloCombs k as = concat (hylo flattenAlg combCoalg <$> seeds)
    where
    seeds = SP k <$> tails as
    combCoalg = \case
        SP 0 _ -> LeafF
        SP _ [] -> LeafF
        SP 1 (a : _) -> FlowerF a ()
        SP k (a : as) -> BranchF a (SP (k-1) <$> tails as)
    flattenAlg = \case
        LeafF -> []
        FlowerF a _ -> [[a]]
        BranchF a asz -> (a :) <$> filter (not . null) (concat asz)

hyloCombsInt :: Int -> Int -> [[Int]]
hyloCombsInt n k = concat (hylo flattenAlg combCoalg <$> seeds)
    where
    as = [1..n]
    seeds = SP k <$> tails as
    combCoalg = \case
        SP 0 _ -> LeafF
        SP _ [] -> LeafF
        SP 1 (a : _) -> FlowerF a ()
        SP k (a : as) -> BranchF a (SP (k-1) <$> tails as)
    flattenAlg = \case
        LeafF -> []
        FlowerF a _ -> [[a]]
        BranchF a asz -> (a :) <$> filter (not . null) (concat asz)

processCombsInt :: (CombRoseF IntSet Int b -> b) -> Int -> Int -> [b]
processCombsInt alg n k = hylo alg combCoalg <$> seeds
    where
    as = [1..n]
    seeds = STr k (IntSet.fromList as) <$> tails as
    combCoalg = \case
        STr 0 _ _ -> LeafF
        STr _ _ [] -> LeafF
        STr 1 tracker (a : _) -> FlowerF a (IntSet.delete a tracker)
        STr k tracker (a : as) ->
            BranchF a (STr (k-1) (IntSet.delete a tracker) <$> tails as)

test1 n = length $ combinations n 12 [1..]
test2 n = length $ hyloCombs 12 [1..n]
test3 n = length $ hyloCombsInt n 12

test4 :: Int -> Int -> [Integer]
test4 n k = processCombsInt alg n k
    where
    alg = \case
        LeafF -> 0
        FlowerF a rest -> fromIntegral a * IntSet.foldl' (\b k -> b * fromIntegral k) 1 rest
        BranchF a bs -> fromIntegral a * foldl' (+) 0 (map fromIntegral bs)

test5 :: Int -> Int -> [[Int]]
test5 n k = concat $ processCombsInt alg n k
    where
    alg = \case
        LeafF -> []
        FlowerF a rest -> [a : IntSet.toList rest]
        BranchF a asz -> (a :) <$> filter (not . null) (concat asz)

-- >$> (length $ combinations 12 6 [1..12]) == bnom 12 6
--
-- >$> length (hyloCombs 12 [1..20]) == bnom 20 12

