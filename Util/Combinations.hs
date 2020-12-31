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
data CombRose z a
    = Leaf (Maybe z)  -- ^ Terminates a combination without adding a final
                      -- value. The 'Maybe' wrapping signals whether this
                      -- outcome is valid (as in choose-0 combinations) or
                      -- not. If it is valid, there is a payload.
    | Flower a z      -- ^ Terminates a combination adding a final value
                      -- and supplying a payload.
    | Branch a [CombRose z a]  -- ^ Continues a combination by branching
                               -- into different possibilities.
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
makeBaseFunctor ''CombRose

data STr a b c = STr !a !b !c
    deriving (Eq, Ord, Show)

processCombsInt :: (CombRoseF IntSet Int b -> b) -> Int -> Int -> [b]
processCombsInt alg n k = hylo alg combCoalg <$> seeds
    where
    as = [1..n]
    seeds = case k of
        0 -> [STr 0 (IntSet.fromList as) []]
        _ -> STr k (IntSet.fromList as) <$> tails as
    combCoalg = \case
        STr 0 tracker _ -> LeafF (Just tracker)
        STr _ _ [] -> LeafF Nothing
        STr 1 tracker (a : _) -> FlowerF a (IntSet.delete a tracker)
        STr k tracker (a : as) ->
            BranchF a (STr (k-1) (IntSet.delete a tracker) <$> tails as)

test1 n = length $ combinations n 12 [1..]

test4 :: Int -> Int -> [Integer]
test4 n k = processCombsInt alg n k
    where
    alg = \case
        LeafF Nothing -> 0
        LeafF (Just rest) -> IntSet.foldl' (\b k -> b * fromIntegral k) 1 rest
        FlowerF a rest -> fromIntegral a * IntSet.foldl' (\b k -> b * fromIntegral k) 1 rest
        BranchF a bs -> fromIntegral a * foldl' (+) 0 (map fromIntegral bs)

test5 :: Int -> Int -> [[Int]]
test5 n k = concat $ processCombsInt alg n k
    where
    alg = \case
        LeafF Nothing -> []
        LeafF (Just rest) -> [IntSet.toList rest]
        FlowerF a rest -> [a : IntSet.toList rest]
        BranchF a asz -> (a :) <$> filter (not . null) (concat asz)

-- >$> (length $ combinations 12 6 [1..12]) == bnom 12 6

