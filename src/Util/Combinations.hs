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

-- | A strict triple type.
data STr a b c = STr !a !b !c
    deriving (Eq, Ord, Show)

-- | Efficiently generates and consumes combinations of 1-based Int indexes.
-- The combinations are generated as the intermediate data structure in a
-- hylomorphism. Except in the degenerate choose-0 case, this intermediate
-- 'CombRose' structure arranges the indexes in increasing order, each level
-- of the tree corresponding to the possibilities for the next index to be
-- chosen. For choose-k combinations, there will be k trees, each
-- corresponding to a possible smallest index, which will give rise to k
-- final results.
processCombsInt
    :: (CombRoseF IntSet Int b -> b)  -- ^ Algebra for consuming the indexes
                                      -- arranged in combinations. The IntSet
                                      -- payload offers access to the indexes
                                      -- left unused by each combination.
    -> Int                            -- ^ Number of indices to choose from.
                                      -- Must be positive.
    -> Int                            -- ^ Number of indices to be chosen.
                                      -- Must be nonnegative.
    -> [b]                            -- ^ Partial results. There will be one
                                      -- result for k == 0, corresponding to
                                      -- the only possible combination, and k
                                      -- results for k > 0, corresponding to
                                      -- the possible smallest elements in
                                      -- the combinations.
processCombsInt alg n k = hylo alg combCoalg <$> seeds
    where
    as = [1..n]
    seeds = case k of
        0 -> [STr 0 (IntSet.fromList as) []]
        _ -> STr k (IntSet.fromList as) <$> tails as
    combCoalg = \case
        -- If the initial k is above 0, k won't become 0 through the unfold,
        -- because of the k == 1 special case. Therefore, k will only be 0
        -- if we have actually asked for a choose-0 combination, and so the
        -- resulting combination is valid.
        STr 0 tracker _ -> LeafF (Just tracker)
        -- Assuming we haven't asked for a choose-0 combination, the input
        -- list should never be completely emptied, given the k == 1 special
        -- case. An empty input list means there aren't enough elements to
        -- complete the combination, and so the resulting combination is
        -- invalid.
        STr _ _ [] -> LeafF Nothing
        -- If k == 1, the combination is validly terminated with the next
        -- element in the input list, and the unused elements are returned
        -- through the IntSet payload.
        STr 1 tracker (a : _) -> FlowerF a (IntSet.delete a tracker)
        STr k tracker (a : as) ->
            BranchF a (STr (k-1) (IntSet.delete a tracker) <$> tails as)

-- >$> processCombsInt (\case { LeafF Nothing -> []; LeafF _ -> []; FlowerF a _ -> [[a]]; BranchF a bs -> (a :) <$> concat bs }) 5 3

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

