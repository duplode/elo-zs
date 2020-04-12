-- |
-- Module: Tabular
--
-- Utility functions for generating output using
-- [the @tabular@ package](https://hackage.haskell.org/package/tabular)
module Tabular
    ( arrangeTable
    , demoPretty
    ) where

import qualified Text.Tabular as Tab
import qualified Text.Tabular.AsciiArt as Tab

-- | Arrange a ranking or listing as a 'Table'.
arrangeTable
    :: ([a] -> [String])  -- ^ How to make row headers from the source list.
    -> [String]           -- ^ Column headers.
    -> (a -> [String])    -- ^ How to display values.
    -> [a]                -- ^ Source list.
    -> Tab.Table String String String
arrangeTable mkRhs chs mkVals as = Tab.Table
    (Tab.Group Tab.SingleLine (Tab.Header <$> mkRhs as))
    (Tab.Group Tab.SingleLine (Tab.Header <$> chs))
    (mkVals <$> as)

-- | Prints a 'Table' to the console.
demoPretty :: Tab.Table String String String -> IO ()
demoPretty t = putStrLn (Tab.render id id id t)

