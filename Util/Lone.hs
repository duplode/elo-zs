{-# LANGUAGE ConstraintKinds #-}
-- |
-- Module: Util.Lone
--
-- There should be a standard class for what one might call lone value
-- functors, that is, a strenghtening of 'Traversable' for 'Functors'
-- isomorphic to tuples. Since such a class does not exist yet, and this
-- package is clearly not the ideal place to define it, the next best thing
-- to do is simulating it through 'Adjunction'. Certainly it would not be
-- difficult to forgo the generalised combinators and stick with concrete
-- signatures. That, however, would require compromising on the interface
-- and ergonomics of 'foldTransversally' in the analysis code here, and
-- for our current purposes I don't see a need for compromising.
module Util.Lone
    ( Lone
    , codistributeL
    , surroundL
    ) where

import Data.Functor.Adjunction
import Control.Comonad

-- | The hypothetical @Lone@ class would look like this:
--
-- > class Traversable t => Lone t where
-- >
-- >    codistribute :: Functor f => t (f a) -> f (t a)
-- >    codistribute = surround id
-- >
-- >    surround :: Functor f => (a -> f b) -> f a -> f (t b)
-- >    surround f = codistribute . fmap f
--
-- (I am loath to introduce yet another variation on the @sequence@,
-- @traverse@ and @mapM@ names, specially given that this particular
-- @sequence@ doesn't actually sequence anything.)
--
-- In the absence of the class, we will make do with a constraint synonym.
-- If nothing else, we can at least add an 'Adjunction' constraint to give
-- this contraption some legitimacy and generality. Lone value functors are
-- left adjoints, and in fact the definitions here could just as easily be
-- brought to 'Data.Functor.Adjunction'.
--
-- @Lone@ functors are all 'Comonad's. If we were creating the actual @Lone@
-- class, we might choose not to add it as a superclass, to its free users
-- from providing @Comonad@ instances. In our concrete use case, though, we
-- will also use the @Comonad@ interface, so we might as well leave the
-- constraint in.
type Lone t u = (Adjunction t u, Traversable t, Comonad t)

-- ^ 'sequenceA' for 'Lone'. Requires just 'Functor' from the other functor.
--
-- Note that this implementation doesn't actually use 'Adjunction'. However,
-- for 'Comonads' that aren't left adjoints it would in general break the
-- traversable laws. It is possible to wring out an implementation in terms
-- of 'splitL' and 'unsplitL' from 'Data.Functor.Adjunction', if one must.
--
codistributeL :: (Lone t u, Functor f) => t (f a) -> f (t a)
codistributeL t = flip fmap t . const <$> extract t

-- ^ 'traverse' for 'Lone'. Requires just 'Functor' from the other functor.
surroundL :: (Lone t u, Functor f) => (a -> f b) -> t a -> f (t b)
surroundL f t = flip fmap t . const <$> f (extract t)
