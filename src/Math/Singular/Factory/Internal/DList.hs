
-- | Difference lists, minimalistic edition

module Math.Singular.Factory.Internal.DList where

--------------------------------------------------------------------------------

import Data.List ( foldr )

--------------------------------------------------------------------------------

type DList a = [a] -> [a]

empty :: DList a
empty = id

singleton :: a -> DList a
singleton = (:)

toList :: DList a -> [a]
toList dl = dl []

append :: DList a -> DList a -> DList a
append = (.)

concat :: [DList a] -> DList a
concat = foldr append empty

--------------------------------------------------------------------------------
