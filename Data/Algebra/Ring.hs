{-# LANGUAGE TypeSynonymInstances #-}
module Data.Algebra.Ring where

import Control.Applicative

import Data.Ratio
import qualified Data.Map as M

import Data.Algebra.Group

infixr 6 *#

-- | A way of forming a ring from functions.  See <http://en.wikipedia.org/wiki/Group_ring>.
type GroupRing r g = M.Map g r

-- | The algebraic structure of a unital ring.  Assumes that the additive operation forms an abelian group,
-- that the multiplication operation forms a group, and that multiplication distributes.
class Group r => Ring r where
	one :: r
	(*#) :: r -> r -> r

instance Ring Bool where
	one = True
	(*#) = (&&)

instance Ring Int where
	one = 1
	(*#) = (*)

instance Ring Integer where
	one = 1
	(*#) = (*)

instance Ring Double where
	one = 1
	(*#) = (*)

instance Integral a => Ring (Ratio a) where
	{-# SPECIALIZE instance Ring Rational #-}
	one = 1
	(*#) = (*)

-- | The polynomial ring.
instance Ring r => Ring (Poly r) where
	one = [one]
	(p:ps) *# (q:qs) = (p *# q):(ps *# (q:qs) ^+^ map (p *#) qs)
	_ *# _ = []

-- | The function ring.
instance Ring r => Ring (a -> r) where
	one = const one
	(*#) = liftA2 (*#)

-- | The group ring.
instance (Ord g, Group g, Ring r) => Ring (GroupRing r g) where
	one = M.singleton zero one
	m *# n = M.fromListWith (^+^) [(u ^+^ v, f *# g) | (u, f) <- M.assocs m, (v, g) <- M.assocs n]

-- | Returns the polynomial @p(x) = x@.
varPoly :: Ring r => Poly r
varPoly = [zero, one]
