-- | Common library for algebraic structures.  Has the advantage of automatically inferring lots of useful structure, especially
-- in the writing of linear programs.  For example, here are several ways of writing @3 x - 4 y + z@:
-- 
-- > gsum [3 *& x, (-4) *^ var y, var z]
-- > linCombination [(3, x), (-4, y), (1, z)]
-- > 3 *& x ^-^ 4 *& y ^+^ var z
-- 
-- In addition, if we have two functions @f@ and @g@, we can construct linear combinations of those functions, using 
-- exactly the same syntax.  Moreover, we can multiply functions with 'Double' coefficients by 'Rational' values successfully.
-- This module is intended to offer as much generality as possible without getting in your way.
module Data.Algebra (
	-- * Algebraic structures
	Group(..),
	Ring(..),
	Field(..),
	Module(..),
	VectorSpace(..),
	Poly,
	varPoly,
	GroupRing,
	LinFunc,
	-- * Algebraic functions
	gsum,
	combination,
	evalPoly,
	-- ** Specialized methods on linear functions
	var,
	varSum,
	(*&),
	linCombination) where

import Data.Algebra.Group
import Data.Algebra.Ring
import Data.Algebra.Field
import Data.Algebra.Module