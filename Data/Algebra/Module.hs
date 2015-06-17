{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, IncoherentInstances, TypeSynonymInstances #-}

module Data.Algebra.Module where

import Data.Ratio
import qualified Data.Map as M
import qualified Data.IntMap as IM

import Data.Algebra.Group
import Data.Algebra.Ring

-- | The algebraic structure of a module.  A vector space is a module with coefficients in a field.
class (Ring r, Group m) => Module r m where
	(*^) :: r -> m -> m

instance Module Int Int where
	(*^) = (*)

instance Module Integer Integer where
	(*^) = (*)

instance Module Int Integer where
	(*^) = (*) . fromIntegral

instance Integral a => Module Int (Ratio a) where
	{-# SPECIALIZE instance Module Int Rational #-}
	(*^) = (*) . fromIntegral

instance Integral a => Module Integer (Ratio a) where
	{-# SPECIALIZE instance Module Integer Rational #-}
	(*^) = (*) . fromIntegral

instance Integral a => Module (Ratio a) (Ratio a) where
	{-# SPECIALIZE instance Module Rational Rational #-}
	(*^) = (*)

instance Module Int Double where
	(*^) = (*) . fromIntegral

instance Module Integer Double where
	(*^) = (*) . fromIntegral

instance Integral a => Module (Ratio a) Double where
	{-# SPECIALIZE instance Module Rational Double #-}
	(*^) = (*) . realToFrac

instance Module Double Double where
	(*^) = (*)

instance (Ord g, Group g, Ring r) => Module (GroupRing r g) (GroupRing r g) where
	(*^) = (*#)

instance Module r m => Module r (a -> m) where
	(*^) = fmap . (*^)

instance (Ord k, Module r m) => Module r (M.Map k m) where
	(*^) = fmap . (*^)

instance Module r m => Module r (IM.IntMap m) where
	(*^) = fmap . (*^)

instance (Module r m1, Module r m2) => Module r (m1, m2) where
	{-# SPECIALIZE instance Module r m => Module r (m, m) #-}
	r *^ (a, b) = (r *^ a, r *^ b)

instance (Module r m1, Module r m2, Module r m3) => Module r (m1, m2, m3) where
	{-# SPECIALIZE instance Module r m => Module r (m, m, m) #-}
	r *^ (a, b, c) = (r *^ a, r *^ b, r *^ c)

instance (Module r m1, Module r m2, Module r m3, Module r m4) => Module r (m1, m2, m3, m4) where
	{-# SPECIALIZE instance Module r m => Module r (m, m, m, m) #-}
	r *^ (a, b, c, d) = (r *^ a, r *^ b, r *^ c, r *^ d)

-- | @'LinFunc' v c@ is a linear combination of variables of type @v@ with coefficients
-- from @c@.  Formally, this is the free @c@-module on @v@.  
type LinFunc = M.Map

-- | Given a variable @v@, returns the function equivalent to @v@.
var :: (Ord v, Ring c) => v -> LinFunc v c
var v = M.singleton v one

-- | @c '*&' v@ is equivalent to @c '*^' 'var' v@.
(*&) :: (Ord v, Ring c) => c -> v -> LinFunc v c
c *& v = M.singleton v c

-- | Equivalent to @'vsum' . 'map' 'var'@.
varSum :: (Ord v, Ring c) => [v] -> LinFunc v c
varSum vs = M.fromList [(v, one) | v <- vs]

-- | Given a collection of vectors and scaling coefficients, returns this
-- linear combination.
combination :: Module r m => [(r, m)] -> m
combination xs = gsum [r *^ m | (r, m) <- xs]

{-# INLINE linCombination #-}
-- | Given a set of basic variables and coefficients, returns the linear combination obtained
-- by summing.
linCombination :: (Ord v, Num r) => [(r, v)] -> LinFunc v r
linCombination xs = M.fromListWith (+) [(v, r) | (r, v) <- xs]

-- | Substitution into a polynomial.
evalPoly :: (Module r m, Ring m) => Poly r -> m -> m
evalPoly f x = foldr (\ c z -> (c *^ one) ^+^ (x *# z)) zero f

{-# RULES
	"zero/*^" forall m . zero *^ m = zero;
	"*^/zero" forall r . r *^ zero = zero;
	"one/*^" forall m . one *^ m = m;
	#-}