{-# LANGUAGE TypeSynonymInstances #-}
module Data.Algebra.Group where

import Control.Applicative
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Ratio

type Poly = []

infixr 4 ^+^
infixr 4 ^-^

-- | The algebraic structure of a group.  Written additively.  Required functions: 'zero' and ('^-^' or ('^+^' and 'neg')).
class Group g where
	zero :: g
	(^+^) :: g -> g -> g
	(^-^) :: g -> g -> g
	neg :: g -> g
	
	a ^+^ b = a ^-^ neg b
	a ^-^ b = a ^+^ neg b
	neg a = zero ^-^ a

instance Group Bool where
	zero = False
	(^+^) = (/=)
	(^-^) = (/=)
	neg = id

instance Group Int where
	zero = 0
	(^+^) = (+)
	(^-^) = (-)
	neg = negate

instance Group Integer where
	zero = 0
	(^+^) = (+)
	(^-^) = (-)
	neg = negate

instance Group Double where
	zero = 0
	(^+^) = (+)
	(^-^) = (-)
	neg = negate

instance Integral a => Group (Ratio a) where
	{-# SPECIALIZE instance Group Rational #-}
	zero = 0
	(^+^) = (+)
	(^-^) = (-)
	neg = negate

instance Group g => Group (a -> g) where
	zero = const zero
	(^+^) = liftA2 (^+^)
	(^-^) = liftA2 (^-^)
	neg = fmap neg

instance (Ord k, Group g) => Group (M.Map k g) where
	zero = M.empty
	(^+^) = M.unionWith (^+^)
	neg = fmap neg

instance Group g => Group (IM.IntMap g) where
	zero = IM.empty
	(^+^) = IM.unionWith (^+^)
	neg = fmap neg

instance Group g => Group (Poly g) where
	zero = []
	[] ^+^ p = p
	p ^+^ [] = p
	(a:as) ^+^ (b:bs) = (a ^+^ b):(as ^+^ bs)

instance (Group g1, Group g2) => Group (g1, g2) where
	{-# SPECIALIZE instance Group g => Group (g, g) #-}
	zero = (zero, zero)
	(x1, y1) ^+^ (x2, y2) = (x1 ^+^ x2, y1 ^+^ y2)
	(x1, y1) ^-^ (x2, y2) = (x1 ^-^ x2, y1 ^-^ y2)
	neg (x, y) = (neg x, neg y)

instance (Group g1, Group g2, Group g3) => Group (g1, g2, g3) where
	{-# SPECIALIZE instance Group g => Group (g, g, g) #-}
	zero = (zero, zero, zero)
	(x1, y1, z1) ^+^ (x2, y2, z2) = (x1 ^+^ x2, y1 ^+^ y2, z1 ^+^ z2)
	(x1, y1, z1) ^-^ (x2, y2, z2) = (x1 ^-^ x2, y1 ^-^ y2, z1 ^-^ z2)
	neg (x, y, z) = (neg x, neg y, neg z)

instance (Group g1, Group g2, Group g3, Group g4) => Group (g1, g2, g3, g4) where
	{-# SPECIALIZE instance Group g => Group (g, g, g, g) #-}
	zero = (zero, zero, zero, zero)
	(x1, y1, z1, w1) ^+^ (x2, y2, z2, w2) = (x1 ^+^ x2, y1 ^+^ y2, z1 ^+^ z2, w1 ^+^ w2)
	(x1, y1, z1, w1) ^-^ (x2, y2, z2, w2) = (x1 ^-^ x2, y1 ^-^ y2, z1 ^-^ z2, w1 ^-^ w2)
	neg (x, y, z, w) = (neg x, neg y, neg z, neg w)

{-# INLINE gsum #-}
-- | Does a summation over the elements of a group.
gsum :: Group g => [g] -> g
gsum = foldr (^+^) zero