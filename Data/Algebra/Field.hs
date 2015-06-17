{-# LANGUAGE UndecidableInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Data.Algebra.Field where

import Data.Ratio

import Data.Algebra.Ring
import Data.Algebra.Module

class Ring f => Field f where
	inv :: f -> f
	(/#) :: f -> f -> f
	inv x = one /# x
	a /# b = a *# inv b

instance Field Double where
	inv = recip

instance Integral a => Field (Ratio a) where
	{-# SPECIALIZE instance Field Rational #-}
	inv = recip

class (Module f v, Field f) => VectorSpace f v
instance (Module f v, Field f) => VectorSpace f v