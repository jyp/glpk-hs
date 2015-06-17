{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Data.LinearProgram.LinExpr (LinExpr(..), solve, substituteExpr, simplifyExpr,
	constTerm, coeffTerm, funcToExpr) where

import Control.Monad

import Data.LinearProgram.Types
import Data.Algebra
import Data.Functor
import Data.Foldable

import Data.Map

import Prelude hiding (lookup, filter, foldr)

constTerm :: LinExpr v c -> c
constTerm (LinExpr _ c) = c

coeffTerm :: LinExpr v c -> LinFunc v c
coeffTerm (LinExpr a _) = a

funcToExpr :: Group c => LinFunc v c -> LinExpr v c
funcToExpr = flip LinExpr zero

data LinExpr v c = LinExpr (LinFunc v c) c deriving (Eq, Read, Show)

instance (Ord v, Group c) => Group (LinExpr v c) where
	zero = LinExpr zero zero
	LinExpr a1 c1 ^+^ LinExpr a2 c2 = LinExpr (a1 ^+^ a2) (c1 ^+^ c2)
	LinExpr a1 c1 ^-^ LinExpr a2 c2 = LinExpr (a1 ^-^ a2) (c1 ^-^ c2)
	neg (LinExpr a c) = LinExpr (neg a) (neg c)

instance (Ord v, Module r c) => Module r (LinExpr v c) where
	k *^ LinExpr a c = LinExpr (k *^ a) (k *^ c)

substituteExpr :: (Ord v, Module c c) => v -> LinExpr v c -> LinExpr v c -> LinExpr v c
substituteExpr v expV expr@(LinExpr a c) = case lookup v a of
	Nothing	-> expr
	Just k	-> LinExpr (delete v a) c ^+^ (k *^ expV)

simplifyExpr :: (Ord v, Module c c) => LinExpr v c -> Map v (LinExpr v c) -> LinExpr v c
simplifyExpr (LinExpr a c) sol =
	foldrWithKey (const (^+^)) (LinExpr (difference a sol) c) (intersectionWith (*^) a sol)

solve :: (Ord v, Eq c, VectorSpace c c) => [(LinFunc v c, c)] -> Maybe (Map v (LinExpr v c))
solve equs = solve' [LinExpr a (neg c) | (a, c) <- equs]

solve' :: (Ord v, Eq c, VectorSpace c c) => [LinExpr v c] -> Maybe (Map v (LinExpr v c))
solve' (LinExpr a c:equs) = case minViewWithKey (filter (/= zero) a) of
	Nothing	-> guard (c == zero) >> solve' equs
	Just ((x, a0), a') -> let expX = neg (inv a0 *^ LinExpr a' c) in
		liftM (simplifyExpr expX >>= insert x) (solve' (substituteExpr x expX <$> equs))
solve' [] = return empty

{-# RULES
	"mapWithKey/mapWithKey" forall f g m .
		mapWithKey f (mapWithKey g m) = mapWithKey (liftM2 (.) f g) m
	#-}