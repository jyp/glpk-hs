{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module Data.LinearProgram.Types (VarKind(..), Direction(..), Bounds(..)) where

import Control.DeepSeq
import Data.Monoid
import GHC.Generics

data VarKind = ContVar | IntVar | BinVar deriving (Eq, Ord, Enum, Show, Read, Generic)

-- instance NFData VarKind

instance Monoid VarKind where
        mempty = ContVar
        mappend = max

data Direction = Min | Max deriving (Eq, Ord, Enum, Show, Read, Generic)

-- instance NFData Direction

data Bounds a =
        Free | LBound !a | UBound !a | Equ !a | Bound !a !a deriving (Eq, Show, Read, Functor)

instance NFData VarKind
instance NFData Direction
instance NFData c => NFData (Bounds c) where
        rnf Free = ()
        rnf (Equ c) = rnf c
        rnf (LBound c) = rnf c
        rnf (UBound c) = rnf c
        rnf (Bound l u) = l `deepseq` rnf u

-- instance NFData (Bounds a)

-- Bounds form a monoid under intersection.
instance Ord a => Monoid (Bounds a) where
        mempty = Free
        Free `mappend` bd = bd
        bd `mappend` Free = bd
        Equ a `mappend` Equ b
                | a == b        = Equ a
        Equ a `mappend` UBound b
                | a <= b        = Equ a
        Equ a `mappend` LBound b
                | a >= b        = Equ a
        Equ a `mappend` Bound l u
                | a >= l && a <= u
                                = Equ a
        Equ _ `mappend` _ = infeasible
        UBound b `mappend` Equ a
                | a <= b        = Equ a
        LBound b `mappend` Equ a
                | a >= b        = Equ a
        Bound l u `mappend` Equ a
                | a >= l && a <= u
                                = Equ a
        _ `mappend` Equ _ = infeasible
        LBound a `mappend` LBound b = LBound (max a b)
        LBound l `mappend` UBound u = bound l u
        UBound u `mappend` LBound l = bound l u
        LBound a `mappend` Bound l u = bound (max a l) u
        Bound l u `mappend` LBound a = bound (max a l) u
        UBound a `mappend` UBound b = UBound (min a b)
        UBound a `mappend` Bound l u = bound l (min a u)
        Bound l u `mappend` UBound a = bound l (min a u)
        Bound l u `mappend` Bound l' u' = bound (max l l') (min u u')

infeasible :: Bounds a
infeasible = error "Mutually contradictory constraints found."

bound :: Ord a => a -> a -> Bounds a
bound l u       | l <= u        = Bound l u
                | otherwise     = infeasible
