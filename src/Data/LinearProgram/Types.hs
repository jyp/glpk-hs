{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module Data.LinearProgram.Types (LinFunc, VarKind(..), Direction(..), Bounds(..)) where

import Control.DeepSeq
import Data.Monoid
import GHC.Generics
import Data.Map

type LinFunc = Map


data VarKind = ContVar | IntVar | BinVar deriving (Eq, Ord, Enum, Show, Read, Generic)

-- instance NFData VarKind

instance Semigroup VarKind where
        (<>) = max

instance Monoid VarKind where
        mempty = ContVar
        mappend = (<>)

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
        mappend = (<>)

instance Ord a => Semigroup (Bounds a) where
        Free <> bd = bd
        bd <> Free = bd
        Equ a <> Equ b
                | a == b        = Equ a
        Equ a <> UBound b
                | a <= b        = Equ a
        Equ a <> LBound b
                | a >= b        = Equ a
        Equ a <> Bound l u
                | a >= l && a <= u
                                = Equ a
        Equ _ <> _ = infeasible
        UBound b <> Equ a
                | a <= b        = Equ a
        LBound b <> Equ a
                | a >= b        = Equ a
        Bound l u <> Equ a
                | a >= l && a <= u
                                = Equ a
        _ <> Equ _ = infeasible
        LBound a <> LBound b = LBound (max a b)
        LBound l <> UBound u = bound l u
        UBound u <> LBound l = bound l u
        LBound a <> Bound l u = bound (max a l) u
        Bound l u <> LBound a = bound (max a l) u
        UBound a <> UBound b = UBound (min a b)
        UBound a <> Bound l u = bound l (min a u)
        Bound l u <> UBound a = bound l (min a u)
        Bound l u <> Bound l' u' = bound (max l l') (min u u')

infeasible :: Bounds a
infeasible = error "Mutually contradictory constraints found."

bound :: Ord a => a -> a -> Bounds a
bound l u       | l <= u        = Bound l u
                | otherwise     = infeasible
