{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

module Control.Monad.LPMonad.Supply (module Control.Monad.LPMonad.Supply.Class, Var(..), VSupply, VSupplyT, runVSupply, runVSupplyT) where

import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.State.Strict
import Control.Monad.RWS.Class
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Applicative
import Control.Monad.LPMonad.Supply.Class

-- | A type suitable for use as a linear program variable.
newtype Var = Var {varId :: Int} deriving (Eq, Ord, Enum)

-- | A monad capable of supplying unique variables.
type VSupply = VSupplyT Identity

runVSupply :: VSupply a -> a
runVSupply = runIdentity . runVSupplyT

-- | A monad transformer capable of supplying unique variables.
newtype VSupplyT m a = VSupplyT (StateT Var m a) deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadTrans, MonadReader r, MonadWriter w, MonadCont,
        MonadIO, MonadFix, MonadError e)

runVSupplyT :: Monad m => VSupplyT m a -> m a
runVSupplyT (VSupplyT m) = evalStateT m (Var 0)

instance Show Var where
        show (Var x) = "x_" ++ show x

instance Read Var where
        readsPrec _ ('x':'_':xs) = [(Var x, s') | (x, s') <- reads xs]
        readsPrec _ _ = []

instance MonadState s m => MonadState s (VSupplyT m) where
        get = lift get
        put = lift . put

instance Monad m => MonadSupply Var (VSupplyT m) where
        {-# SPECIALIZE instance MonadSupply Var VSupply #-}
        supplyNew = VSupplyT $ StateT $ \ v -> return (v, succ v)
        supplyN n = VSupplyT $ StateT $ \ (Var x) -> return (map Var [x..x+n-1], Var (x + n))
