{-# LANGUAGE BangPatterns, FlexibleContexts, RecordWildCards #-}

module Control.Monad.LPMonad.Internal (
--         module Data.LinearProgram.Common,
        -- * Monad definitions
        LPM,
        LPT,
        runLPM,
        runLPT,
        execLPM,
        execLPT,
        evalLPM,
        evalLPT,
        -- * Constructing the LP
        -- ** Objective configuration
        setDirection,
        setObjective,
        addObjective,
        addWeightedObjective,
        -- ** Two-function constraints
        leq,
        equal,
        geq,
        leq',
        equal',
        geq',
        -- ** One-function constraints
        leqTo,
        equalTo,
        geqTo,
        constrain,
        leqTo',
        equalTo',
        geqTo',
        constrain',
        -- ** Variable constraints
        varLeq,
        varEq,
        varGeq,
        varBds,
        setVarBounds,
        setVarKind,
--         newVariables,
--         newVariables'
        ) where

import Prelude hiding ((-),(+))
import Control.Monad.State.Strict
import Control.Monad.Identity

import Data.Map

import Data.LinearProgram.Common

-- | A simple monad for constructing linear programs.  This library is intended to be able to link to
-- a variety of different linear programming implementations.
type LPM v c = LPT v c Identity

-- | A simple monad transformer for constructing linear programs in an arbitrary monad.
type LPT v c = StateT (LP v c)

runLPM :: (Ord v, Group c) => LPM v c a -> (a, LP v c)
runLPM = runIdentity . runLPT

runLPT :: (Ord v, Group c) => LPT v c m a -> m (a, LP v c)
runLPT m = runStateT m (LP Max zero [] mempty mempty)

-- | Constructs a linear programming problem.
execLPM :: (Ord v, Group c) => LPM v c a -> LP v c
execLPM = runIdentity . execLPT

-- | Constructs a linear programming problem in the specified monad.
execLPT :: (Ord v, Group c, Monad m) => LPT v c m a -> m (LP v c)
execLPT = liftM snd . runLPT

-- | Runs the specified operation in the linear programming monad.
evalLPM :: (Ord v, Group c) => LPM v c a -> a
evalLPM = runIdentity . evalLPT

-- | Runs the specified operation in the linear programming monad transformer.
evalLPT :: (Ord v, Group c, Monad m) => LPT v c m a -> m a
evalLPT = liftM fst . runLPT

-- | Sets the optimization direction of the linear program: maximization or minimization.
{-# SPECIALIZE setDirection :: Direction -> LPM v c (), Monad m => Direction -> LPT v c m () #-}
setDirection :: (MonadState (LP v c) m) => Direction -> m ()
setDirection dir = modify (\ lp -> lp{direction = dir})

{-# SPECIALIZE equal :: (Ord v, Group c) => LinFunc v c -> LinFunc v c -> LPM v c (),
        (Ord v, Group c, Monad m) => LinFunc v c -> LinFunc v c -> LPT v c m () #-}
{-# SPECIALIZE leq :: (Ord v, Group c) => LinFunc v c -> LinFunc v c -> LPM v c (),
        (Ord v, Group c, Monad m) => LinFunc v c -> LinFunc v c -> LPT v c m () #-}
{-# SPECIALIZE geq :: (Ord v, Group c) => LinFunc v c -> LinFunc v c -> LPM v c (),
        (Ord v, Group c, Monad m) => LinFunc v c -> LinFunc v c -> LPT v c m () #-}
-- | Specifies the relationship between two functions in the variables.  So, for example,
--
-- > equal (f ^+^ g) h
--
-- constrains the value of @h@ to be equal to the value of @f@ plus the value of @g@.
equal, leq, geq :: (Ord v, Group c, MonadState (LP v c) m) => LinFunc v c -> LinFunc v c -> m ()
equal f g = equalTo (f - g) zero
leq f g = leqTo (f - g) zero
geq = flip leq

{-# SPECIALIZE equal' :: (Ord v, Group c) => String -> LinFunc v c -> LinFunc v c -> LPM v c (),
        (Ord v, Group c, Monad m) => String -> LinFunc v c -> LinFunc v c -> LPT v c m () #-}
{-# SPECIALIZE geq' :: (Ord v, Group c) => String -> LinFunc v c -> LinFunc v c -> LPM v c (),
        (Ord v, Group c, Monad m) => String -> LinFunc v c -> LinFunc v c -> LPT v c m () #-}
{-# SPECIALIZE leq' :: (Ord v, Group c) => String -> LinFunc v c -> LinFunc v c -> LPM v c (),
        (Ord v, Group c, Monad m) => String -> LinFunc v c -> LinFunc v c -> LPT v c m () #-}
-- | Specifies the relationship between two functions in the variables, with a label on the constraint.
equal', leq', geq' :: (Ord v, Group c, MonadState (LP v c) m) => String -> LinFunc v c -> LinFunc v c -> m ()
equal' lab f g = equalTo' lab (f - g) zero
leq' lab f g = leqTo' lab (f - g) zero
geq' = flip . leq'

{-# SPECIALIZE equalTo :: LinFunc v c -> c -> LPM v c (), Monad m => LinFunc v c -> c -> LPT v c m () #-}
{-# SPECIALIZE geqTo :: LinFunc v c -> c -> LPM v c (), Monad m => LinFunc v c -> c -> LPT v c m () #-}
{-# SPECIALIZE leqTo :: LinFunc v c -> c -> LPM v c (), Monad m => LinFunc v c -> c -> LPT v c m () #-}
-- | Sets a constraint on a linear function in the variables.
equalTo, leqTo, geqTo :: MonadState (LP v c) m => LinFunc v c -> c -> m ()
equalTo f v = constrain f (Equ v)
leqTo f v = constrain f (UBound v)
geqTo f v = constrain f (LBound v)

{-# SPECIALIZE equalTo' :: String -> LinFunc v c -> c -> LPM v c (),
        Monad m => String -> LinFunc v c -> c -> LPT v c m () #-}
{-# SPECIALIZE geqTo' :: String -> LinFunc v c -> c -> LPM v c (),
        Monad m => String -> LinFunc v c -> c -> LPT v c m () #-}
{-# SPECIALIZE leqTo' :: String -> LinFunc v c -> c -> LPM v c (),
        Monad m => String -> LinFunc v c -> c -> LPT v c m () #-}
-- | Sets a labeled constraint on a linear function in the variables.
equalTo', leqTo', geqTo' :: MonadState (LP v c) m => String -> LinFunc v c -> c -> m ()
equalTo' lab f v = constrain' lab f (Equ v)
leqTo' lab f v = constrain' lab f (UBound v)
geqTo' lab f v = constrain' lab f (LBound v)

-- {-# SPECIALIZE newVariables :: (Ord v, Enum v) => Int -> LPM v c [v],
--         (Ord v, Enum v, Monad m) => Int -> LPT v c m [v] #-}
-- -- | Returns a list of @k@ unused variables.  If the program is currently empty,
-- -- starts at @'toEnum' 0@.  Otherwise, if @v@ is the biggest variable currently in use
-- -- (by the 'Ord' ordering), then this returns @take k (tail [v..])@, which uses the 'Enum'
-- -- implementation.  Note that if the 'Enum' instance doesn't play well with 'Ord',
-- -- bad things can happen.
-- newVariables :: (MonadState (LP v c) m, Ord v, Enum v) => Int -> m [v]
-- newVariables !k = do        LP{..} <- get
--                         let allVars0 = () <$ objective `union`
--                                 unions [() <$ f | Constr _ f _ <- constraints] `union`
--                                 (() <$ varBounds) `union` (() <$ varTypes)
--                         case minViewWithKey allVars0 of
--                                 Nothing        -> return $ take k [toEnum 0..]
--                                 Just ((start, _), _)
--                                         -> return $ take k $ tail [start..]
--
-- {-# SPECIALIZE newVariables' :: (Ord v, Enum v) => LPM v c [v],
--         (Ord v, Enum v, Monad m) => LPT v c m [v] #-}
-- -- | Returns an infinite list of unused variables.  If the program is currently empty,
-- -- starts at @'toEnum' 0@.  Otherwise, if @v@ is the biggest variable currently in use
-- -- (by the 'Ord' ordering), then this returns @tail [v..]@, which uses the 'Enum'
-- -- implementation.  Note that if the 'Enum' instance doesn't play well with 'Ord',
-- -- bad things can happen.
-- newVariables' :: (MonadState (LP v c) m, Ord v, Enum v) => m [v]
-- newVariables' = do        LP{..} <- get
--                         let allVars0 = () <$ objective `union`
--                                 unions [() <$ f | Constr _ f _ <- constraints] `union`
--                                 (() <$ varBounds) `union` (() <$ varTypes)
--                         case minViewWithKey allVars0 of
--                                 Nothing        -> return [toEnum 0..]
--                                 Just ((start, _), _)
--                                         -> return $ tail [start..]

{-# SPECIALIZE varEq :: (Ord v, Ord c) => v -> c -> LPM v c (),
        (Ord v, Ord c, Monad m) => v -> c -> LPT v c m () #-}
{-# SPECIALIZE varLeq :: (Ord v, Ord c) => v -> c -> LPM v c (),
        (Ord v, Ord c, Monad m) => v -> c -> LPT v c m () #-}
{-# SPECIALIZE varGeq :: (Ord v, Ord c) => v -> c -> LPM v c (),
        (Ord v, Ord c, Monad m) => v -> c -> LPT v c m () #-}
-- | Sets a constraint on the value of a variable.  If you constrain a variable more than once,
-- the constraints will be combined.  If the constraints are mutually contradictory,
-- an error will be generated.  This is more efficient than adding an equivalent function constraint.
varEq, varLeq, varGeq :: (Ord v, Ord c, MonadState (LP v c) m) => v -> c -> m ()
varEq v c = setVarBounds v (Equ c)
varLeq v c = setVarBounds v (UBound c)
varGeq v c = setVarBounds v (LBound c)

{-# SPECIALIZE varBds :: (Ord v, Ord c) => v -> c -> c -> LPM v c (),
        (Ord v, Ord c, Monad m) => v -> c -> c -> LPT v c m () #-}
-- | Bounds the value of a variable on both sides.  If you constrain a variable more than once,
-- the constraints will be combined.  If the constraints are mutually contradictory,
-- an error will be generated.  This is more efficient than adding an equivalent function constraint.
varBds :: (Ord v, Ord c, MonadState (LP v c) m) => v -> c -> c -> m ()
varBds v l u = setVarBounds v (Bound l u)

{-# SPECIALIZE constrain :: LinFunc v c -> Bounds c -> LPM v c (),
        Monad m => LinFunc v c -> Bounds c -> LPT v c m () #-}
-- | The most general form of an unlabeled constraint.
constrain :: MonadState (LP v c) m => LinFunc v c -> Bounds c -> m ()
constrain f bds = modify addConstr where
        addConstr lp@LP{..}
                = lp{constraints = Constr Nothing f bds:constraints}

{-# SPECIALIZE constrain' :: String -> LinFunc v c -> Bounds c -> LPM v c (),
        Monad m => String -> LinFunc v c -> Bounds c -> LPT v c m () #-}
-- | The most general form of a labeled constraint.
constrain' :: MonadState (LP v c) m => String -> LinFunc v c -> Bounds c -> m ()
constrain' lab f bds = modify addConstr where
        addConstr lp@LP{..}
                = lp{constraints = Constr (Just lab) f bds:constraints}

{-# SPECIALIZE setObjective :: LinFunc v c -> LPM v c (),
        Monad m => LinFunc v c -> LPT v c m () #-}
-- | Sets the objective function, overwriting the previous objective function.
setObjective :: MonadState (LP v c) m => LinFunc v c -> m ()
setObjective obj = modify setObj where
        setObj lp = lp{objective = obj}

{-# SPECIALIZE addObjective :: (Ord v, Group c) => LinFunc v c -> LPM v c (),
        (Ord v, Group c, Monad m) => LinFunc v c -> LPT v c m () #-}
-- | Adds this function to the objective function.
addObjective :: (Ord v, Group c, MonadState (LP v c) m) => LinFunc v c -> m ()
addObjective obj = modify addObj where
        addObj lp@LP{..} = lp {objective = obj + objective}

{-# SPECIALIZE addWeightedObjective ::
        (Ord v, Ring c) => c -> LinFunc v c -> LPM v c (),
        (Ord v, Ring c, Monad m) => c -> LinFunc v c -> LPT v c m () #-}
-- | Adds this function to the objective function, with the specified weight.  Equivalent to
-- @'addObjective' (wt '*^' obj)@.
addWeightedObjective :: (Ord v, Ring c, MonadState (LP v c) m) =>
                        c -> LinFunc v c -> m ()
addWeightedObjective wt obj = addObjective (wt *^ obj)

{-# SPECIALIZE setVarBounds :: (Ord v, Ord c) => v -> Bounds c -> LPM v c (),
        (Ord v, Ord c, Monad m) => v -> Bounds c -> LPT v c m () #-}
-- | The most general way to set constraints on a variable.
-- If you constrain a variable more than once, the constraints will be combined.
-- If you combine mutually contradictory constraints, an error will be generated.
-- This is more efficient than creating an equivalent function constraint.
setVarBounds :: (Ord v, Ord c, MonadState (LP v c) m) => v -> Bounds c -> m ()
setVarBounds var bds = modify addBds where
        addBds lp@LP{..} = lp{varBounds = insertWith mappend var bds varBounds}

{-# SPECIALIZE setVarKind :: Ord v => v -> VarKind -> LPM v c (),
        (Ord v, Monad m) => v -> VarKind -> LPT v c m () #-}
-- | Sets the kind ('type') of a variable.  See 'VarKind'.
setVarKind :: (Ord v, MonadState (LP v c) m) => v -> VarKind -> m ()
setVarKind v k = modify setK where
        setK lp@LP{..} = lp{varTypes = insertWith mappend v k varTypes}
