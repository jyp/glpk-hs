{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE TupleSections, RecordWildCards #-}

-- | Interface between the Haskell representation of a linear programming problem, a value of type 'LP', and
-- the GLPK solver.  The options available to the solver correspond naturally with GLPK's available options,
-- so to find the meaning of any particular option, consult the GLPK documentation.
-- 
-- The option of which solver to use -- the general LP solver, which solves a problem over the reals, or the 
-- MIP solver, which allows variables to be restricted to integers -- can be made by choosing the appropriate
-- constructor for 'GLPOpts'.
-- 
-- The marshalling from Haskell to C is specialized for 'Int's and 'Double's, so using those types in your
-- linear program is recommended.
module Data.LinearProgram.GLPK.Solver (
	-- * Solver options
	GLPOpts(..),
	simplexDefaults, 
	mipDefaults, 
	-- * Running the solver
	glpSolveVars,
	RowValue(..),
	glpSolveAll,
	-- * GLPK enumerations
	ReturnCode(..),
	MsgLev(..), 
	BranchingTechnique(..),
	BacktrackTechnique(..), 
	Preprocessing(..), 
	Cuts(..)) where 

import Control.Monad

import Data.Map
import Data.LinearProgram.Spec
import Data.LinearProgram.GLPK.Common

-- | Options available for customizing GLPK operations.  This also determines
-- which kind of solving is performed -- relaxed LP, or MIP.
data GLPOpts = SimplexOpts {msgLev :: MsgLev, tmLim :: !Int, presolve :: Bool} |
	MipOpts {msgLev :: MsgLev, tmLim :: !Int, presolve :: Bool,
		brTech :: BranchingTechnique, btTech :: BacktrackTechnique,
		ppTech :: Preprocessing,
		fpHeur :: Bool,
		cuts :: [Cuts],
		mipGap :: !Double}

data RowValue v c = RowVal {row :: !(Constraint v c), rowVal :: !Double}

simplexDefaults, mipDefaults :: GLPOpts
simplexDefaults = SimplexOpts MsgOn 10000 True
mipDefaults = MipOpts MsgOn 10000 True DrTom LocBound AllPre False [] 0.0

{-# SPECIALIZE glpSolveVars :: Ord v => GLPOpts -> LP v Double -> IO (ReturnCode, Maybe (Double, Map v Double)),
	Ord v => GLPOpts -> LP v Int -> IO (ReturnCode, Maybe (Double, Map v Double)) #-}
-- | Solves the linear or mixed integer programming problem.  Returns
-- the value of the objective function, and the values of the variables.
glpSolveVars :: (Ord v, Real c) => GLPOpts -> LP v c -> IO (ReturnCode, Maybe (Double, Map v Double))
glpSolveVars opts@SimplexOpts{} lp = runGLPK $ do
	(code, vars) <- doGLP opts lp
	liftM (code, ) $ maybe (return Nothing) ( \ vars -> do
		obj <- getObjVal
		vals <- sequence [do
			val <- getColPrim i
			return (v, val)
				| (v, i) <- assocs vars]
		return (Just (obj, fromDistinctAscList vals))) vars
glpSolveVars opts@MipOpts{} lp = runGLPK $ do
	(code, vars) <- doGLP opts lp
	liftM (code, ) $ maybe (return Nothing) (\ vars -> do
		obj <- mipObjVal
		vals <- sequence [do
			val <- mipColVal i
			return (v, val)
				| (v, i) <- assocs vars]
		return (Just (obj, fromDistinctAscList vals))) vars

{-# SPECIALIZE glpSolveAll :: 
	Ord v => GLPOpts -> LP v Double -> IO (ReturnCode, Maybe (Double, Map v Double, [RowValue v Double])),
	Ord v => GLPOpts -> LP v Int -> IO (ReturnCode, Maybe (Double, Map v Double, [RowValue v Int])) #-}
-- | Solves the linear or mixed integer programming problem.  Returns
-- the value of the objective function, the values of the variables,
-- and the values of any labeled rows.
glpSolveAll :: (Ord v, Real c) => GLPOpts -> LP v c -> IO (ReturnCode, Maybe (Double, Map v Double, [RowValue v c]))
glpSolveAll opts@SimplexOpts{} lp@LP{..} = runGLPK $ do
	(code, vars) <- doGLP opts lp
	liftM (code, ) $ maybe (return Nothing) (\ vars -> do
		obj <- getObjVal
		vals <- sequence [do
			val <- getColPrim i
			return (v, val)
				| (v, i) <- assocs vars]
		rows <- sequence [liftM (RowVal c) (getRowPrim i)
					| (i, c) <- zip [1..] constraints]
		return (Just (obj, fromDistinctAscList vals, rows))) vars
glpSolveAll opts@MipOpts{} lp@LP{..} = runGLPK $ do
	(code, vars) <- doGLP opts lp
	liftM (code, ) $ maybe (return Nothing) (\ vars -> do
		obj <- mipObjVal
		vals <- sequence [do
			val <- mipColVal i
			return (v, val)
				| (v, i) <- assocs vars]
		rows <- sequence [liftM (RowVal c) (mipRowVal i)
					| (i, c) <- zip [1..] constraints]
		return (Just (obj, fromDistinctAscList vals, rows))) vars

{-# SPECIALIZE doGLP :: Ord v => GLPOpts -> LP v Double -> GLPK (ReturnCode, Maybe (Map v Int)),
	Ord v => GLPOpts -> LP v Int -> GLPK (ReturnCode, Maybe (Map v Int)) #-}
doGLP :: (Ord v, Real c) => GLPOpts -> LP v c -> GLPK (ReturnCode, Maybe (Map v Int))
doGLP SimplexOpts{..} lp = do
	vars <- writeProblem lp
	success <- solveSimplex msgLev tmLim presolve
	bad <- getBadRay
	maybe (return (success, guard (gaveAnswer success) >> Just vars)) (fail . show) bad
doGLP MipOpts{..} lp = do
	vars <- writeProblem lp
	success <- mipSolve msgLev brTech btTech ppTech fpHeur cuts mipGap tmLim presolve
	bad <- getBadRay
	return (success, guard (gaveAnswer success) >> Just vars)