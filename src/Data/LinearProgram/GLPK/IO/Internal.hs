{-# LANGUAGE ForeignFunctionInterface #-}

module Data.LinearProgram.GLPK.IO.Internal (readGLPLP, writeGLPLP) where
import Prelude hiding ((+), subtract)
import Control.Monad
import Control.Monad.Trans (liftIO, lift)

import Data.Map hiding (map, filter)
import Debug.Trace
import Foreign.Storable

import Data.LinearProgram.Common
import Data.LinearProgram.GLPK.Common
import Control.Monad.LPMonad.Internal

foreign import ccall unsafe "c_glp_write_lp" glpWriteLP :: Ptr GlpProb -> CString -> IO ()
foreign import ccall unsafe "c_glp_read_lp" glpReadLP :: Ptr GlpProb -> CString -> IO ()
foreign import ccall unsafe "c_glp_set_col_name" glpSetColName :: Ptr GlpProb -> CInt -> CString -> IO ()
foreign import ccall unsafe "c_glp_set_row_name" glpSetRowName :: Ptr GlpProb -> CInt -> CString -> IO ()
foreign import ccall unsafe "c_glp_get_obj_dir" glpGetObjDir :: Ptr GlpProb -> IO CInt
foreign import ccall unsafe "c_glp_get_num_rows" glpGetNumRows :: Ptr GlpProb -> IO CInt
foreign import ccall unsafe "c_glp_get_num_cols" glpGetNumCols :: Ptr GlpProb -> IO CInt
foreign import ccall unsafe "c_glp_get_row_name" glpGetRowName :: Ptr GlpProb -> CInt -> IO CString
foreign import ccall unsafe "c_glp_get_col_name" glpGetColName :: Ptr GlpProb -> CInt -> IO CString
foreign import ccall unsafe "c_glp_get_col_kind" glpGetColKind :: Ptr GlpProb -> CInt -> IO CInt
foreign import ccall unsafe "c_glp_get_row_type" glpGetRowType :: Ptr GlpProb -> CInt -> IO CInt
foreign import ccall unsafe "c_glp_get_col_type" glpGetColType :: Ptr GlpProb -> CInt -> IO CInt
foreign import ccall unsafe "c_glp_get_row_lb" glpGetRowLb :: Ptr GlpProb -> CInt -> IO CDouble
foreign import ccall unsafe "c_glp_get_col_lb" glpGetColLb :: Ptr GlpProb -> CInt -> IO CDouble
foreign import ccall unsafe "c_glp_get_row_ub" glpGetRowUb :: Ptr GlpProb -> CInt -> IO CDouble
foreign import ccall unsafe "c_glp_get_col_ub" glpGetColUb :: Ptr GlpProb -> CInt -> IO CDouble
foreign import ccall unsafe "c_glp_get_obj_coef" glpGetObjCoef :: Ptr GlpProb -> CInt -> IO CDouble
foreign import ccall unsafe "c_glp_get_mat_row" glpGetMatRow :: Ptr GlpProb -> CInt -> Ptr CInt -> Ptr CDouble -> IO CInt

writeLP :: FilePath -> GLPK ()
writeLP file = GLP $ withCString file . glpWriteLP

readLP :: FilePath -> GLPK ()
readLP file = GLP $ withCString file . glpReadLP

getDir :: GLPK Direction
getDir = liftM (toEnum . subtract 1 . fromIntegral) $ GLP glpGetObjDir

getRowName, getColName :: Int -> GLPK (Maybe String)
getRowName i = GLP $ peekCAString' <=< flip glpGetRowName (fromIntegral i)
getColName i = GLP $ peekCAString' <=< flip glpGetColName (fromIntegral i)

peekCAString' :: CString -> IO (Maybe String)
peekCAString' str
	| str == nullPtr	= return Nothing
	| otherwise		= liftM Just $ peekCAString str

getNumRows, getNumCols :: GLPK Int
getNumRows = liftM fromIntegral $ GLP glpGetNumRows
getNumCols = liftM fromIntegral $ GLP glpGetNumCols

rowBounds, colBounds :: Int -> GLPK (Bounds Double)
rowBounds = loadBounds (getCDouble glpGetRowLb) (getCDouble glpGetRowUb) (getCInt glpGetRowType)
colBounds = loadBounds (getCDouble glpGetColLb) (getCDouble glpGetColUb) (getCInt glpGetColType)

colKind :: Int -> GLPK VarKind
colKind = liftM (toEnum . subtract 1) . getCInt glpGetColKind

getCInt :: (Ptr GlpProb -> CInt -> IO CInt) -> Int -> GLPK Int
getCInt f i = GLP $ \ lp -> liftM fromIntegral $ f lp (fromIntegral i)

getCDouble :: (Ptr GlpProb -> CInt -> IO CDouble) -> Int -> GLPK Double
getCDouble f i = GLP $ \ lp -> liftM realToFrac $ f lp (fromIntegral i)

setRowName :: Int -> String -> GLPK ()
setRowName i nam = GLP $ withCString nam . flip glpSetRowName (fromIntegral i)

setColName :: Int -> String -> GLPK ()
setColName i nam = GLP $ withCString nam . flip glpSetColName (fromIntegral i)

loadBounds :: (Int -> GLPK Double) -> (Int -> GLPK Double) ->
	(Int -> GLPK Int) -> Int -> GLPK (Bounds Double)
loadBounds lb ub tp i = do
	typ <- tp i
	case typ of
		1	-> return Free
		2	-> liftM LBound (lb i)
		3	-> liftM UBound (ub i)
		4	-> liftM2 Bound (lb i) (ub i)
		_	-> liftM Equ (lb i)

getObjCoef :: Int -> GLPK Double
getObjCoef = getCDouble glpGetObjCoef

getRows :: GLPK [(Int, [(Int, Double)])]
getRows = do	n <- getNumRows
		m <- getNumCols
		ixs <- liftIO $ mallocArray (m+1)
		coefs <- liftIO $ mallocArray (m+1)
		sequence [do
			k <- liftM fromIntegral $ GLP $ \ lp -> glpGetMatRow lp (fromIntegral i) ixs coefs
			ixsL <- liftIO $ mapM (peekElemOff ixs) [1..k]
			coefsL <- liftIO $ mapM (peekElemOff ixs) [1..k]
			return (i, zip (map fromIntegral ixsL) (map realToFrac coefsL))
			| i <- [1..n]]

readGLPLP :: FilePath -> GLPK (LP String Double)
readGLPLP file = execLPT $ do
	lift $ readLP file
	setDirection =<< lift getDir
	nCols <- lift getNumCols
	names <- lift $ liftM fromList $ mapM (\ i -> do
		Just name <- getColName i
		return (i, name)) [1..nCols]
	sequence_ [do
		bds <- lift $ colBounds i
		kind <- lift $ colKind i
		setVarBounds name bds
		setVarKind name kind
		return (i, name)
			| (i, name) <- assocs names]
	rowContents <- lift getRows
	sequence_ [do
		bds <- lift $ rowBounds i
		name <- lift $ getRowName i
		maybe constrain constrain' name
			(linCombination [(v, names ! j) | (j, v) <- row]) bds
			| (i, row) <- rowContents]
	obj <- lift $ sequence [do
		c <- getObjCoef i
		return (name, c) | (i, name) <- assocs names]
	setObjective (fromList (filter ((/= 0) . snd) obj))

writeGLPLP :: (Show v, Ord v, Real c) => FilePath -> LP v c -> GLPK ()
writeGLPLP file lp = do
	vars <- writeProblem lp
	sequence_ [setColName i (show v) | (v, i) <- assocs vars]
	sequence_ [setRowName i lab | (i, Constr (Just lab) _ _) <- zip [1..] (constraints lp)]
	writeLP file
