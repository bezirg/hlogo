{-# LANGUAGE TemplateHaskell, CPP, ExplicitForAll #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-- | 
-- Module      :  Language.Logo.Keyword
-- Copyright   :  (c) 2013-2016, the HLogo team
-- License     :  BSD3
-- Maintainer  :  Nikolaos Bezirgiannis <bezirgia@cwi.nl>
-- Stability   :  experimental
--
-- The module defines the macros of the HLogo language using TemplateHaskell (lisp-like macros).
module Language.Logo.Keyword
    (
      -- * Running an HLogo program
      run, runT
      -- * Declaring new breeds
    , breeds, directed_link_breed, undirected_link_breed
      -- * User-provided variables
    , globals, patches_own, turtles_own, links_own, breeds_own, link_breeds_own
    ) where

import Language.Haskell.TH
import Language.Logo.Core
import Language.Logo.Base
import Language.Logo.Prim
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Reader as Reader
import Control.Concurrent (runInUnboundThread)
import Control.Concurrent.STM
import qualified Data.Vector as V ((!), replicateM)
import Data.List (genericLength)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import System.Environment (withArgs, getArgs)
import Control.Monad (liftM, filterM)
import System.Random.TF.Instances (randomR)
import System.IO.Unsafe (unsafePerformIO)
import Data.Foldable (foldlM, foldrM)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>),(<*>))
#endif
import Data.IORef


-- | globals macro takes a list of variable names (as strings) and creates top-level global variables.
-- Global variables have type 'Double' and are initialized to 0.
globals :: [String] -> Q [Dec]
globals vs  = do
    clear_globals <- [d|clear_globals :: C Observer () IO () 
                        clear_globals =
                         $(if null vs then [| return ()|] else [| lift $(doE $ map (\ v -> noBindS [| atomically (writeTVar $(varE (mkName ("__" ++ v))) 0) |]) vs )|])|]
    -- create 1 getter (no unsafe) per global variable
    settersGetters <- liftM concat $ mapM (\ v -> do
                      noInline <- pragInlD (mkName ("__" ++ v)) NoInline FunLike AllPhases                 
                      topLevelVar <- [d|$(varP $ mkName ("__" ++ v)) = unsafePerformIO $ newTVarIO 0 :: TVar Double |]
                      getVarSig <- sigD (mkName v) [t| forall _s _s' m. STMorIO m => C _s _s' m Double|] -- cannot infer the STMOrIO otherwise
                      getVar <- [d|$(varP $ mkName v) = readTVarSI $(varE (mkName ("__"++v))) |]

                      y <- newName "y"
                      setVar <- funD (mkName ("set_" ++ v)) [clause [varP y] 
                                                            (normalB [| lift $ writeTVar $(varE (mkName ("__" ++ v))) $! $(varE y) |]) []]
                      withVar <- funD (mkName ("with_" ++ v)) [clause [varP y] 
                                                              (normalB [| lift $ modifyTVar' $(varE (mkName ("__" ++ v))) $(varE y) |]) []]

                      return $ topLevelVar ++ getVar ++ [noInline, getVarSig, setVar, withVar]
                                         )  vs
    return $ clear_globals ++ settersGetters

-- | turtles_own macro takes a list of variable names (as strings) and creates corresponding field variables for each __turtle agent__.
-- This user-declared turtle-field variables have type 'Double' and are initialized to 0.
--
-- NB: turtle fields are inherited to all __turtle-like__ agents, i.e. both the turtle agents as well as all any-breed agents.
turtles_own :: [String] -> Q [Dec]
turtles_own vs = do
  tl <- [d|{-# INLINE turtles_length #-}
           turtles_length = $(litE $ integerL $ genericLength vs)
          |]
  -- Variables setters/getters
  pg <- mapM (\ (v, i) -> do
          p' <- sigD (mkName v) [t| forall _s' m. STMorIO m => C Turtle _s' m Double|]
          p <- [d|$(varP $ mkName v) = do
                      (MkTurtle {tvars_ = pv},_,_) <- Reader.ask
                      readTVarSI (pv V.! $(litE $ integerL i))
               |]
          y <- newName "y"
          w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do
                                                                       (MkTurtle {tvars_ = pv},_,_) <- Reader.ask
                                                                       lift $ writeTVar (pv V.! $(litE (integerL i))) $! $(varE y)
                                                                     |]) []]
          x <- funD (mkName ("with_" ++ v)) [clause [varP y] (normalB [| do
                                                                       (MkTurtle {tvars_ = pv},_,_) <- Reader.ask
                                                                       lift $ modifyTVar' (pv V.! $(litE (integerL i))) $(varE y)
                                                                     |]) []]

          return $ p':p++[w,x]
            ) (zip vs [0..])
  return $ tl ++ concat pg


-- | patches_own macro takes a list of variable names (as strings) and creates corresponding field variables for each __patch agent__.
-- This user-declared patch-field variables have type 'Double' and are initialized to 0.
patches_own :: [String] -> Q [Dec]
patches_own vs = do
  pl <- [d|{-# INLINE patches_length #-}
           patches_length = $(litE $ integerL $ genericLength vs)
          |]
  -- Variables setters/getters
  pg <- mapM (\ (v, i) -> do
          p' <- sigD (mkName v) [t| forall s _s' m. (TurtlePatch s, STMorIO m) => C s _s' m Double|]
          p <- [d|$(varP $ mkName v) = do
                          (s,_,_) <- Reader.ask
                          (MkPatch {pvars_ = pv}) <- patch_on_ s
                          readTVarSI $ pv V.! $(litE $ integerL i)
                 |]
          y <- newName "y"
          w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do 
                                                 (s,_,_) <- Reader.ask
                                                 (MkPatch {pvars_ = pv}) <- patch_on_ s
                                                 lift $ writeTVar (pv V.! $(litE (integerL i))) $! $(varE y)
                                                                    |]) []]
          x <- funD (mkName ("with_" ++ v)) [clause [varP y] (normalB [| do 
                                                 (s,_,_) <- Reader.ask
                                                 (MkPatch {pvars_ = pv}) <- patch_on_ s
                                                 lift $ modifyTVar' (pv V.! $(litE (integerL i))) $(varE y)
                                                                     |]) []]
          return $ p':p ++[w,x]
            ) (zip vs [0..])
  return $ pl ++ concat pg


-- | links_own macro takes a list of variable names (as strings) and creates corresponding field variables for each __link agent__.
-- This user-declared link-field variables have type 'Double' and are initialized to 0.
--
-- NB: link fields are inherited to all __link-like__ agents, i.e. both the link agents as well as all any-linkbreed agents.
links_own :: [String] -> Q [Dec]
links_own vs = do
  ll <- [d|{-# INLINE links_length #-}
           links_length = $(litE $ integerL $ genericLength vs)
          |]
  pg <- mapM (\ (v, i) -> do
          p' <- sigD (mkName v) [t| forall _s' m. STMorIO m => C Link _s' m Double|]
          p <- [d|$(varP $ mkName v) = do
                      (MkLink {lvars_ = pv},_,_) <- Reader.ask
                      readTVarSI (pv V.! $(litE (integerL i)))
               |]
          y <- newName "y"
          w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do 
                                                                       (MkLink {lvars_ = pv},_,_) <- Reader.ask
                                                                       lift $ writeTVar (pv V.! $(litE (integerL i))) $! $(varE y) 
                                                                     |]) []]
          x <- funD (mkName ("with_" ++ v)) [clause [varP y] (normalB [| do 
                                                                       (MkLink {lvars_ = pv},_,_) <- Reader.ask
                                                                       lift $ modifyTVar' (pv V.! $(litE (integerL i))) $(varE y) 
                                                                     |]) []]

          return $ p':p ++[w,x]
            ) (zip vs [0..])
  return $ ll ++ concat pg


-- | breeds_own macro takes a list of variable names (as strings) and creates corresponding field variables for each __breed-exact agent__.
-- A specific breed must be declared with 'breeds'.
-- This user-declared breed-field variables have type 'Double' and are initialized to 0.
-- 
-- NOTE TO SELF: also applies for linkbreeds_own: this cannot become "wolves_own", etc. because of the _GHC stage restriction_. This means
-- that the keywords 'breeds' and 'breeds_own' have to be defined in separate Haskell modules.
breeds_own :: String -> [String] -> Q [Dec]
breeds_own p vs = do
  y <- newName "y"
  cb <- funD (mkName ("create_" ++ p)) [clause [varP y]
                                       (normalB [| create_breeds $(litE (stringL p)) $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  cob <- funD (mkName ("create_ordered_" ++ p)) [clause [varP y]
                                                (normalB [| create_ordered_breeds $(litE (stringL p)) $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  sp <- funD (mkName ("sprout_" ++ p)) [clause [varP y] (normalB [| do
                                                           (MkPatch {pxcor_=px,pycor_=py}, _,ogen_) <- Reader.ask
                                                           lift $ do
                                                              oldWho <- atomically $ do
                                                                                      x <- readTVar __who
                                                                                      writeTVar __who $! ($(varE y) +x)
                                                                                      return x
                                                              gen <- readIORef ogen_
                                                              (ns,gen') <- foldrM (\ i (ts,g) -> do
                                                                let (rpc,g') = randomR (0,13 :: Int) g
                                                                let (rih,g'') = randomR (0,360 :: Int) g'
                                                                t <- MkTurtle i <$>
                                                                    newTVarIO $(litE (stringL p)) <*>
                                                                    newTVarIO (primary_colors !! rpc)  <*>
                                                                    newTVarIO (fromIntegral rih) <*>
                                                                    newTVarIO (fromIntegral px) <*>
                                                                    newTVarIO (fromIntegral py) <*>
                                                                    newTVarIO "default" <*>
                                                                    newTVarIO "" <*>
                                                                    newTVarIO 9.9 <*>
                                                                    newTVarIO False <*>
                                                                    newTVarIO 1 <*>
                                                                    newTVarIO 1 <*>
                                                                    newTVarIO Up <*>
                                                                    (V.replicateM $(litE (integerL (genericLength vs))) (newTVarIO 0))
                                                                return ((i,t):ts, g'')
                                                                  ) ([],gen) [oldWho..oldWho + $(varE y)-1]
                                                              let ns' = IM.fromDistinctAscList ns
                                                              writeIORef ogen_ $! gen'
                                                              atomically $ modifyTVar' __turtles (`IM.union` ns') 
                                                              return ns'
                                                         |]) []]





  pg <- mapM (\ (v, i) -> do
          p' <- sigD (mkName v) [t| forall _s' m. STMorIO m => C Turtle _s' m Double|]
          p <- valD (varP (mkName v)) (normalB [| do
                                                 (MkTurtle {tvars_ = pv},_,_) <- Reader.ask
                                                 readTVarSI (pv V.! $(litE (integerL i)))
                                              |]) []
          w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do 
                                                                       (MkTurtle {tvars_ = pv},_,_) <- Reader.ask
                                                                       lift $ writeTVar (pv V.! $(litE (integerL i))) $! $(varE y)
                                                                     |]) []]
          x <- funD (mkName ("with_" ++ v)) [clause [varP y] (normalB [| do 
                                                                       (MkTurtle {tvars_ = pv},_,_) <- Reader.ask 
                                                                       lift $ modifyTVar' (pv V.! $(litE (integerL i))) $(varE y)
                                                                     |]) []]

          return [p',p,w,x]
            ) (zip vs [0..])
  return $ cb : sp : cob : concat pg

-- | Like 'breeds_own' but for __linkbreeds__, which are declared with 'directed_link_breed' or 'undirected_link_breed'.
link_breeds_own :: String -> [String] -> Q [Dec]
link_breeds_own p vs = do
  y <- newName "y"
  cls <- funD (mkName $ "create_" ++ p ++ "_with") [clause [varP y] (normalB 
          [|do
            (MkTurtle {who_=x},_,_) <- Reader.ask
            ls <- lift $ readTVar __links
            let insertLink t x s =  do
                       n <- newLBreed x t False $(litE (stringL p)) $(litE (integerL (genericLength vs)))
                       return $ M.insertWith (flip const) (t,x) n $ M.insertWith (flip const) (x,t) n s
            ls' <- lift $ foldlM (\ acc (MkTurtle {who_=t}) -> insertLink t x acc) ls (IM.elems $(varE y))
            lift $ writeTVar __links $! ls' :: C Turtle _s' STM ()
          |]) []]
  cts <- funD (mkName $ "create_" ++ p ++ "_to") [clause [varP y] (normalB 
          [|do
              (MkTurtle {who_=x},_,_) <- Reader.ask
              ls <- lift $ readTVar __links
              let insertLink t x s =  do
                       n <- newLBreed x t True $(litE (stringL p)) $(litE (integerL (genericLength vs)))
                       return $ M.insertWith (flip const) (x,t) n s
              ls' <- lift $ foldlM (\ acc (MkTurtle {who_=t}) -> insertLink t x acc) ls (IM.elems $(varE y))
              lift $ writeTVar __links $! ls' :: C Turtle _s' STM ()       
          |]) []]
  cfs <- funD (mkName $ "create_" ++ p ++ "_from") [clause [varP y] (normalB 
          [|do
              (MkTurtle {who_=x},_,_) <- Reader.ask
              ls <- lift $ readTVar __links
              let insertLink f x s =  do
                       n <- newLBreed f x True $(litE (stringL p)) $(litE (integerL (genericLength vs)))
                       return $ M.insertWith (flip const) (f,x) n s
              ls' <- lift $ foldlM (\ acc (MkTurtle {who_=f}) -> insertLink f x acc) ls (IM.elems $(varE y))
              lift $ writeTVar __links $! ls' :: C Turtle _s' STM ()
          |]) []]

  pg <- mapM (\ (v, i) -> do
          p' <- sigD (mkName v) [t| forall _s' m. STMorIO m => C Link _s' m Double|]
          p <- valD (varP (mkName v)) (normalB [| do
                                                 (MkLink {lvars_ = pv},_,_) <- Reader.ask
                                                 readTVarSI (pv V.! $(litE (integerL i)))

                                              |]) []

          y <- newName "y"
          w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do 
                                                                       (MkLink {lvars_ = pv},_,_) <- Reader.ask
                                                                       lift $ writeTVar (pv V.! $(litE (integerL i))) $! $(varE y)
                                                                     |]) []]
          x <- funD (mkName ("with_" ++ v)) [clause [varP y] (normalB [| do 
                                                                       (MkLink {lvars_ = pv},_,_) <- Reader.ask
                                                                       lift $ modifyTVar' (pv V.! $(litE (integerL i))) $(varE y)
                                                                     |]) []]

          return [p',p,w,x]
            ) (zip vs [0..])
  return $ cls : cts : cfs : concat pg


-- | Takes a 2-length list, with 1st element the plural name of the breeds (e.g. "wolves") and as 2nd element the singlular form (e.g. "wolf").
--
-- NB: breed agents share the same who counter (same namespace). The agentset 'turtles' returns all turtle-like agents (both turtles and any-breeds).
breeds :: [String] -> Q [Dec]
breeds [plural,singular] = do
  sp' <- sigD (mkName plural) [t| forall _s _s' m. STMorIO m => C _s _s' m Turtles |]
  sp <- valD (varP (mkName plural)) (normalB [| liftM IM.fromDistinctAscList $ (filterM (\ (_,MkTurtle {tbreed_ = tb}) -> do
                                                                                     b <- readTVarSI tb
                                                                                     return $ b == $(litE (stringL plural))) =<< (liftM IM.toAscList $ turtles))
                                        |]) []
  up <- valD (varP (mkName ("unsafe_" ++ plural))) (normalB [| with (breed >>= \ b -> return (b == $(litE (stringL plural)))) =<< turtles |]) []
  x <- newName "x"
  y <- newName "y"

  ss' <- sigD (mkName singular) [t| forall _s _s' m. STMorIO m => Int -> C _s _s' m Turtle |]
  ss <- funD (mkName singular) [clause [varP y] (normalB [| do 
                                                    ts <- readTVarSI __turtles
                                                    let {t = ts IM.! $(varE y)}
                                                    b <- readTVarSI (tbreed_ t)
                                                    return $ if b == $(litE (stringL plural)) 
                                                             then t 
                                                             else error ("turtle is not a " ++ singular) |]) []]
  th' <- sigD (mkName (plural ++ "_here")) [t| forall s _s'. TurtlePatch s => C s _s' STM Turtles|]
  th <- valD (varP (mkName (plural ++ "_here"))) (normalB [| do 
                                                       (s,_,_) <- Reader.ask
                                                       (MkPatch {pxcor_=px, pycor_=py}) <- patch_on_ s
                                                       IM.fromDistinctAscList <$> (filterM (\ (_,MkTurtle {xcor_ = x_, ycor_ = y_, tbreed_ = tb}) -> lift $ do 
                                                                  x' <- readTVar x_
                                                                  y' <- readTVar y_
                                                                  b <- readTVar tb
                                                                  return $ round x' == px && round y' == py && b == $(litE (stringL plural))) =<< (IM.toAscList <$> turtles)) |]) []

  ta <- funD (mkName (plural ++ "_at")) [clause [varP x, varP y] (normalB [| do 
                                                                       MkPatch {pxcor_=px,pycor_=py} <- patch_at $(varE x) $(varE y)
                                                                       IM.fromDistinctAscList <$> (filterM (\ (_,MkTurtle {xcor_ = x_, ycor_ = y_, tbreed_ = tb}) -> lift $ do 
                                                                                  x' <- readTVar x_
                                                                                  y' <- readTVar y_
                                                                                  b <- readTVar tb
                                                                                  return $ round x' == px && round y' == py && b == $(litE (stringL plural))) =<< (IM.toAscList <$> turtles)) |]) []]
  -- to <- funD (mkName (p ++ "_on")) [clause [varP y] (normalB [|
  --                                                             case $(varE y) of
  --                                                               [] -> return []
  --                                                               ps@(PatchRef _ _ : _) -> with (liftM (flip elem ps . head) patch_here) =<< $(varE (mkName ("unsafe_" ++ p)))
  --                                                               ts@(TurtleRef _ _ : _) -> $(varE (mkName (p ++ "_on"))) =<< of_ (liftM head patch_here) ts
  --                                                               (a:_) -> throw $ ContextException (s ++ "or patch agentset") a
  --                                                           |]) []]

  ib <- funD (mkName ("is_" ++ singular ++ "p")) [clause [varP y] 
                                          (normalB [| case $(varE y) of
                                                        MkTurtle {tbreed_ = tb} -> do
                                                         liftM ($(litE (stringL plural)) ==) (readTVarSI tb)
                                                    |]) []]
  return [sp',sp,up,ss',ss,th',th,ta,ib]
breeds _ = fail "Breeds accepts exactly two string arguments, e.g. breeds [\"wolves\", \"wolf\"]"


-- | Creates a directed-linkbreed. The arguments behave the same as 'breeds'. 
directed_link_breed :: [String] -> Q [Dec]
directed_link_breed [p,s] = do
  sp <- [d|$(varP (mkName p)) = do ls <- links
                                   filterM (\ (MkLink {lbreed_ = b}) -> return $ b == $(litE (stringL p))) ls 
         |]
  x <- newName "x"
  y <- newName "y"
  ss <- funD (mkName s) [clause [varP x, varP y] 
                        (normalB [| do ls <- lift $ readTVar __links
                                       maybe nobody (return . return) $ M.lookup ($(varE x),$(varE y)) ls |]) []]
  return $ ss : sp
directed_link_breed _ = fail "Link Breeds accepts exactly two string arguments, e.g. breeds [\"streets\", \"street\"]"

-- | Creates an undirected-linkbreed. The arguments behave the same as 'breeds'. 
undirected_link_breed :: [String] -> Q [Dec]
undirected_link_breed [p,s] = do
  sp <- [d|$(varP (mkName p)) = do ls <- links
                                   filterM (\ (MkLink {lbreed_ = b}) -> return $ b == $(litE (stringL p))) ls
          |]
  x <- newName "x"
  y <- newName "y"
  ss <- funD (mkName s) [clause [varP x, varP y] 
                        (normalB [| do ls <- lift $ readTVar __links
                                       maybe nobody (return . return) $ M.lookup ($(varE x),$(varE y)) ls |]) []]
  return $ ss : sp
undirected_link_breed _ = fail "Link Breeds accepts exactly two string arguments, e.g. breeds [\"streets\",\"street\"]"


-- | The entrypoint of a non-interactive HLogo. Takes as input a list of HLogo procedure names (as strings) to run in sequence.
-- The program exits either when the last procedure stops, or when a program exception occurs.
--
-- NOTE TO SELF: cannot have 'run' take procedures as values of 'CIO ()', possibly for the same reason as 'breeds_own', i.e. _GHC stage restriction_. 
-- It has to take either a list of strings or a list of symbol names (this was chosen). 
run :: [String] -> Q [Dec]
run procs = do
  plength <- maybe (litE $ integerL 0) varE <$> lookupValueName "patches_length"
  tlength <- maybe (litE $ integerL 0) varE <$> lookupValueName "turtles_length" 
  llength <- maybe (litE $ integerL 0) varE <$> lookupValueName "links_length"
  
  cts <- [d|create_turtles y = do
                  (_,_,ogen_) <- Reader.ask
                  lift (do
                    oldWho <- atomically $ do
                        x <- readTVar __who
                        writeTVar __who $! (y +x)
                        return x
                    gen <- readIORef ogen_
                    (ns,gen') <- foldrM (\ i (ts,g) -> do
                         let (rpc,g') = randomR (0,13 :: Int) g
                         let (rih,g'') = randomR (0,360 :: Int) g'
                         t <- MkTurtle i <$>
                             newTVarIO "turtles" <*>
                             newTVarIO (primary_colors !! rpc)  <*>
                             newTVarIO (fromIntegral rih) <*>
                             newTVarIO 0 <*>
                             newTVarIO 0 <*>
                             newTVarIO "default" <*>
                             newTVarIO "" <*>
                             newTVarIO 9.9 <*>
                             newTVarIO False <*>
                             newTVarIO 1 <*>
                             newTVarIO 1 <*>
                             newTVarIO Up <*>
                             (V.replicateM $(tlength) (newTVarIO 0))
                         return ((i,t):ts, g'')
                           ) ([],gen) [oldWho..oldWho + y-1]
                    let ns' = IM.fromDistinctAscList ns
                    writeIORef ogen_ $! gen'
                    atomically $ modifyTVar' __turtles (`IM.union` ns') 
                    return ns') :: C Observer () IO Turtles
            |]

  sp <- [d|sprout y = do
                        (MkPatch {pxcor_=px,pycor_=py},_,ogen_) <- Reader.ask
                        lift $ do 
                          oldWho <- atomically $ do
                                                  x <- readTVar __who
                                                  writeTVar __who $! (y +x)
                                                  return x
                          gen <- readIORef ogen_
                          (ns,gen') <- foldrM (\ i (ts,g) -> do
                             let (rpc,g') = randomR (0,13 :: Int) g
                             let (rih,g'') = randomR (0,360 :: Int) g'
                             t <- MkTurtle i <$>
                                 newTVarIO "turtles" <*>
                                 newTVarIO (primary_colors !! rpc)  <*>
                                 newTVarIO (fromIntegral rih) <*>
                                 newTVarIO (fromIntegral px) <*>
                                 newTVarIO (fromIntegral py) <*>
                                 newTVarIO "default" <*>
                                 newTVarIO "" <*>
                                 newTVarIO 9.9 <*>
                                 newTVarIO False <*>
                                 newTVarIO 1 <*>
                                 newTVarIO 1 <*>
                                 newTVarIO Up <*>
                                 (V.replicateM $(tlength) (newTVarIO 0))
                             return ((i,t):ts, g'')
                               ) ([],gen) [oldWho..oldWho + y-1]
                          let ns' = IM.fromDistinctAscList ns
                          writeIORef ogen_ $! gen'
                          atomically $ modifyTVar' __turtles (`IM.union` ns') 
                          return ns'
        |]



  co <- [d|create_ordered_turtles :: Int -> C Observer () IO Turtles
           create_ordered_turtles y = lift (do
                                            let newTurtles w n = IM.fromDistinctAscList <$> mapM (\ (i,j) -> do
                                                                                                              t <- newOrderedTurtle i n j $(tlength)
                                                                                                              return (j, t))
                                                                                            (zip [1..n]  [w..w+n-1])
                                            oldWho <- atomically $ do
                                                                    x <- readTVar __who
                                                                    writeTVar __who $! (y +x)
                                                                    return x
                                            ns <- newTurtles oldWho y
                                            atomically $ modifyTVar' __turtles (`IM.union` ns) 
                                            return ns)
        |]


  crt <- [d|{-# INLINE crt #-}
            crt = create_turtles |]
  cro <- [d|{-# INLINE cro #-}
            cro = create_ordered_turtles |]
 
  clsw <- [d|{-# INLINE create_links_with #-}
             create_links_with :: Turtles -> C Turtle _s' STM ()
             create_links_with as =  do
              (MkTurtle {who_=x},_,_) <- Reader.ask
              ls <- lift $ readTVar __links
              ls' <- lift $ foldlM (\ acc (MkTurtle {who_=t}) -> insertLink t x acc) ls (IM.elems as)
              lift $ writeTVar __links $! ls'
                where insertLink t x s =  do
                       n <- newLBreed x t False "links" $(llength)
                       return $ M.insertWith (flip const) (t,x) n $ M.insertWith (flip const) (x,t) n s
          |]
  clst <- [d|{-# INLINE create_links_to #-}
             create_links_to :: Turtles -> C Turtle _s' STM ()
             create_links_to as = do
              (MkTurtle {who_=x},_,_) <- Reader.ask
              ls <- lift $ readTVar __links
              ls' <- lift $ foldlM (\ acc (MkTurtle {who_=t}) -> insertLink t x acc) ls (IM.elems as)
              lift $ writeTVar __links $! ls'
                where insertLink t x s =  do
                       n <- newLink x t $(llength)
                       return $ M.insertWith (flip const) (x,t) n s
          |]
  clsf <- [d|{-# INLINE create_links_from #-}
             create_links_from :: Turtles -> C Turtle _s' STM ()
             create_links_from as = do
              (MkTurtle {who_=x},_,_) <- Reader.ask
              ls <- lift $ readTVar __links
              ls' <- lift $ foldlM (\ acc (MkTurtle {who_=f}) -> insertLink f x acc) ls (IM.elems as)
              lift $ writeTVar __links $! ls'
                where insertLink f x s =  do
                       n <- newLink f x $(llength)
                       return $ M.insertWith (flip const) (f,x) n s
          |]

  clw <-  [d|{-# INLINE create_link_with #-}
             create_link_with :: Turtle -> C Turtle _s' STM ()
             create_link_with(MkTurtle {who_=t}) = do
              (MkTurtle {who_=x},_,_) <- Reader.ask
              ls' <- lift $ insertLink t x =<< readTVar __links
              lift $ writeTVar __links $! ls'
                where insertLink t x s =  do
                       n <- newLink x t $(llength)
                       return $ M.insertWith (flip const) (t,x) n $ M.insertWith (flip const) (x,t) n s
          |]

  clt <-  [d|{-# INLINE create_link_to #-}
             create_link_to :: Turtle -> C Turtle _s' STM ()
             create_link_to (MkTurtle {who_=t}) = do
               (MkTurtle {who_=x},_,_) <- Reader.ask
               ls' <- lift $ insertLink t x =<< readTVar __links
               lift $ writeTVar __links $! ls'
                where insertLink t x s =  do
                       n <- newLink x t $(llength)
                       return $ M.insertWith (flip const) (x,t) n s
            |]


  clf <-  [d|{-# INLINE create_link_from #-}
             create_link_from :: Turtle -> C Turtle _s' STM ()
             create_link_from (MkTurtle {who_=f}) = do
               (MkTurtle {who_=x},_,_) <- Reader.ask
               ls' <- lift $ insertLink f x =<< readTVar __links
               lift $ writeTVar __links $! ls'
                 where insertLink f x s =  do
                                    n <- newLink f x $(llength)
                                    return $ M.insertWith (flip const) (f,x) n s
           |]

  clear_all <- [d| 
               {-# INLINE clear_all #-}
               clear_all = do
                   $(varE (mkName "clear_globals"))
                   clear_ticks
                   clear_turtles
                   clear_links
                   clear_patches
                   -- Disabled for now, because not implemented/not related
                   -- clear_drawing
                   -- clear_all_plots
                   -- clear_output
               {-# INLINE ca #-}
               ca = clear_all 
               |]

  let as = map mkName procs
  
  mArgs <- lookupValueName "args"
  m <- case mArgs of
        Nothing -> [d| main = runInUnboundThread $
                               cInit $(plength) >>=
                               Reader.runReaderT (sequence_ ($(listE (map (\ a -> infixE (Just (varE a)) (varE (mkName ">>")) 
                                                                 (Just (appE (varE (mkName "return")) (conE (mkName "()"))))) as)) :: [C Observer () IO ()]))
                  |]
        Just args -> [d| main = do
                          args' <- getArgs
                          withArgs ($(varE args)++args') $ runInUnboundThread $
                               cInit $(plength) >>=
                               Reader.runReaderT (sequence_ ($(listE (map (\ a -> infixE (Just (varE a)) (varE (mkName ">>")) 
                                                                 (Just (appE (varE (mkName "return")) (conE (mkName "()"))))) as)) :: [C Observer () IO ()]))
                  |]
  
  clear_globals <- [d|clear_globals :: C Observer () IO ()
                      clear_globals = return ()
                   |]
  gl <- lookupValueName "clear_globals"
  return $ (case gl of
              Nothing -> (clear_globals ++)
              _ -> id) cts ++ crt ++ co ++ cro ++ clsw ++ clst ++ clsf ++ clw ++ clt ++ clf ++ sp ++ clear_all ++ m
                 
-- | Internal, used only in Test code.
runT :: C Observer () IO b -> IO b
runT as = cInit 0 >>= Reader.runReaderT as
             

{-# INLINE newOrderedTurtle #-}
-- | Internal
newOrderedTurtle :: Int -> Int -> Int -> Int -> IO Turtle -- ^ Index -> Order -> Who -> VarLength -> CSTM Turtle
newOrderedTurtle i o x to = do
    let rpc = primary_colors !! ((i-1) `mod` 14)
    let rdh = ((toEnum i-1) / toEnum o) * 360 :: Double
    MkTurtle x <$>
             newTVarIO "turtles" <*>
             newTVarIO rpc <*>          --  ordered primary color
             newTVarIO rdh <*> --  ordered double heading
             newTVarIO 0 <*>
             newTVarIO 0 <*>
             newTVarIO "default" <*>
             newTVarIO "" <*>
             newTVarIO 9.9 <*>
             newTVarIO False <*>
             newTVarIO 1 <*>
             newTVarIO 1 <*>
             newTVarIO Up <*>
             (V.replicateM to (newTVarIO 0))


-- | Internal, Utility function to make TemplateHaskell easier
create_breeds :: String -> Int -> Int -> C Observer () IO Turtles -- ^ Breed -> Size -> VarLength -> CSTM BreededTurtles
create_breeds b n to = do
  (_,_,ogen_) <- Reader.ask
  lift (do
         oldWho <- atomically $ do
                        x <- readTVar __who
                        writeTVar __who $! (n +x)
                        return x
         gen <- readIORef ogen_
         (ns,gen') <- foldrM (\ i (ts,g) -> do
                         let (rpc,g') = randomR (0,13 :: Int) g
                         let (rih,g'') = randomR (0,360 :: Int) g'
                         t <- MkTurtle i <$>
                             newTVarIO b <*>
                             newTVarIO (primary_colors !! rpc)  <*>
                             newTVarIO (fromIntegral rih) <*>
                             newTVarIO 0 <*>
                             newTVarIO 0 <*>
                             newTVarIO "default" <*>
                             newTVarIO "" <*>
                             newTVarIO 9.9 <*>
                             newTVarIO False <*>
                             newTVarIO 1 <*>
                             newTVarIO 1 <*>
                             newTVarIO Up <*>
                             (V.replicateM to (newTVarIO 0))
                         return ((i,t):ts, g'')
                            ) ([],gen) [oldWho..oldWho + n-1]
         let ns' = IM.fromDistinctAscList ns
         writeIORef ogen_ $! gen'
         atomically $ modifyTVar' __turtles (`IM.union` ns') 
         return ns')


-- | Internal, Utility function to make TemplateHaskell easier
create_ordered_breeds :: String -> Int -> Int -> C Observer () IO Turtles  -- ^ Breed -> Size -> VarLength -> CSTM BreededTurtles
create_ordered_breeds b n to = lift $ do
    oldWho <- atomically $ do
                        x <- readTVar __who
                        writeTVar __who $! (n +x)
                        return x
    ns <- newTurtles oldWho
    atomically $ modifyTVar' __turtles (`IM.union` ns) 
    return ns -- todo: can be optimized
        where
          newTurtles w = IM.fromDistinctAscList <$> mapM (\ (i,j) -> do
                                                            t <- newOrderedBreed i n b j to
                                                            return (j, t)) (zip [1..n] [w..w+n-1])
          newOrderedBreed :: Int -> Int -> String -> Int -> Int -> IO Turtle -- ^ Index -> Order -> Breed -> Who -> VarLength -> IO Turtle
          newOrderedBreed i o b x to = do
            let rpc = primary_colors !! ((i-1) `mod` 14)
            let rdh = ((toEnum i-1) / toEnum o) * 360 :: Double
            MkTurtle x <$>
                 newTVarIO b <*>
                 newTVarIO rpc <*> --  ordered primary color
                 newTVarIO rdh <*> --  ordered double heading
                 newTVarIO 0 <*>
                 newTVarIO 0 <*>
                 newTVarIO "default" <*>
                 newTVarIO "" <*>
                 newTVarIO 9.9 <*>
                 newTVarIO False <*>
                 newTVarIO 1 <*>
                 newTVarIO 1 <*>
                 newTVarIO Up <*>
                 (V.replicateM to (newTVarIO 0))


-- Links
-------


{-# WARNING newLink "TODO: determine at run-time the direction of links" #-}
-- | Internal
-- links directed by default, that makes create-link(s)-with faulty
newLink :: Int -> Int -> Int -> STM Link -- ^ FromIndex -> ToIndex -> VarLength -> CSTM Link
newLink f t ls = MkLink f t True <$>
                  newTVar 5 <*>
                  newTVar "" <*>
                  newTVar 9.9 <*>
                  newTVar False <*>
                  newTVar "links" <*>
                  newTVar 0 <*>
                  newTVar "default" <*>
                  newTVar None <*>
                  (V.replicateM ls (newTVar 0))


-- | Internal
newLBreed :: Int -> Int -> Bool -> String -> Int -> STM Link -- ^ FromIndex -> ToIndex -> Directed-> Breed -> VarLength -> CSTM Link
newLBreed f t d b ls = MkLink f t d <$>
                  newTVar 5 <*>
                  newTVar "" <*>
                  newTVar 9.9 <*>
                  newTVar False <*>
                  newTVar b <*>
                  newTVar 0 <*>
                  newTVar "default" <*>
                  newTVar None <*>
                  (V.replicateM ls (newTVar 0))
                              

-- | Internal
{-# INLINE primary_colors #-}
-- | Not much gain to turn this into a vector.
primary_colors :: [Double]
primary_colors = [gray, red, orange, brown, yellow, green, lime, turquoise, cyan, sky, blue, violet, magenta, pink]
