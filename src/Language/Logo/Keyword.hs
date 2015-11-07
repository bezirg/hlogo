{-# LANGUAGE TemplateHaskell, CPP #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-- | 
-- Module      :  Language.Logo.Keyword
-- Copyright   :  (c) 2013-2015, the HLogo team
-- License     :  BSD3
-- Maintainer  :  Nikolaos Bezirgiannis <bezirgia@cwi.nl>
-- Stability   :  experimental
--
-- The module defines the macros of the HLogo language using TemplateHaskell (lisp-like macros).
module Language.Logo.Keyword (
                              -- * Running an HLogo program
                              run, runT,
                              -- * Declaring new breeds
                              breeds,directed_link_breed,undirected_link_breed,
                              -- * User-provided variables
                              globals, patches_own, turtles_own, links_own, breeds_own, link_breeds_own
                             ) where

import Language.Haskell.TH
import Language.Logo.Core
import Language.Logo.Base
import Language.Logo.Prim
import Language.Logo.Exception
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Reader as Reader
import Control.Concurrent.STM
import Data.Array
import Data.List (genericLength)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Typeable (cast)
import Control.Monad (liftM, filterM, replicateM)
import System.Random (randomR, mkStdGen)
import System.IO.Unsafe (unsafePerformIO)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
#ifdef STATS_STM
import Data.IORef (newIORef)
#endif

-- | globals macro takes a list of variable names (as strings) and creates top-level global variables.
-- Global variables have type 'Double' and are initialized to 0.
globals :: [String] -> Q [Dec]
globals vs  = 
              -- create 2 getters (1 prim and 1 unsafe) per global variable
              (liftM concat $ mapM (\ v -> do
                      noInline <- pragInlD (mkName ("__" ++ v)) NoInline FunLike AllPhases                 
                      topLevelVar <- valD (varP (mkName ("__" ++ v)))
                                                   (normalB [| unsafePerformIO $ newTVarIO 0 :: TVar Double |]) []
                      getVarSig <- sigD (mkName v) [t| (STMorIO m) => C m Double|] -- cannot infer the STMOrIO otherwise
                      getVar <- valD (varP (mkName v)) 
                               (normalB [| readGlobal $(varE (mkName ("__" ++v))) |]) []

                      y <- newName "y"
                      setVar <- funD (mkName ("set_" ++ v)) [clause [varP y] 
                                                            (normalB [| lift $ writeTVar $(varE (mkName ("__" ++ v))) $(varE y) |]) []]
                      withVar <- funD (mkName ("with_" ++ v)) [clause [varP y] 
                                                              (normalB [| lift $ modifyTVar' $(varE (mkName ("__" ++ v))) $(varE y) |]) []]

                      return [noInline, topLevelVar, getVar, getVarSig, setVar, withVar]
                    )  vs)



-- | turtles_own macro takes a list of variable names (as strings) and creates corresponding field variables for each __turtle agent__.
-- This user-declared turtle-field variables have type 'Double' and are initialized to 0.
--
-- NB: turtle fields are inherited to all __turtle-like__ agents, i.e. both the turtle agents as well as all any-breed agents.
turtles_own :: [String] -> Q [Dec]
turtles_own vs = do
  tlInline <- pragInlD (mkName "turtles_length") Inline FunLike AllPhases
  tl <- valD (varP (mkName "turtles_length")) (normalB [| $(litE (integerL (genericLength vs))) |]) []
  -- Variables setters/getters
  pg <- mapM (\ (v, i) -> do
          p' <- sigD (mkName v) [t| (STMorIO m) => C m Double|]
          p <- valD (varP (mkName v)) (normalB [| readTurtle $(litE (integerL i)) |]) []
          y <- newName "y"
          w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do
                                                                       (_,a,_,_) <- Reader.ask :: CSTM Context; 
                                                                       case a of
                                                                         TurtleRef _ (MkTurtle {tvars_ = pv}) -> lift $ writeTVar (pv ! $(litE (integerL i))) $(varE y)
                                                                         _ -> throw $ ContextException "turtle" a
                                                                     |]) []]
          x <- funD (mkName ("with_" ++ v)) [clause [varP y] (normalB [| do
                                                                       (_,a,_,_) <- Reader.ask :: CSTM Context; 
                                                                       case a of
                                                                         TurtleRef _ (MkTurtle {tvars_ = pv}) -> lift $ modifyTVar' (pv ! $(litE (integerL i))) $(varE y)
                                                                         _ -> throw $ ContextException "turtle" a
                                                                     |]) []]

          return [p',p,w,x]
            ) (zip vs [0..])
  return $ tlInline: tl : concat pg -- sp : cts : co : crtInline: crt : croInline: cro: concat pg


-- | patches_own macro takes a list of variable names (as strings) and creates corresponding field variables for each __patch agent__.
-- This user-declared patch-field variables have type 'Double' and are initialized to 0.
patches_own :: [String] -> Q [Dec]
patches_own vs = do
  plInline <- pragInlD (mkName "patches_length") Inline FunLike AllPhases
  pl <- valD (varP (mkName "patches_length")) (normalB [| $(litE (integerL (genericLength vs))) |]) []
  -- Variables setters/getters
  pg <- mapM (\ (v, i) -> do
          p' <- sigD (mkName v) [t| (STMorIO m) => C m Double|]
          p <- valD (varP (mkName v)) (normalB [| readPatch $(litE (integerL i)) |]) []
          y <- newName "y"
          w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do 
                                                 (_,a,_,_) <- Reader.ask :: CSTM Context
                                                 case a of
                                                   PatchRef _ (MkPatch {pvars_ = pv}) -> lift $ writeTVar (pv ! $(litE (integerL i))) $(varE y)
                                                   TurtleRef _ _ -> patch_here >>= \ ([PatchRef _ (MkPatch {pvars_ = pv})]) -> lift $ writeTVar (pv ! $(litE (integerL i))) $(varE y)
                                                   _ -> throw $ ContextException "turtle or patch" a
                                                                     |]) []]
          x <- funD (mkName ("with_" ++ v)) [clause [varP y] (normalB [| do 
                                                 (_,a,_,_) <- Reader.ask :: CSTM Context
                                                 case a of
                                                   PatchRef _ (MkPatch {pvars_ = pv}) -> lift $ modifyTVar' (pv ! $(litE (integerL i))) $(varE y)
                                                   TurtleRef _ _ -> patch_here >>= \ ([PatchRef _ (MkPatch {pvars_ = pv})]) -> lift $ modifyTVar' (pv ! $(litE (integerL i))) $(varE y)
                                                   _ -> throw $ ContextException "turtle or patch" a
                                                                     |]) []]
          return [p',p,w,x]
            ) (zip vs [0..])
  return $ plInline: pl : concat pg


-- | links_own macro takes a list of variable names (as strings) and creates corresponding field variables for each __link agent__.
-- This user-declared link-field variables have type 'Double' and are initialized to 0.
--
-- NB: link fields are inherited to all __link-like__ agents, i.e. both the link agents as well as all any-linkbreed agents.
links_own :: [String] -> Q [Dec]
links_own vs = do
  llInline <- pragInlD (mkName "links_length") Inline FunLike AllPhases
  ll <- valD (varP (mkName "links_length")) (normalB [| $(litE (integerL (genericLength vs))) |]) []

  pg <- mapM (\ (v, i) -> do
          p' <- sigD (mkName v) [t| (STMorIO m) => C m Double|]
          p <- valD (varP (mkName v)) (normalB [| readLink $(litE (integerL i))|]) []
          y <- newName "y"
          w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do 
                                                                       (_,a,_,_) <- Reader.ask :: CSTM Context; 
                                                                       case a of
                                                                         LinkRef _ (MkLink {lvars_ = pv}) -> lift $ writeTVar (pv ! $(litE (integerL i))) $(varE y) 
                                                                         _ -> throw $ ContextException "link" a
                                                                     |]) []]
          x <- funD (mkName ("with_" ++ v)) [clause [varP y] (normalB [| do 
                                                                       (_,a,_,_) <- Reader.ask :: CSTM Context; 
                                                                       case a of
                                                                         LinkRef _ (MkLink {lvars_ = pv}) -> lift $ modifyTVar' (pv ! $(litE (integerL i))) $(varE y) 
                                                                         _ -> throw $ ContextException "link" a
                                                                     |]) []]

          return [p',p,w,x]
            ) (zip vs [0..])
  return $ llInline: ll: concat pg


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
                                                           (tw, a, _, _) <- Reader.ask
                                                           case a of
                                                             PatchRef (px,py) _ -> do
                                                                 let  newTurtles w n = return . IM.fromAscList =<< sequence [do
                                                                                                                              t <- newBSprout i $(litE (integerL (genericLength vs))) (fromIntegral px) (fromIntegral py) $(litE (stringL p))
                                                                                                                              return (i, t)
                                                                                                                             | i <- [w..w+n-1]]
                                                                 let addTurtles ts' (MkWorld ps ts ls)  = MkWorld ps (ts `IM.union` ts') ls
                                                                 oldWho <- lift $ readTVar __who
                                                                 lift $ modifyTVar' __who ($(varE y) +)
                                                                 ns <- newTurtles oldWho $(varE y)
                                                                 lift $ modifyTVar' tw (addTurtles ns) 
                                                                 return $ map (uncurry TurtleRef) $ IM.toList ns -- todo: can be optimized
                                                             _ -> throw $ ContextException "patch" a
                                                         |]) []]





  pg <- mapM (\ (v, i) -> do
          p' <- sigD (mkName v) [t| (STMorIO m) => C m Double|]
          p <- valD (varP (mkName v)) (normalB [| readTurtle $(litE (integerL i)) |]) []
          y <- newName "y"
          w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do 
                                                                       (_,a,_,_) <- Reader.ask :: CSTM Context; 
                                                                       case a of
                                                                         TurtleRef _ (MkTurtle {tvars_ = pv}) -> lift $ writeTVar (pv ! $(litE (integerL i))) $(varE y)
                                                                         _ -> throw $ ContextException "turtle" a
                                                                     |]) []]
          x <- funD (mkName ("with_" ++ v)) [clause [varP y] (normalB [| do 
                                                                       (_,a,_,_) <- Reader.ask :: CSTM Context; 
                                                                       case a of
                                                                         TurtleRef _ (MkTurtle {tvars_ = pv}) -> lift $ modifyTVar' (pv ! $(litE (integerL i))) $(varE y)
                                                                         _ -> throw $ ContextException "turtle" a
                                                                     |]) []]

          return [p',p,w,x]
            ) (zip vs [0..])
  return $ cb : sp : cob : concat pg

-- | Like 'breeds_own' but for __linkbreeds__, which are declared with 'directed_link_breed' or 'undirected_link_breed'.
link_breeds_own :: String -> [String] -> Q [Dec]
link_breeds_own p vs = do
  y <- newName "y"
  cls <- funD (mkName $ "create_" ++ p ++ "_with") [clause [varP y] (normalB [| create_breeded_links_with $(litE (stringL p)) $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  cts <- funD (mkName $ "create_" ++ p ++ "_to") [clause [varP y] (normalB [| create_breeded_links_to $(litE (stringL p)) $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  cfs <- funD (mkName $ "create_" ++ p ++ "_from") [clause [varP y] (normalB [| create_breeded_links_from $(litE (stringL p)) $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  pg <- mapM (\ (v, i) -> do
          p' <- sigD (mkName v) [t| (STMorIO m) => C m Double|]
          p <- valD (varP (mkName v)) (normalB [| readLink $(litE (integerL i)) |]) []

          y <- newName "y"
          w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do 
                                                                       (_,a,_,_) <- Reader.ask :: CSTM Context; 
                                                                       case a of
                                                                         LinkRef _ (MkLink {lvars_ = pv}) -> lift $ writeTVar (pv ! $(litE (integerL i))) $(varE y)
                                                                         _ -> throw $ ContextException "link" a
                                                                     |]) []]
          x <- funD (mkName ("with_" ++ v)) [clause [varP y] (normalB [| do 
                                                                       (_,a,_,_) <- Reader.ask :: CSTM Context; 
                                                                       case a of
                                                                         LinkRef _ (MkLink {lvars_ = pv}) -> lift $ modifyTVar' (pv ! $(litE (integerL i))) $(varE y)
                                                                         _ -> throw $ ContextException "link" a
                                                                     |]) []]

          return [p',p,w,x]
            ) (zip vs [0..])
  return $ cls : cts : cfs : concat pg


-- | Takes a 2-length list, with 1st element the plural name of the breeds (e.g. "wolves") and as 2nd element the singlular form (e.g. "wolf").
--
-- NB: breed agents share the same who counter (same namespace). The agentset 'turtles' returns all turtle-like agents (both turtles and any-breeds).
breeds :: [String] -> Q [Dec]
breeds [p,s] = do
  sp <- valD (varP (mkName p)) (normalB [| do 
                                          ts <- turtles; 
                                          filterM (\ (TurtleRef _ (MkTurtle {breed_ = tb})) -> do
                                                     b <- lift $ readTVar tb
                                                     return $ b == $(litE (stringL p))) ts 
                                        |]) []
  up <- valD (varP (mkName ("unsafe_" ++ p))) (normalB [| with (breed >>= \ b -> return (b == $(litE (stringL p)))) =<< turtles |]) []
  x <- newName "x"
  y <- newName "y"
  us <- funD (mkName ("unsafe_" ++ s)) [clause [varP y] 
                                       (normalB [| do 
                                                   (tw,_, _, _) <- Reader.ask :: CIO Context
                                                   (MkWorld _ ts _) <- lift (readTVarIO tw)
                                                   let {t = ts IM.! $(varE y)}
                                                   b <- lift $ readTVarIO (breed_ t)
                                                   return $ if b == $(litE (stringL p)) 
                                                            then [TurtleRef $(varE y) t] 
                                                            else error ("turtle is not a " ++ s) |]) []]

  ss <- funD (mkName s) [clause [varP y] (normalB [| do 
                                                    (tw,_, _, _) <- Reader.ask :: CSTM Context
                                                    (MkWorld _ ts _) <- lift (readTVar tw)
                                                    let {t = ts IM.! $(varE y)}
                                                    b <- lift $ readTVar (breed_ t)
                                                    return $ if b == $(litE (stringL p)) 
                                                             then [TurtleRef $(varE y) t] 
                                                             else error ("turtle is not a " ++ s) |]) []]
  th <- valD (varP (mkName (p ++ "_here"))) (normalB [| do 
                                                       [PatchRef (px,py) _] <- patch_here
                                                       ts <- turtles
                                                       filterM (\ (TurtleRef _ (MkTurtle {xcor_ = x_, ycor_ = y_, breed_ = tb})) -> do 
                                                                  x' <- lift $ readTVar x_
                                                                  y' <- lift $ readTVar y_
                                                                  b <- lift $ readTVar tb
                                                                  return $ round x' == px && round y' == py && b == $(litE (stringL p))) ts |]) []
  uth <- valD (varP (mkName ("unsafe_" ++ p ++ "_here"))) (normalB [| do 
                                                                     (_, a, _,  _) <- Reader.ask
                                                                     h <- case a of
                                                                             TurtleRef _ _ -> patch_here
                                                                             PatchRef _ _ -> return [a]
                                                                             _ -> throw $ ContextException "turtle or patch" a
                                                                     ts <- $(varE (mkName ("unsafe_" ++ p)))
                                                                     with (return . ( == h) =<< patch_here) ts
                                                                   |]) []

  ta <- funD (mkName (p ++ "_at")) [clause [varP x, varP y] (normalB [| do 
                                                                       [PatchRef (px,py) _] <- patch_at $(varE x) $(varE y)
                                                                       ts <- turtles
                                                                       filterM (\ (TurtleRef _ (MkTurtle {xcor_ = x_, ycor_ = y_, breed_ = tb})) -> do 
                                                                                  x' <- lift $ readTVar x_
                                                                                  y' <- lift $ readTVar y_
                                                                                  b <- lift $ readTVar tb
                                                                                  return $ round x' == px && round y' == py && b == $(litE (stringL p))) ts |]) []]
  to <- funD (mkName (p ++ "_on")) [clause [varP y] (normalB [|
                                                              case $(varE y) of
                                                                [] -> return []
                                                                ps@(PatchRef _ _ : _) -> with (liftM (flip elem ps . head) patch_here) =<< $(varE (mkName ("unsafe_" ++ p)))
                                                                ts@(TurtleRef _ _ : _) -> $(varE (mkName (p ++ "_on"))) =<< of_ (liftM head patch_here) ts
                                                                (a:_) -> throw $ ContextException (s ++ "or patch agentset") a
                                                            |]) []]

  ib <- funD (mkName ("is_" ++ s ++ "p")) [clause [varP y] 
                                          (normalB [| maybe (return False) (\ t -> case t of 
                                                                                    [TurtleRef _ (MkTurtle {breed_ = tb})] -> do
                                                                                      b <- lift $ readTVar tb
                                                                                      return $ b == $(litE (stringL p))
                                                                                    _ -> return False) (cast $(varE y) :: Maybe [AgentRef]) |]) []]
  return [sp,up,us,ss,th,uth,ta, to, ib]
breeds _ = fail "Breeds accepts exactly two string arguments, e.g. breeds [\"wolves\", \"wolf\"]"


-- | Creates a directed-linkbreed. The arguments behave the same as 'breeds'. 
directed_link_breed :: [String] -> Q [Dec]
directed_link_breed [p,s] = do
  sp <- valD (varP (mkName p)) (normalB [| do 
                                          ls <- links :: CSTM [AgentRef]
                                          filterM (\ (LinkRef _ (MkLink {lbreed_ = b})) -> return $ b == $(litE (stringL p))) ls |]) []
  x <- newName "x"
  y <- newName "y"
  ss <- funD (mkName s) [clause [varP x, varP y] 
                        (normalB [| do (tw,_, _, _) <- Reader.ask :: CSTM Context; (MkWorld _ _ ls) <- lift $ readTVar tw; return [maybe Nobody (LinkRef ($(varE x),$(varE y))) $M.lookup ($(varE x),$(varE y)) ls] |]) []]
  return [sp, ss]
directed_link_breed _ = fail "Link Breeds accepts exactly two string arguments, e.g. breeds [\"streets\", \"street\"]"

-- | Creates an undirected-linkbreed. The arguments behave the same as 'breeds'. 
undirected_link_breed :: [String] -> Q [Dec]
undirected_link_breed [p,s] = do
  sp <- valD (varP (mkName p)) (normalB [| do 
                                          ls <- links :: CSTM [AgentRef]
                                          filterM (\ (LinkRef _ (MkLink {lbreed_ = b})) -> return $ b == $(litE (stringL p))) ls |]) []
  x <- newName "x"
  y <- newName "y"
  ss <- funD (mkName s) [clause [varP x, varP y] 
                        (normalB [| do (tw,_, _, _) <- Reader.ask :: CSTM Context; (MkWorld _ _ ls) <- lift $ readTVar tw; return [maybe Nobody (LinkRef ($(varE x),$(varE y))) $M.lookup ($(varE x),$(varE y)) ls] |]) []]
  return [sp, ss]
undirected_link_breed _ = fail "Link Breeds accepts exactly two string arguments, e.g. breeds [\"streets\",\"street\"]"


-- | The entrypoint of a non-interactive HLogo. Takes as input a list of HLogo procedure names (as strings) to run in sequence.
-- The program exits either when the last procedure stops, or when a program exception occurs.
--
-- NOTE TO SELF: cannot have 'run' take procedures as values of 'CIO ()', possibly for the same reason as 'breeds_own', i.e. _GHC stage restriction_. 
-- It has to take either a list of strings or a list of symbol names (this was chosen). 
run :: [String] -> Q [Dec]
run procs = do
  let as = map mkName procs
  pl <- lookupValueName "patches_length"
  let plength = case pl of
                  Nothing -> litE (integerL 0)
                  _ -> varE $ mkName "patches_length"
  tl <- lookupValueName "turtles_length"
  let tlength = case tl of
                  Nothing -> litE (integerL 0)
                  _ -> varE $ mkName "turtles_length"

  ll <- lookupValueName "links_length"
  let llength = case ll of
                  Nothing -> litE (integerL 0)
                  _ -> varE $ mkName "links_length"
  

  y <- newName "y"
  cts <- funD (mkName "create_turtles") [clause [varP y] (normalB [| do
                                                                   (tw, a, _, _) <- Reader.ask
                                                                   case a of
                                                                     ObserverRef _ -> return ()
                                                                     _ -> throw $ ContextException "observer" a
                                                                   let  newTurtles w n = return . IM.fromAscList =<< sequence [do
                                                                                                                                 t <- newTurtle i $(tlength)
                                                                                                                                 return (i, t)
                                                                                                                                | i <- [w..w+n-1]]
                                                                   let addTurtles ts' (MkWorld ps ts ls)  = MkWorld ps (ts `IM.union` ts') ls
                                                                   oldWho <- lift $ readTVar __who
                                                                   lift $ modifyTVar' __who ($(varE y) +)
                                                                   ns <- newTurtles oldWho $(varE y)
                                                                   lift $ modifyTVar' tw (addTurtles ns) 
                                                                   return $ map (uncurry TurtleRef) $ IM.toList ns -- todo: can be optimized
                                                                   |]) []]
  sp <- funD (mkName "sprout") [clause [varP y] (normalB [| do
                                                           (tw, a, _, _) <- Reader.ask
                                                           case a of
                                                             PatchRef (px,py) _ -> do
                                                                 let  newTurtles w n = return . IM.fromAscList =<< sequence [do
                                                                                                                              t <- newSprout i $(tlength) (fromIntegral px) (fromIntegral py)
                                                                                                                              return (i, t)
                                                                                                                             | i <- [w..w+n-1]]
                                                                 let addTurtles ts' (MkWorld ps ts ls)  = MkWorld ps (ts `IM.union` ts') ls
                                                                 oldWho <- lift $ readTVar __who
                                                                 lift $ modifyTVar' __who ($(varE y) +)
                                                                 ns <- newTurtles oldWho $(varE y)
                                                                 lift $ modifyTVar' tw (addTurtles ns) 
                                                                 return $ map (uncurry TurtleRef) $ IM.toList ns -- todo: can be optimized
                                                             _ -> throw $ ContextException "patch" a
                                                         |]) []]



  co <- funD (mkName "create_ordered_turtles") [clause [varP y] (normalB [| do
                                                                           (tw, a, _, _) <- Reader.ask
                                                                           case a of
                                                                               ObserverRef _ -> return ()
                                                                               _ -> throw $ ContextException "observer" a
                                                                           let newTurtles w n = return . IM.fromAscList =<< mapM (\ (i,j) -> do
                                                                                                                                        t <- newOrderedTurtle i n j $(tlength)
                                                                                                                                        return (j, t))
                                                                                                                                      (zip [1..n]  [w..w+n-1])
                                                                           let addTurtles ts' (MkWorld ps ts ls)  = MkWorld ps (ts `IM.union` ts') ls
                                                                           lift $ do
                                                                             oldWho <- readTVar __who
                                                                             modifyTVar' __who ($(varE y) +)
                                                                             ns <- newTurtles oldWho $(varE y)
                                                                             modifyTVar' tw (addTurtles ns) 
                                                                             return $ map (uncurry TurtleRef) $ IM.toList ns -- todo: can be optimized

                                                                        |]) []]


  crtInline <- pragInlD (mkName "crt") Inline FunLike AllPhases                 
  crt <- valD (varP (mkName "crt") ) (normalB [| $(varE (mkName "create_turtles")) |]) []
  croInline <- pragInlD (mkName "cro") Inline FunLike AllPhases                 
  cro <- valD (varP (mkName "cro") ) (normalB [| $(varE (mkName "create_ordered_turtles")) :: Int -> CSTM [AgentRef] |]) []

  clsw <- funD (mkName "create_links_with") [clause [varP y] (normalB [| create_links_with_ $(varE y) $(llength) |]) []]
  clst <- funD (mkName "create_links_to") [clause [varP y] (normalB [| create_links_to_ $(varE y) $(llength) |]) []]
  clsf <- funD (mkName "create_links_from") [clause [varP y] (normalB [| create_links_from_ $(varE y) $(llength) |]) []]
  clw <- funD (mkName "create_link_with") [clause [varP y] (normalB [| create_link_with_ $(varE y) $(llength) |]) []]
  clt <- funD (mkName "create_link_to") [clause [varP y] (normalB [| create_link_to_ $(varE y) $(llength) |]) []]
  clf <- funD (mkName "create_link_from") [clause [varP y] (normalB [| create_link_from_ $(varE y) $(llength) |]) []]


  m <- [d| main = do 
                    c <- cInit $(plength)
                    Reader.runReaderT (sequence_ $(listE (map (\ a -> infixE (Just (varE a)) (varE (mkName ">>")) 
                                                                 (Just (appE (varE (mkName "return")) (conE (mkName "()"))))) as))) c 
      |]
  return $ cts: sp: co: crtInline: crt: croInline: cro: clsw: clst: clsf: clw: clt: clf: m
                 
-- | Internal, used only in Test code.
runT :: CIO b -> IO b
runT as = do c <- cInit 0
             Reader.runReaderT as c
             

-- | Internal
random_primary_color :: CSTM Double
random_primary_color = do
  (_,s,_,_) <- Reader.ask
  let ts = case s of
            ObserverRef tg -> tg
            TurtleRef _ t -> tgen t
            PatchRef _ p -> pgen p
            LinkRef _ l -> lgen l
            Nobody -> throw DevException
  gen <- lift $ readTVar ts
  let (v,gen') = randomR (0,13 :: Int) gen
  lift $ writeTVar ts gen'
  return (primary_colors !! v)

-- | Internal
random_integer_heading :: CSTM Integer
random_integer_heading = do
  (_,s,_,_) <- Reader.ask
  let ts = case s of
            ObserverRef tg -> tg
            TurtleRef _ t -> tgen t
            PatchRef _ p -> pgen p
            LinkRef _ l -> lgen l
            Nobody -> throw DevException
  gen <- lift $ readTVar ts
  let (v,gen') = randomR (0,360 :: Integer) gen
  lift $ writeTVar ts gen'
  return v

-- | Internal
newBreed :: String -> Int -> Int -> CSTM Turtle
newBreed b x to = do
  rpc <- random_primary_color
  rih <- random_integer_heading
  lift $ MkTurtle <$>
       return x <*>
       newTVar b <*>
       newTVar rpc <*>          --  random primary color
       newTVar (fromInteger rih) <*> --  random integer heading
       newTVar 0 <*>
       newTVar 0 <*>
       newTVar "default" <*>
       newTVar "" <*>
       newTVar 9.9 <*>
       newTVar False <*>
       newTVar 1 <*>
       newTVar 1 <*>
       newTVar Up <*>
       (return . listArray (0, fromIntegral to -1) =<< replicateM (fromIntegral to) (newTVar 0)) <*>
       newTVar (mkStdGen x) <*>
       pure 0 <*>
       pure 0
#ifdef STATS_STM
       <*> pure (unsafePerformIO (newIORef 0)) <*>
       pure (unsafePerformIO (newIORef 0))
#endif

-- | Internal
newOrderedBreed :: Int -> Int -> String -> Int -> Int -> STM Turtle -- ^ Index -> Order -> Breed -> Who -> VarLength -> CSTM Turtle
newOrderedBreed i o b x to = do
  let rpc = primary_colors !! ((i-1) `mod` 14)
  let rdh = ((toEnum i-1) / toEnum o) * 360 :: Double
  MkTurtle <$>
       return x <*>
       newTVar b <*>
       newTVar rpc <*>          --  ordered primary color
       newTVar rdh <*> --  ordered double heading
       newTVar 0 <*>
       newTVar 0 <*>
       newTVar "default" <*>
       newTVar "" <*>
       newTVar 9.9 <*>
       newTVar False <*>
       newTVar 1 <*>
       newTVar 1 <*>
       newTVar Up <*>
       (return . listArray (0, fromIntegral to -1) =<< replicateM (fromIntegral to) (newTVar 0)) <*>
       newTVar (mkStdGen x) <*>
       pure 0 <*>
       pure 0
#ifdef STATS_STM
       <*> pure (unsafePerformIO (newIORef 0)) <*>
       pure (unsafePerformIO (newIORef 0))
#endif


-- | Internal
newTurtle :: Integral a => Int -> a -> CSTM Turtle
newTurtle x to = do
  rpc <- random_primary_color
  rih <- random_integer_heading
  lift $ MkTurtle <$>
       return x <*>
       newTVar "turtles" <*>
       newTVar rpc <*>          --  random primary color
       newTVar (fromInteger rih) <*> --  random integer heading
       newTVar 0 <*>
       newTVar 0 <*>
       newTVar "default" <*>
       newTVar "" <*>
       newTVar 9.9 <*>
       newTVar False <*>
       newTVar 1 <*>
       newTVar 1 <*>
       newTVar Up <*>
       (return . listArray (0, fromIntegral to -1) =<< replicateM (fromIntegral to) (newTVar 0)) <*>
       newTVar (mkStdGen x) <*>
       pure 0 <*>
       pure 0
#ifdef STATS_STM
       <*> pure (unsafePerformIO (newIORef 0)) <*>
       pure (unsafePerformIO (newIORef 0))
#endif

-- | Internal
newSprout :: Integral a => Int -> a -> Double -> Double -> CSTM Turtle
newSprout w to x y = do
  rpc <- random_primary_color
  rih <- random_integer_heading
  lift $ MkTurtle <$>
       return w <*>
       newTVar "turtles" <*>
       newTVar rpc <*>          --  random primary color
       newTVar (fromInteger rih) <*> --  random integer heading
       newTVar x <*>
       newTVar y <*>
       newTVar "default" <*>
       newTVar "" <*>
       newTVar 9.9 <*>
       newTVar False <*>
       newTVar 1 <*>
       newTVar 1 <*>
       newTVar Up <*>
       (return . listArray (0, fromIntegral to -1) =<< replicateM (fromIntegral to) (newTVar 0)) <*>
       newTVar (mkStdGen w) <*>
       pure (round x) <*>
       pure (round y)
#ifdef STATS_STM
       <*> pure (unsafePerformIO (newIORef 0)) <*>
       pure (unsafePerformIO (newIORef 0))
#endif

-- | Internal
newBSprout :: Integral a => Int -> a -> Double -> Double -> String -> CSTM Turtle
newBSprout w to x y b = do
  rpc <- random_primary_color
  rih <- random_integer_heading
  lift $ MkTurtle <$>
       return w <*>
       newTVar b <*>
       newTVar rpc <*>          --  random primary color
       newTVar (fromInteger rih) <*> --  random integer heading
       newTVar x <*>
       newTVar y <*>
       newTVar "default" <*>
       newTVar "" <*>
       newTVar 9.9 <*>
       newTVar False <*>
       newTVar 1 <*>
       newTVar 1 <*>
       newTVar Up <*>
       (return . listArray (0, fromIntegral to -1) =<< replicateM (fromIntegral to) (newTVar 0)) <*>
       newTVar (mkStdGen w) <*>
       pure (round x) <*>
       pure (round y)
#ifdef STATS_STM
       <*> pure (unsafePerformIO (newIORef 0)) <*>
       pure (unsafePerformIO (newIORef 0))
#endif



-- | Internal
newOrderedTurtle :: Int -> Int -> Int -> Int -> STM Turtle -- ^ Index -> Order -> Who -> VarLength -> CSTM Turtle
newOrderedTurtle i o x to = do
    let rpc = primary_colors !! ((i-1) `mod` 14)
    let rdh = ((toEnum i-1) / toEnum o) * 360 :: Double
    MkTurtle <$>
             return x <*>
             newTVar "turtles" <*>
             newTVar rpc <*>          --  ordered primary color
             newTVar rdh <*> --  ordered double heading
             newTVar 0 <*>
             newTVar 0 <*>
             newTVar "default" <*>
             newTVar "" <*>
             newTVar 9.9 <*>
             newTVar False <*>
             newTVar 1 <*>
             newTVar 1 <*>
             newTVar Up <*>
             (return . listArray (0, fromIntegral to -1) =<< replicateM (fromIntegral to) (newTVar 0)) <*>
             newTVar (mkStdGen x) <*>
             pure 0 <*>
             pure 0
#ifdef STATS_STM
             <*> pure (unsafePerformIO (newIORef 0)) <*>
             pure (unsafePerformIO (newIORef 0))
#endif


-- | Internal, Utility function to make TemplateHaskell easier
create_breeds :: String -> Int -> Int -> CSTM [AgentRef] -- ^ Breed -> Size -> VarLength -> CSTM BreededTurtles
create_breeds b n to = do
  (tw, a, _, _) <- Reader.ask
  -- context checking
  case a of
    ObserverRef _ -> return ()
    _ -> throw $ ContextException "observer" a
  oldWho <- lift $ readTVar __who
  lift $ modifyTVar' __who (n +)
  ns <- newBreeds oldWho
  lift $ modifyTVar' tw (addTurtles ns) 
  return $ map (uncurry TurtleRef) $ IM.toList ns -- todo: can be optimized
        where
                      newBreeds w = return . IM.fromAscList =<< sequence [do
                                                                              t <- newBreed b i to
                                                                              return (i, t)
                                                                             | i <- [w..w+n-1]]
                      addTurtles ts' (MkWorld ps ts ls)  = MkWorld ps (ts `IM.union` ts') ls

-- | Internal, Utility function to make TemplateHaskell easier
create_ordered_breeds :: String -> Int -> Int -> CSTM [AgentRef]  -- ^ Breed -> Size -> VarLength -> CSTM BreededTurtles
create_ordered_breeds b n to = do
  (tw, a, _, _) <- Reader.ask
  -- context checking
  case a of
    ObserverRef _ -> return ()
    _ -> throw $ ContextException "observer" a
  lift $ do
    oldWho <- readTVar __who
    modifyTVar' __who (n +)
    ns <- newTurtles oldWho
    modifyTVar' tw (addTurtles ns) 
    return $ map (uncurry TurtleRef) $ IM.toList ns -- todo: can be optimized
        where
                      newTurtles w = return . IM.fromAscList =<< mapM (\ (i,j) -> do
                                                                              t <- newOrderedBreed i n b j to
                                                                              return (j, t))
                                                                             (zip [1..n] [w..w+n-1])
                      addTurtles ts' (MkWorld ps ts ls)  = MkWorld ps (ts `IM.union` ts') ls



-- Links
-------


{-# WARNING newLink "TODO: determine at run-time the direction of links" #-}
-- | Internal
-- links directed by default, that makes create-link(s)-with faulty
newLink :: Int -> Int -> Int -> CSTM Link -- ^ FromIndex -> ToIndex -> VarLength -> CSTM Link
newLink f t ls = lift $ MkLink <$>
                  return f <*>
                  return t <*>
                  return True <*>
                  newTVar 5 <*>
                  newTVar "" <*>
                  newTVar 9.9 <*>
                  newTVar False <*>
                  return "links" <*>
                  newTVar 0 <*>
                  newTVar "default" <*>
                  newTVar None <*>
                  (return . listArray (0, fromIntegral ls -1) =<< replicateM (fromIntegral ls) (newTVar 0))  <*>
                  newTVar (mkStdGen (f+t*1000))
#ifdef STATS_STM
                  <*> pure (unsafePerformIO (newIORef 0)) <*>
                  pure (unsafePerformIO (newIORef 0))
#endif


-- | Internal
newLBreed :: Int -> Int -> Bool -> String -> Int -> CSTM Link -- ^ FromIndex -> ToIndex -> Directed-> Breed -> VarLength -> CSTM Link
newLBreed f t d b ls = lift $ MkLink <$>
                  return f <*>
                  return t <*>
                  return d <*>
                  newTVar 5 <*>
                  newTVar "" <*>
                  newTVar 9.9 <*>
                  newTVar False <*>
                  return b <*>
                  newTVar 0 <*>
                  newTVar "default" <*>
                  newTVar None <*>
                  (return . listArray (0, fromIntegral ls -1) =<< replicateM (fromIntegral ls) (newTVar 0)) <*>
                  newTVar (mkStdGen (f+t*1000))
#ifdef STATS_STM
                  <*> pure (unsafePerformIO (newIORef 0)) <*>
                  pure (unsafePerformIO (newIORef 0))
#endif



-- |  Used for creating breeded and unbreeded links between turtles.
-- create-link-with creates an undirected link between the caller and agent. create-link-to creates a directed link from the caller to agent. create-link-from creates a directed link from agent to the caller.
-- When the plural form of the breed name is used, an agentset is expected instead of an agent and links are created between the caller and all agents in the agentset. 
create_link_from_ :: [AgentRef] -> Int -> CSTM ()
create_link_from_ f ls = case f of
                       [_] -> create_links_from_ f ls
                       _ -> throw $ TypeException "singleton agentset" Nobody
-- |  Used for creating breeded and unbreeded links between turtles.
-- create-link-with creates an undirected link between the caller and agent. create-link-to creates a directed link from the caller to agent. create-link-from creates a directed link from agent to the caller.
-- When the plural form of the breed name is used, an agentset is expected instead of an agent and links are created between the caller and all agents in the agentset. 
create_links_from_ :: [AgentRef] -> Int -> CSTM ()
create_links_from_ as nls = do
  (tw, a, _,_) <- Reader.ask
  case a of
    TurtleRef x _ -> do
           (MkWorld ps ts ls) <- lift $ readTVar tw
           ls' <- foldr (=<<) (return ls) [ insertLink f x | (TurtleRef f _) <- as]
           lift $ writeTVar tw (MkWorld ps ts ls')
    _ -> throw $ ContextException "turtle" a
    where insertLink f x s =  do
                       n <- newLink f x nls
                       return $ M.insertWith (flip const) (f,x) n s
-- |  Used for creating breeded and unbreeded links between turtles.
-- create-link-with creates an undirected link between the caller and agent. create-link-to creates a directed link from the caller to agent. create-link-from creates a directed link from agent to the caller.
-- When the plural form of the breed name is used, an agentset is expected instead of an agent and links are created between the caller and all agents in the agentset. 
create_link_to_ :: [AgentRef] -> Int -> CSTM ()
create_link_to_ t ls = case t of
                     [_] -> create_links_to_ t ls
                     _ -> throw $ TypeException "singleton agentset" Nobody

-- |  Used for creating breeded and unbreeded links between turtles.
-- create-link-with creates an undirected link between the caller and agent. create-link-to creates a directed link from the caller to agent. create-link-from creates a directed link from agent to the caller.
-- When the plural form of the breed name is used, an agentset is expected instead of an agent and links are created between the caller and all agents in the agentset. 
create_links_to_ :: [AgentRef] -> Int -> CSTM ()
create_links_to_ as nls = do
  (tw, a, _, _) <- Reader.ask
  case a of
    TurtleRef x _ -> do
           (MkWorld ps ts ls) <- lift $ readTVar tw
           ls' <- foldr (=<<) (return ls) [ insertLink t x | (TurtleRef t _) <- as]
           lift $ writeTVar tw (MkWorld ps ts ls')
    _ -> throw $ ContextException "turtle" a
    where insertLink t x s =  do
                       n <- newLink x t nls
                       return $ M.insertWith (flip const) (x,t) n s
                                   

-- |  Used for creating breeded and unbreeded links between turtles.
-- create-link-with creates an undirected link between the caller and agent. create-link-to creates a directed link from the caller to agent. create-link-from creates a directed link from agent to the caller.
-- When the plural form of the breed name is used, an agentset is expected instead of an agent and links are created between the caller and all agents in the agentset. 
create_link_with_ :: [AgentRef] -> Int -> CSTM ()
create_link_with_ w ls = case w of
                     [_] -> create_links_with_ w ls
                     _ -> throw $ TypeException "singleton agentset" Nobody

-- |  Used for creating breeded and unbreeded links between turtles.
-- create-link-with creates an undirected link between the caller and agent. create-link-to creates a directed link from the caller to agent. create-link-from creates a directed link from agent to the caller.
-- When the plural form of the breed name is used, an agentset is expected instead of an agent and links are created between the caller and all agents in the agentset. 
create_links_with_ :: [AgentRef] -> Int -> CSTM ()
create_links_with_ as nls =  do
  (tw, a, _, _) <- Reader.ask
  case a of
    TurtleRef x _ -> do
           (MkWorld ps ts ls) <- lift $ readTVar tw
           ls' <- foldr (=<<) (return ls) [ insertLink t x  | (TurtleRef t _) <- as]
           lift $ writeTVar tw (MkWorld ps ts ls')
    _ -> throw $ ContextException "turtle" a
    where insertLink t x s =  do
                       n <- newLBreed x t False "links" nls
                       return $ M.insertWith (flip const) (t,x) n $ M.insertWith (flip const) (x,t) n s

-- | Internal, Utility function to make TemplateHaskell easier
create_breeded_links_to :: String -> [AgentRef] -> Int -> CSTM ()
create_breeded_links_to b as nls = do
  (tw, a, _, _) <- Reader.ask
  case a of
    TurtleRef x _  -> do
           (MkWorld ps ts ls) <- lift $ readTVar tw
           ls' <- foldr (=<<) (return ls) [ insertLink t x | (TurtleRef t _) <- as]
           lift $ writeTVar tw (MkWorld ps ts ls')
    _ -> throw $ ContextException "turtle" a
    where insertLink t x s =  do
                       n <- newLBreed x t True b nls
                       return $ M.insertWith (flip const) (x,t) n s


-- | Internal, Utility function to make TemplateHaskell easier
create_breeded_links_from :: String -> [AgentRef] -> Int -> CSTM ()
create_breeded_links_from b as nls = do
  (tw, a, _, _) <- Reader.ask
  case a of
    TurtleRef x _ -> do
           (MkWorld ps ts ls) <- lift $ readTVar tw
           ls' <- foldr (=<<) (return ls) [ insertLink f x | (TurtleRef f _) <- as]
           lift $ writeTVar tw (MkWorld ps ts ls')
    _ -> throw $ ContextException "turtle" a
    where insertLink f x s =  do
                       n <- newLBreed f x True b nls
                       return $ M.insertWith (flip const) (f,x) n s

-- | Internal, Utility function to make TemplateHaskell easier
create_breeded_links_with :: String -> [AgentRef] -> Int -> CSTM ()
create_breeded_links_with b as nls =  do
  (tw, a, _, _) <- Reader.ask
  case a of
    TurtleRef x _ -> do
           (MkWorld ps ts ls) <- lift $ readTVar tw
           ls' <- foldr (=<<) (return ls) [ insertLink t x  | (TurtleRef t _) <- as]
           lift $ writeTVar tw (MkWorld ps ts ls')
    _ -> throw $ ContextException "turtle" a
    where insertLink t x s =  do
                       n <- newLBreed x t False b nls
                       return $ M.insertWith (flip const) (t,x) n $ M.insertWith (flip const) (x,t) n s

-- | Internal
{-# INLINE primary_colors #-}
primary_colors :: [Double]
primary_colors = [gray, red, orange, brown, yellow, green, lime, turquoise, cyan, sky, blue, violet, magenta, pink]

