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
module Language.Logo.Keyword where

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
import Control.Monad (liftM, liftM2, filterM, replicateM)
import System.Random (randomR, mkStdGen)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

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



turtles_own :: [String] -> Q [Dec]
turtles_own vs = do
  y <- newName "y"
  ct <- funD (mkName "create_turtles") [clause [varP y] (normalB [| do
                                                                   (tw, a, _, _) <- Reader.ask
                                                                   case a of
                                                                     ObserverRef _ -> return ()
                                                                     _ -> throw $ ContextException "observer" a
                                                                   let  newTurtles w n = return . IM.fromAscList =<< sequence [do
                                                                                                                                 t <- newTurtle i $(litE (integerL (genericLength vs)))
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
                                                                                                                              t <- newSprout i $(litE (integerL (genericLength vs))) (fromIntegral px) (fromIntegral py)
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
                                                                                                                                        t <- newOrderedTurtle i n j $(litE (integerL (genericLength vs)))
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


  crt <- valD (varP (mkName "crt") ) (normalB [| $(varE (mkName "create_turtles")) |]) []
  cro <- valD (varP (mkName "cro") ) (normalB [| $(varE (mkName "create_ordered_turtles")) :: Int -> CSTM [AgentRef] |]) []
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
  return $ sp : ct : co : crt : cro: concat pg

patches_own :: [String] -> Q [Dec]
patches_own vs = do
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
  return $ pl : concat pg


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
                                                       [s] <- self
                                                       [PatchRef (px,py) _] <- patch_here
                                                       ts <- turtles
                                                       filterM (\ (TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y, breed_ = tb})) -> do 
                                                                  x' <- lift $ readTVar x
                                                                  y' <- lift $ readTVar y
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
                                                                       [s] <- self; 
                                                                       [PatchRef (px,py) _] <- patch_at $(varE x) $(varE y)
                                                                       ts <- turtles
                                                                       filterM (\ (TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y, breed_ = tb})) -> do 
                                                                                  x' <- lift $ readTVar x
                                                                                  y' <- lift $ readTVar y
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

links_own :: [String] -> Q [Dec]
links_own vs = do
  y <- newName "y"
  cls <- funD (mkName "create_links_with") [clause [varP y] (normalB [| create_links_with_ $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  cts <- funD (mkName "create_links_to") [clause [varP y] (normalB [| create_links_to_ $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  cfs <- funD (mkName "create_links_from") [clause [varP y] (normalB [| create_links_from_ $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  cl <- funD (mkName "create_link_with") [clause [varP y] (normalB [| create_link_with_ $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  ct <- funD (mkName "create_link_to") [clause [varP y] (normalB [| create_link_to_ $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  cf <- funD (mkName "create_link_from") [clause [varP y] (normalB [| create_link_from_ $(varE y) $(litE (integerL (genericLength vs))) |]) []]
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
  return $ cls : cts : cfs : cl : ct : cf : concat pg

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


run :: [Name] -> DecsQ
run as = [d| main = do 
                    c <- cInit $(varE (mkName "patches_length"))
                    Reader.runReaderT (foldl1 (>>) $(listE (map (\ a -> infixE (Just (varE a)) (varE (mkName ">>")) 
                                                                 (Just (appE (varE (mkName "return")) (conE (mkName "()"))))) as))) c 
         |]

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
       pure (unsafePerformIO (newIORef 0)) <*>
       pure (unsafePerformIO (newIORef 0)) <*>
       pure 0 <*>
       pure 0


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
       pure (unsafePerformIO (newIORef 0)) <*>
       pure (unsafePerformIO (newIORef 0)) <*>
       pure 0 <*>
       pure 0


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
       pure (unsafePerformIO (newIORef 0)) <*>
       pure (unsafePerformIO (newIORef 0)) <*>
       pure 0 <*>
       pure 0

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
       pure (unsafePerformIO (newIORef 0)) <*>
       pure (unsafePerformIO (newIORef 0)) <*>
       pure (round x) <*>
       pure (round y)

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
       pure (unsafePerformIO (newIORef 0)) <*>
       pure (unsafePerformIO (newIORef 0)) <*>
       pure (round x) <*>
       pure (round y)


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
             pure (unsafePerformIO (newIORef 0)) <*>
             pure (unsafePerformIO (newIORef 0)) <*>
             pure 0 <*>
             pure 0



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
  ns <- newBreeds oldWho n
  lift $ modifyTVar' tw (addTurtles ns) 
  return $ map (uncurry TurtleRef) $ IM.toList ns -- todo: can be optimized
        where
                      newBreeds w n = return . IM.fromAscList =<< sequence [do
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
    ns <- newTurtles oldWho n
    modifyTVar' tw (addTurtles ns) 
    return $ map (uncurry TurtleRef) $ IM.toList ns -- todo: can be optimized
        where
                      newTurtles w n = return . IM.fromAscList =<< mapM (\ (i,j) -> do
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
                  newTVar (mkStdGen (f+t*1000)) <*>
                  pure (unsafePerformIO (newIORef 0)) <*>
                  pure (unsafePerformIO (newIORef 0))


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
                  newTVar (mkStdGen (f+t*1000)) <*>
                  pure (unsafePerformIO (newIORef 0)) <*>
                  pure (unsafePerformIO (newIORef 0))



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
create_links_from_ as ls = do
  (tw, a, _,_) <- Reader.ask
  case a of
    TurtleRef x _ -> do
           (MkWorld ps ts ls) <- lift $ readTVar tw
           ls' <- foldr (=<<) (return ls) [ insertLink f x | (TurtleRef f _) <- as]
           lift $ writeTVar tw (MkWorld ps ts ls')
    _ -> throw $ ContextException "turtle" a
    where insertLink f x s =  do
                       n <- newLink f x ls
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
create_links_to_ as ls = do
  (tw, a, _, _) <- Reader.ask
  case a of
    TurtleRef x _ -> do
           (MkWorld ps ts ls) <- lift $ readTVar tw
           ls' <- foldr (=<<) (return ls) [ insertLink t x | (TurtleRef t _) <- as]
           lift $ writeTVar tw (MkWorld ps ts ls')
    _ -> throw $ ContextException "turtle" a
    where insertLink t x s =  do
                       n <- newLink x t ls
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
create_links_with_ as ls =  do
  (tw, a, _, _) <- Reader.ask
  case a of
    TurtleRef x _ -> do
           (MkWorld ps ts ls) <- lift $ readTVar tw
           ls' <- foldr (=<<) (return ls) [ insertLink t x  | (TurtleRef t _) <- as]
           lift $ writeTVar tw (MkWorld ps ts ls')
    _ -> throw $ ContextException "turtle" a
    where insertLink t x s =  do
                       n <- newLBreed x t False "links" ls
                       return $ M.insertWith (flip const) (t,x) n $ M.insertWith (flip const) (x,t) n s

-- | Internal, Utility function to make TemplateHaskell easier
create_breeded_links_to :: String -> [AgentRef] -> Int -> CSTM ()
create_breeded_links_to b as ls = do
  (tw, a, _, _) <- Reader.ask
  case a of
    TurtleRef x _  -> do
           (MkWorld ps ts ls) <- lift $ readTVar tw
           ls' <- foldr (=<<) (return ls) [ insertLink t x | (TurtleRef t _) <- as]
           lift $ writeTVar tw (MkWorld ps ts ls')
    _ -> throw $ ContextException "turtle" a
    where insertLink t x s =  do
                       n <- newLBreed x t True b ls
                       return $ M.insertWith (flip const) (x,t) n s


-- | Internal, Utility function to make TemplateHaskell easier
create_breeded_links_from :: String -> [AgentRef] -> Int -> CSTM ()
create_breeded_links_from b as ls = do
  (tw, a, _, _) <- Reader.ask
  case a of
    TurtleRef x _ -> do
           (MkWorld ps ts ls) <- lift $ readTVar tw
           ls' <- foldr (=<<) (return ls) [ insertLink f x | (TurtleRef f _) <- as]
           lift $ writeTVar tw (MkWorld ps ts ls')
    _ -> throw $ ContextException "turtle" a
    where insertLink f x s =  do
                       n <- newLBreed f x True b ls
                       return $ M.insertWith (flip const) (f,x) n s

-- | Internal, Utility function to make TemplateHaskell easier
create_breeded_links_with :: String -> [AgentRef] -> Int -> CSTM ()
create_breeded_links_with b as ls =  do
  (tw, a, _, _) <- Reader.ask
  case a of
    TurtleRef x _ -> do
           (MkWorld ps ts ls) <- lift $ readTVar tw
           ls' <- foldr (=<<) (return ls) [ insertLink t x  | (TurtleRef t _) <- as]
           lift $ writeTVar tw (MkWorld ps ts ls')
    _ -> throw $ ContextException "turtle" a
    where insertLink t x s =  do
                       n <- newLBreed x t False b ls
                       return $ M.insertWith (flip const) (t,x) n $ M.insertWith (flip const) (x,t) n s

