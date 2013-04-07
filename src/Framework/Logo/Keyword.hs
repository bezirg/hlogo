{-# LANGUAGE TemplateHaskell #-}
-- | The module defines the macros of the HLogo language using TemplateHaskell (lisp-like macros).
module Framework.Logo.Keyword where

import Language.Haskell.TH
import Framework.Logo.Core
import Framework.Logo.Base
import Framework.Logo.Prim
import Framework.Logo.Exception
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad (liftM)
import Control.Concurrent.STM
import Control.Applicative
import System.Random (randomR)
import Data.Array
import Data.List (genericLength)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Maybe (maybe)
import Data.Typeable (cast)
import Control.Monad (liftM2, filterM, replicateM)

globals vs  = liftM2 (++) 
              -- trick to store the length of globals
              [d| globals_length = $(litE (integerL (genericLength vs))) |] 
              -- create 2 getters (1 prim and 1 unsafe) per global variable
              (liftM concat $ mapM (\ (v,i) -> do
                      p <- valD (varP (mkName v)) (normalB [| do (gs,_,_,_,_,_) <- ask :: CSTM Context; lift $ readTVar (gs ! $(litE (integerL i))) |]) []
                      u <- valD (varP (mkName ("unsafe_" ++ v))) (normalB [| do (gs,_,_,_,_,_) <- ask :: CIO Context; lift $ readTVarIO (gs ! $(litE (integerL i))) |]) []
                      y <- newName "y"
                      w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do (gs,_,_,_,_,_) <- ask :: CSTM Context; lift $ writeTVar (gs ! $(litE (integerL i))) $(varE y) |]) []]
                      x <- funD (mkName ("with_" ++ v)) [clause [varP y] (normalB [| do (gs,_,_,_,_,_) <- ask :: CSTM Context; lift $ modifyTVar' (gs ! $(litE (integerL i))) $(varE y) |]) []]


                      return [p,u,w,x]
                    )  (zip vs [2..]))



turtles_own vs = do
  y <- newName "y"
  ct <- funD (mkName "create_turtles") [clause [varP y] (normalB [| do
                                                                   (gs, tw, a, _, _,_) <- ask
                                                                   case a of
                                                                     ObserverRef -> return ()
                                                                     _ -> throw $ ContextException "observer" a
                                                                   let who = gs ! 0
                                                                   let  newTurtles w n = return . IM.fromAscList =<< sequence [do
                                                                                                                                 t <- newTurtle i $(litE (integerL (genericLength vs)))
                                                                                                                                 return (i, t)
                                                                                                                                | i <- [w..w+n-1]]
                                                                   let addTurtles ts' (MkWorld ps ts ls)  = MkWorld ps (ts `IM.union` ts') ls
                                                                   oldWho <- lift $ liftM round $ readTVar who
                                                                   lift $ modifyTVar' who (\ ow -> fromIntegral $(varE y) + ow)
                                                                   ns <- newTurtles oldWho $(varE y)
                                                                   lift $ modifyTVar' tw (addTurtles ns) 
                                                                   return $ map (uncurry TurtleRef) $ IM.toList ns -- todo: can be optimized
                                                                   |]) []]
  sp <- funD (mkName "sprout") [clause [varP y] (normalB [| do
                                                           (gs, tw, a, _, _,_) <- ask
                                                           case a of
                                                             PatchRef (px,py) _ -> do
                                                                 let who = gs ! 0
                                                                 let  newTurtles w n = return . IM.fromAscList =<< sequence [do
                                                                                                                              t <- newSprout i $(litE (integerL (genericLength vs))) (fromIntegral px) (fromIntegral py)
                                                                                                                              return (i, t)
                                                                                                                             | i <- [w..w+n-1]]
                                                                 let addTurtles ts' (MkWorld ps ts ls)  = MkWorld ps (ts `IM.union` ts') ls
                                                                 oldWho <- lift $ liftM round $ readTVar who
                                                                 lift $ modifyTVar' who (\ ow -> fromIntegral $(varE y) + ow)
                                                                 ns <- newTurtles oldWho $(varE y)
                                                                 lift $ modifyTVar' tw (addTurtles ns) 
                                                                 return $ map (uncurry TurtleRef) $ IM.toList ns -- todo: can be optimized
                                                             _ -> throw $ ContextException "patch" a
                                                         |]) []]



  co <- funD (mkName "create_ordered_turtles") [clause [varP y] (normalB [| do
                                                                           (gs, tw, a, _, _,_) <- ask
                                                                           case a of
                                                                               ObserverRef -> return ()
                                                                               _ -> throw $ ContextException "observer" a
                                                                           let who = gs ! 0
                                                                           let newTurtles w n = return . IM.fromAscList =<< mapM (\ (i,j) -> do
                                                                                                                                        t <- newOrderedTurtle i n j $(litE (integerL (genericLength vs)))
                                                                                                                                        return (j, t))
                                                                                                                                      (zip [1..n]  [w..w+n-1])
                                                                           let addTurtles ts' (MkWorld ps ts ls)  = MkWorld ps (ts `IM.union` ts') ls
                                                                           lift $ do
                                                                             oldWho <- liftM round $ readTVar who
                                                                             modifyTVar' who (\ ow -> fromIntegral $(varE y) + ow)
                                                                             ns <- newTurtles oldWho $(varE y)
                                                                             modifyTVar' tw (addTurtles ns) 
                                                                             return $ map (uncurry TurtleRef) $ IM.toList ns -- todo: can be optimized

                                                                        |]) []]


  crt <- valD (varP (mkName "crt") ) (normalB [| $(varE (mkName "create_turtles")) |]) []
  cro <- valD (varP (mkName "cro") ) (normalB [| $(varE (mkName "create_ordered_turtles")) :: Int -> CSTM [AgentRef] |]) []
  -- Variables setters/getters
  pg <- mapM (\ (v, i) -> do
          p <- valD (varP (mkName v)) (normalB [| do 
                                                 (_,_,a ,_,_,_) <- ask :: CSTM Context; 
                                                 case a of
                                                   TurtleRef _ (MkTurtle {tvars_ = pv}) -> lift $ readTVar (pv ! $(litE (integerL i)))
                                                   _ -> throw $ ContextException "turtle" a
                                               |]) []
          u <- valD (varP (mkName ("unsafe_" ++ v))) (normalB [| do
                                                                (_,_,a ,_,_,_) <- ask :: CIO Context;
                                                                case a of
                                                                  TurtleRef _ (MkTurtle {tvars_ = pv}) -> lift $ readTVarIO (pv ! $(litE (integerL i)))
                                                                  _ -> throw $ ContextException "turtle" a
                                                              |]) []
          y <- newName "y"
          w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do
                                                                       (_,_,a,_,_,_) <- ask :: CSTM Context; 
                                                                       case a of
                                                                         TurtleRef _ (MkTurtle {tvars_ = pv}) -> lift $ writeTVar (pv ! $(litE (integerL i))) $(varE y)
                                                                         _ -> throw $ ContextException "turtle" a
                                                                     |]) []]
          x <- funD (mkName ("with_" ++ v)) [clause [varP y] (normalB [| do
                                                                       (_,_,a,_,_,_) <- ask :: CSTM Context; 
                                                                       case a of
                                                                         TurtleRef _ (MkTurtle {tvars_ = pv}) -> lift $ modifyTVar' (pv ! $(litE (integerL i))) $(varE y)
                                                                         _ -> throw $ ContextException "turtle" a
                                                                     |]) []]

          return [p,u,w,x]
            ) (zip vs [0..])
  return $ sp : ct : co : crt : cro: concat pg

patches_own vs = do
  pl <- valD (varP (mkName "patches_length")) (normalB [| $(litE (integerL (genericLength vs))) |]) []
  -- Variables setters/getters
  pg <- mapM (\ (v, i) -> do
          p <- valD (varP (mkName v)) (normalB [| do 
                                                 (_,_,a,_,_,_) <- ask :: CSTM Context
                                                 case a of
                                                   PatchRef _ (MkPatch {pvars_ = pv}) -> lift $ readTVar $ pv ! $(litE (integerL i))
                                                   TurtleRef _ _ -> patch_here >>= \ ([PatchRef _ (MkPatch {pvars_ = pv})]) -> lift $ readTVar $ pv ! $(litE (integerL i)) 
                                                   _ -> throw $ ContextException "turtle or patch" a
                                               |]) []

          u <- valD (varP (mkName ("unsafe_" ++ v))) (normalB [| do
                                                 (_,_,a,_,_,_) <- ask :: CIO Context
                                                 case a of
                                                   PatchRef _ (MkPatch {pvars_ = pv}) -> lift $ readTVarIO $ pv ! $(litE (integerL i))
                                                   TurtleRef _ _ -> unsafe_patch_here >>= \ ([PatchRef _ (MkPatch {pvars_ = pv})]) -> lift $ readTVarIO $ pv ! $(litE (integerL i))
                                                   _ -> throw $ ContextException "turtle or patch" a
                                                              |]) []
          y <- newName "y"
          w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do 
                                                 (_,_,a,_,_,_) <- ask :: CSTM Context
                                                 case a of
                                                   PatchRef _ (MkPatch {pvars_ = pv}) -> lift $ writeTVar (pv ! $(litE (integerL i))) $(varE y)
                                                   TurtleRef _ _ -> patch_here >>= \ ([PatchRef _ (MkPatch {pvars_ = pv})]) -> lift $ writeTVar (pv ! $(litE (integerL i))) $(varE y)
                                                   _ -> throw $ ContextException "turtle or patch" a
                                                                     |]) []]
          x <- funD (mkName ("with_" ++ v)) [clause [varP y] (normalB [| do 
                                                 (_,_,a,_,_,_) <- ask :: CSTM Context
                                                 case a of
                                                   PatchRef _ (MkPatch {pvars_ = pv}) -> lift $ modifyTVar' (pv ! $(litE (integerL i))) $(varE y)
                                                   TurtleRef _ _ -> patch_here >>= \ ([PatchRef _ (MkPatch {pvars_ = pv})]) -> lift $ modifyTVar' (pv ! $(litE (integerL i))) $(varE y)
                                                   _ -> throw $ ContextException "turtle or patch" a
                                                                     |]) []]
          return [p,u,w,x]
            ) (zip vs [0..])
  return $ pl : concat pg


breeds_own p vs = do
  y <- newName "y"
  cb <- funD (mkName ("create_" ++ p)) [clause [varP y]
                                       (normalB [| create_breeds $(litE (stringL p)) $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  cob <- funD (mkName ("create_ordered_" ++ p)) [clause [varP y]
                                                (normalB [| create_ordered_breeds $(litE (stringL p)) $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  sp <- funD (mkName ("sprout_" ++ p)) [clause [varP y] (normalB [| do
                                                           (gs, tw, a, _, _,_) <- ask
                                                           case a of
                                                             PatchRef (px,py) _ -> do
                                                                 let who = gs ! 0
                                                                 let  newTurtles w n = return . IM.fromAscList =<< sequence [do
                                                                                                                              t <- newBSprout i $(litE (integerL (genericLength vs))) (fromIntegral px) (fromIntegral py) $(litE (stringL p))
                                                                                                                              return (i, t)
                                                                                                                             | i <- [w..w+n-1]]
                                                                 let addTurtles ts' (MkWorld ps ts ls)  = MkWorld ps (ts `IM.union` ts') ls
                                                                 oldWho <- lift $ liftM round $ readTVar who
                                                                 lift $ modifyTVar' who (\ ow -> fromIntegral $(varE y) + ow)
                                                                 ns <- newTurtles oldWho $(varE y)
                                                                 lift $ modifyTVar' tw (addTurtles ns) 
                                                                 return $ map (uncurry TurtleRef) $ IM.toList ns -- todo: can be optimized
                                                             _ -> throw $ ContextException "patch" a
                                                         |]) []]





  pg <- mapM (\ (v, i) -> do
          p <- valD (varP (mkName v)) (normalB [| do 
                                                 (_,_,a ,_,_,_) <- ask :: CSTM Context; 
                                                 case a of
                                                   TurtleRef _ (MkTurtle {tvars_ = pv}) -> lift $ readTVar (pv ! $(litE (integerL i)))
                                                   _ -> throw $ ContextException "turtle" a
                                               |]) []
          u <- valD (varP (mkName ("unsafe_" ++ v))) (normalB [| do 
                                                                (_,_,a ,_,_,_) <- ask :: CIO Context; 
                                                                case a of
                                                                  TurtleRef _ (MkTurtle {tvars_ = pv}) -> lift $ readTVarIO (pv ! $(litE (integerL i)))
                                                                  _ -> throw $ ContextException "turtle" a
                                                              |]) []
              
          y <- newName "y"
          w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do 
                                                                       (_,_,a,_,_,_) <- ask :: CSTM Context; 
                                                                       case a of
                                                                         TurtleRef _ (MkTurtle {tvars_ = pv}) -> lift $ writeTVar (pv ! $(litE (integerL i))) $(varE y)
                                                                         _ -> throw $ ContextException "turtle" a
                                                                     |]) []]
          x <- funD (mkName ("with_" ++ v)) [clause [varP y] (normalB [| do 
                                                                       (_,_,a,_,_,_) <- ask :: CSTM Context; 
                                                                       case a of
                                                                         TurtleRef _ (MkTurtle {tvars_ = pv}) -> lift $ modifyTVar' (pv ! $(litE (integerL i))) $(varE y)
                                                                         _ -> throw $ ContextException "turtle" a
                                                                     |]) []]

          return [p,u,w,x]
            ) (zip vs [0..])
  return $ cb : sp : cob : concat pg

breeds [p,s] = do
  sp <- valD (varP (mkName p)) (normalB [| do 
                                          ts <- turtles; 
                                          filterM (\ (TurtleRef _ (MkTurtle {breed_ = tb})) -> do
                                                     b <- lift $ readTVar tb
                                                     return $ b == $(litE (stringL p))) ts 
                                        |]) []
  up <- valD (varP (mkName ("unsafe_" ++ p))) (normalB [| with (unsafe_breed >>= \ b -> return (b == $(litE (stringL p)))) =<< unsafe_turtles |]) []
  x <- newName "x"
  y <- newName "y"
  us <- funD (mkName ("unsafe_" ++ s)) [clause [varP y] 
                                       (normalB [| do 
                                                   (_,tw,_, _, _,_) <- ask :: CIO Context
                                                   (MkWorld _ ts _) <- lift (readTVarIO tw)
                                                   let {t = ts IM.! $(varE y)}
                                                   b <- lift $ readTVarIO (breed_ t)
                                                   return $ if b == $(litE (stringL p)) 
                                                            then [TurtleRef $(varE y) t] 
                                                            else error ("turtle is not a " ++ s) |]) []]

  ss <- funD (mkName s) [clause [varP y] (normalB [| do 
                                                    (_,tw,_, _, _,_) <- ask :: CSTM Context
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
                                                                     [s] <- self
                                                                     h <- unsafe_patch_here
                                                                     ts <- $(varE (mkName ("unsafe_" ++ p)))
                                                                     res <- with (return . ( == h) =<< unsafe_patch_here) ts
                                                                     return (s:res)
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
  ib <- funD (mkName ("is_" ++ s ++ "p")) [clause [varP y] 
                                          (normalB [| maybe (return False) (\ t -> case t of 
                                                                                    [TurtleRef _ (MkTurtle {breed_ = tb})] -> do
                                                                                      b <- lift $ readTVar tb
                                                                                      return $ b == $(litE (stringL p))
                                                                                    _ -> return False) (cast $(varE y) :: Maybe [AgentRef]) |]) []]
  return [sp,up,us,ss,th,uth,ta, ib]


directed_link_breed [p,s] = do
  sp <- valD (varP (mkName p)) (normalB [| do ls <- links; filterM (\ (LinkRef _ (MkLink {lbreed_ = b})) -> do return $ b == $(litE (stringL p))) ls |]) []
  x <- newName "x"
  y <- newName "y"
  ss <- funD (mkName s) [clause [varP x, varP y] 
                        (normalB [| do (_, tw,_, _, _,_) <- ask :: CSTM Context; (MkWorld _ _ ls) <- lift $ readTVar tw; return $ [maybe Nobody (LinkRef ($(varE x),$(varE y))) $M.lookup ($(varE x),$(varE y)) ls] |]) []]
  return [sp, ss]

undirected_link_breed [p,s] = do
  sp <- valD (varP (mkName p)) (normalB [| do ls <- links; filterM (\ (LinkRef _ (MkLink {lbreed_ = b})) -> do return $ b == $(litE (stringL p))) ls |]) []
  x <- newName "x"
  y <- newName "y"
  ss <- funD (mkName s) [clause [varP x, varP y] 
                        (normalB [| do (_, tw,_, _, _,_) <- ask :: CSTM Context; (MkWorld _ _ ls) <- lift $ readTVar tw; return $ [maybe Nobody (LinkRef ($(varE x),$(varE y))) $M.lookup ($(varE x),$(varE y)) ls] |]) []]
  return [sp, ss]

links_own vs = do
  y <- newName "y"
  cls <- funD (mkName "create_links_with") [clause [varP y] (normalB [| create_links_with_ $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  cts <- funD (mkName "create_links_to") [clause [varP y] (normalB [| create_links_to_ $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  cfs <- funD (mkName "create_links_from") [clause [varP y] (normalB [| create_links_from_ $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  cl <- funD (mkName "create_link_with") [clause [varP y] (normalB [| create_link_with_ $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  ct <- funD (mkName "create_link_to") [clause [varP y] (normalB [| create_link_to_ $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  cf <- funD (mkName "create_link_from") [clause [varP y] (normalB [| create_link_from_ $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  pg <- mapM (\ (v, i) -> do
          p <- valD (varP (mkName v)) (normalB [| do 
                                                 (_,_,a ,_,_,_) <- ask :: CSTM Context; 
                                                 case a of
                                                   LinkRef _ (MkLink {lvars_ = pv}) -> lift $ readTVar (pv ! $(litE (integerL i)))
                                                   _ -> throw $ ContextException "link" a
                                               |]) []
                                                 
          u <- valD (varP (mkName ("unsafe_" ++ v))) (normalB [| do 
                                                                (_,_,a ,_,_,_) <- ask :: CIO Context; 
                                                                case a of
                                                                  LinkRef _ (MkLink {lvars_ = pv}) -> lift $ readTVarIO (pv ! $(litE (integerL i))) 
                                                                  _ -> throw $ ContextException "link" a
                                                              |]) []
          y <- newName "y"
          w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do 
                                                                       (_,_,a,_,_,_) <- ask :: CSTM Context; 
                                                                       case a of
                                                                         LinkRef _ (MkLink {lvars_ = pv}) -> lift $ writeTVar (pv ! $(litE (integerL i))) $(varE y) 
                                                                         _ -> throw $ ContextException "link" a
                                                                     |]) []]
          x <- funD (mkName ("with_" ++ v)) [clause [varP y] (normalB [| do 
                                                                       (_,_,a,_,_,_) <- ask :: CSTM Context; 
                                                                       case a of
                                                                         LinkRef _ (MkLink {lvars_ = pv}) -> lift $ modifyTVar' (pv ! $(litE (integerL i))) $(varE y) 
                                                                         _ -> throw $ ContextException "link" a
                                                                     |]) []]

          return [p,u,w,x]
            ) (zip vs [0..])
  return $ cls : cts : cfs : cl : ct : cf : concat pg

link_breeds_own p vs = do
  y <- newName "y"
  cls <- funD (mkName $ "create_" ++ p ++ "_with") [clause [varP y] (normalB [| create_breeded_links_with $(litE (stringL p)) $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  cts <- funD (mkName $ "create_" ++ p ++ "_to") [clause [varP y] (normalB [| create_breeded_links_to $(litE (stringL p)) $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  cfs <- funD (mkName $ "create_" ++ p ++ "_from") [clause [varP y] (normalB [| create_breeded_links_from $(litE (stringL p)) $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  pg <- mapM (\ (v, i) -> do
          p <- valD (varP (mkName v)) (normalB [| do 
                                                 (_,_,a ,_,_,_) <- ask :: CSTM Context;
                                                 case a of
                                                   LinkRef _ (MkLink {lvars_ = pv}) -> lift $ readTVar (pv ! $(litE (integerL i))) 
                                                   _ -> throw $ ContextException "link" a
                                               |]) []
          u <- valD (varP (mkName ("unsafe_" ++ v))) (normalB [| do
                                                                (_,_,a ,_,_,_) <- ask :: CIO Context; 
                                                                case a of
                                                                  LinkRef _ (MkLink {lvars_ = pv}) -> lift $ readTVarIO (pv ! $(litE (integerL i))) 
                                                                  _ -> throw $ ContextException "link" a
                                                              |]) []
          y <- newName "y"
          w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do 
                                                                       (_,_,a,_,_,_) <- ask :: CSTM Context; 
                                                                       case a of
                                                                         LinkRef _ (MkLink {lvars_ = pv}) -> lift $ writeTVar (pv ! $(litE (integerL i))) $(varE y)
                                                                         _ -> throw $ ContextException "link" a
                                                                     |]) []]
          x <- funD (mkName ("with_" ++ v)) [clause [varP y] (normalB [| do 
                                                                       (_,_,a,_,_,_) <- ask :: CSTM Context; 
                                                                       case a of
                                                                         LinkRef _ (MkLink {lvars_ = pv}) -> lift $ modifyTVar' (pv ! $(litE (integerL i))) $(varE y)
                                                                         _ -> throw $ ContextException "link" a
                                                                     |]) []]

          return [p,u,w,x]
            ) (zip vs [0..])
  return $ cls : cts : cfs : concat pg


run as = do 
  [d| main = do c <- cInit $(varE (mkName "globals_length")) $(varE (mkName "patches_length")); runReaderT (foldl1 (>>) $(listE (map (\ a -> infixE (Just (varE a)) (varE (mkName ">>")) (Just (appE (varE (mkName "return")) (conE (mkName "()"))))) as))) c |]


runT as = do c <- cInit 1 0
             runReaderT as c
             



-- | Internal
random_primary_color :: CSTM Double
random_primary_color = do
  (_,_,_,_,ts,_) <- ask
  s <- lift $ readTVar ts
  let (v,s') = randomR (0,13) s
  lift $ writeTVar ts s'
  return (primary_colors !! v)

-- | Internal
random_integer_heading :: CSTM Integer
random_integer_heading = do
  (_,_,_,_,ts,_) <- ask
  s <- lift $ readTVar ts
  let (v,s') = randomR (0,360) s
  lift $ writeTVar ts s'
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
       (return . listArray (0, fromIntegral to -1) =<< replicateM (fromIntegral to) (newTVar 0))

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
       (return . listArray (0, fromIntegral to -1) =<< replicateM (fromIntegral to) (newTVar 0))

-- | Internal
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
       (return . listArray (0, fromIntegral to -1) =<< replicateM (fromIntegral to) (newTVar 0))

-- | Internal
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
       (return . listArray (0, fromIntegral to -1) =<< replicateM (fromIntegral to) (newTVar 0))

-- | Internal
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
       (return . listArray (0, fromIntegral to -1) =<< replicateM (fromIntegral to) (newTVar 0))


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
            (return . listArray (0, fromIntegral to -1) =<< replicateM (fromIntegral to) (newTVar 0))


-- | Internal, Utility function to make TemplateHaskell easier
create_breeds :: String -> Int -> Int -> CSTM [AgentRef] -- ^ Breed -> Size -> VarLength -> CSTM BreededTurtles
create_breeds b n to = do
  (gs, tw, a, _, _,_) <- ask
  -- context checking
  case a of
    ObserverRef -> return ()
    _ -> throw $ ContextException "observer" a
  let who = gs ! 0
  oldWho <- lift $ liftM round $ readTVar who
  lift $ modifyTVar' who (\ ow -> fromIntegral n + ow)
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
  (gs, tw, a, _, _,_) <- ask
  -- context checking
  case a of
    ObserverRef -> return ()
    _ -> throw $ ContextException "observer" a
  let who = gs ! 0
  lift $ do
    oldWho <- liftM round $ readTVar who
    modifyTVar' who (\ ow -> fromIntegral n + ow)
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


-- | Internal
-- | internal links directed by default, that makes create-link(s)-with faulty
-- | todo determine at run-time the direction of links
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
                  (return . listArray (0, fromIntegral ls -1) =<< replicateM (fromIntegral ls) (newTVar 0))                  

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
                  (return . listArray (0, fromIntegral ls -1) =<< replicateM (fromIntegral ls) (newTVar 0))                  


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
  (_, tw, a, _, _,_) <- ask
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
  (_, tw, a, _, _,_) <- ask
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
  (_, tw, a, _, _,_) <- ask
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
  (_, tw, a, _, _,_) <- ask
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
  (_, tw, a, _, _,_) <- ask
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
  (_, tw, a, _, _,_) <- ask
  case a of
    TurtleRef x _ -> do
           (MkWorld ps ts ls) <- lift $ readTVar tw
           ls' <- foldr (=<<) (return ls) [ insertLink t x  | (TurtleRef t _) <- as]
           lift $ writeTVar tw (MkWorld ps ts ls')
    _ -> throw $ ContextException "turtle" a
    where insertLink t x s =  do
                       n <- newLBreed x t False b ls
                       return $ M.insertWith (flip const) (t,x) n $ M.insertWith (flip const) (x,t) n s

