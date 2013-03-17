{-# LANGUAGE TemplateHaskell #-}
module Framework.Logo.Keyword where

import Language.Haskell.TH
import Framework.Logo.Core
import Framework.Logo.Base
import Framework.Logo.Prim
import Control.Monad.Reader
import Control.Monad (liftM)
import Control.Concurrent.STM
import Data.Array
import Data.List (genericLength)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Maybe (maybe)
import Data.Typeable (cast)

globals vs  = liftM2 (++) 
              -- trick to store the length of globals
              [d| globals_length = $(litE (integerL (genericLength vs))) |] 
              -- create 2 getters (1 prim and 1 unsafe) per global variable
              (liftM concat $ mapM (\ (v,i) -> do
                      p <- valD (varP (mkName v)) (normalB [| do (gs,_,_,_,_) <- ask :: CSTM Context; lift $ readTVar (gs ! $(litE (integerL i))) |]) []
                      u <- valD (varP (mkName ("unsafe_" ++ v))) (normalB [| do (gs,_,_,_,_) <- ask :: CIO Context; lift $ readTVarIO (gs ! $(litE (integerL i))) |]) []
                      y <- newName "y"
                      w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do (gs,_,_,_,_) <- ask :: CSTM Context; lift $ writeTVar (gs ! $(litE (integerL i))) $(varE y) |]) []]
                      return [p,u,w]
                    )  (zip vs [2..]))



turtles_own vs = [d| |]

patches_own vs = [d| |]

links_own vs = [d| |]

breeds_own bs vs = [d| |]

breeds [p,s] = do
  sp <- valD (varP (mkName p)) (normalB [| do ts <- turtles; filterM (\ (TurtleRef _ (MkTurtle {breed_ = b})) -> do return $ b == $(litE (stringL p))) ts |]) []
  up <- valD (varP (mkName ("unsafe_" ++ p))) (normalB [| with (breed >>= \ b -> return (b == $(litE (stringL p)))) =<< unsafe_turtles |]) []
  x <- newName "x"
  y <- newName "y"
  us <- funD (mkName ("unsafe_" ++ s)) [clause [varP y] 
                                       (normalB [| do (_,tw,_, _, _) <- ask :: CIO Context; (MkWorld _ ts _) <- lift (readTVarIO tw); let {t = ts IM.! $(varE y)}; return $ if (breed_ t) == $(litE (stringL p)) then [TurtleRef $(varE y) t] else error ("turtle is not a " ++ s) |]) []]
  ss <- funD (mkName s) [clause [varP y] 
                        (normalB [| do (_,tw,_, _, _) <- ask :: CSTM Context; (MkWorld _ ts _) <- lift (readTVar tw); let {t = ts IM.! $(varE y)}; return $ if (breed_ t) == $(litE (stringL p)) then [TurtleRef $(varE y) t] else error ("turtle is not a " ++ s) |]) []]
  cb <- funD (mkName ("create_" ++ p)) [clause [varP y]
                                       (normalB [| create_breeds $(litE (stringL p)) $(varE y) |]) []]
  cob <- funD (mkName ("create_ordered_" ++ p)) [clause [varP y]
                                                (normalB [| create_ordered_breeds $(litE (stringL p)) $(varE y) |]) []]
  th <- valD (varP (mkName (p ++ "_here"))) (normalB [| do [s] <- self; [PatchRef (px,py) _] <- patch_here;  ts <- turtles;  filterM (\ (TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y, breed_ = b})) -> do x' <- lift $ readTVar x;  y' <- lift $ readTVar y; return $ round x' == px && round y' == py && b == $(litE (stringL p))) ts |]) []
  ta <- funD (mkName (p ++ "_at")) [clause [varP x, varP y] 
                                   (normalB [| do [s] <- self; [PatchRef (px,py) _] <- patch_at $(varE x) $(varE y);  ts <- turtles;  filterM (\ (TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y, breed_ = b})) -> do x' <- lift $ readTVar x;  y' <- lift $ readTVar y; return $ round x' == px && round y' == py && b == $(litE (stringL p))) ts |]) []]
  ib <- funD (mkName ("is_" ++ s ++ "p")) [clause [varP y] 
                                   (normalB [| maybe False (\ t -> case t of [TurtleRef _ (MkTurtle {breed_ = b})] -> b == $(litE (stringL p)); _ -> False) (cast $(varE y) :: Maybe [AgentRef]) |]) []]
  return [sp,up,us,ss,cb,cob,th,ta, ib]


directed_link_breed [p,s] = do
  sp <- valD (varP (mkName p)) (normalB [| do ls <- links; filterM (\ (LinkRef _ (MkLink {lbreed_ = b})) -> do return $ b == $(litE (stringL p))) ls |]) []
  x <- newName "x"
  y <- newName "y"
  ss <- funD (mkName s) [clause [varP x, varP y] 
                        (normalB [| do (_, tw,_, _, _) <- ask :: CSTM Context; (MkWorld _ _ ls) <- lift $ readTVar tw; return $ [maybe Nobody (LinkRef ($(varE x),$(varE y))) $M.lookup ($(varE x),$(varE y)) ls] |]) []]
  ct <- funD (mkName ("create_" ++ s ++ "_to")) [clause [varP y]
                                                (normalB [| case $(varE y) of [TurtleRef _ _] -> create_breeded_links_to $(litE (stringL p)) $(varE y); _ -> error "expected agentset with a single turtle" |]) []]
  cf <- funD (mkName ("create_" ++ s ++ "_from")) [clause [varP y]
                                                (normalB [| case $(varE y) of [TurtleRef _ _] -> create_breeded_links_from $(litE (stringL p)) $(varE y); _ -> error "expected agentset with a single turtle" |]) []]
  ct' <- funD (mkName ("create_" ++ p ++ "to")) [clause [varP y]
                                                (normalB [| create_breeded_links_to $(litE (stringL p)) $(varE y) |]) []]
  cf' <- funD (mkName ("create_" ++ p ++ "from")) [clause [varP y]
                                                (normalB [| create_breeded_links_from $(litE (stringL p)) $(varE y) |]) []]
  return [sp, ss, ct, cf, ct', cf']

undirected_link_breed [p,s] = do
  sp <- valD (varP (mkName p)) (normalB [| do ls <- links; filterM (\ (LinkRef _ (MkLink {lbreed_ = b})) -> do return $ b == $(litE (stringL p))) ls |]) []
  x <- newName "x"
  y <- newName "y"
  ss <- funD (mkName s) [clause [varP x, varP y] 
                        (normalB [| do (_, tw,_, _, _) <- ask :: CSTM Context; (MkWorld _ _ ls) <- lift $ readTVar tw; return $ [maybe Nobody (LinkRef ($(varE x),$(varE y))) $M.lookup ($(varE x),$(varE y)) ls] |]) []]
  ct <- funD (mkName ("create_" ++ s ++ "_with")) [clause [varP y]
                                                (normalB [| case $(varE y) of [TurtleRef _ _] -> create_breeded_links_with $(litE (stringL p)) $(varE y); _ -> error "expected agentset with a single turtle" |]) []]
  ct' <- funD (mkName ("create_" ++ p ++ "with")) [clause [varP y]
                                                (normalB [| create_breeded_links_with $(litE (stringL p)) $(varE y) |]) []]

  return [sp, ss, ct, ct']

link_breeds_own ls vs = [d| |]

run as = do 
  [d| main = do c <- cInit $(varE (mkName "globals_length")); runReaderT (foldl1 (>>) $(listE (map (\ a -> infixE (Just (varE a)) (varE (mkName ">>")) (Just (appE (varE (mkName "return")) (conE (mkName "()"))))) as))) c |]


