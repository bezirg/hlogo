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
import qualified Data.IntMap as IM

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
  return [sp,up,us,ss,cb,cob,th,ta]


directed_link_breed [p,s] = [d| |]

undirected_link_breed [p,s] = [d| |]

link_breeds_own ls vs = [d| |]

run as = do 
  [d| main = do c <- cInit $(varE (mkName "globals_length")); runReaderT (foldl1 (>>) $(listE (map (\ a -> infixE (Just (varE a)) (varE (mkName ">>")) (Just (appE (varE (mkName "return")) (conE (mkName "()"))))) as))) c |]


