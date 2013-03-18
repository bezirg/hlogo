{-# LANGUAGE TemplateHaskell #-}
module Framework.Logo.Keyword where

import Language.Haskell.TH
import Framework.Logo.Core
import Framework.Logo.Base
import Framework.Logo.Prim
import Control.Monad.Reader
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



turtles_own vs = do
  y <- newName "y"
  ct <- funD (mkName "create_turtles") [clause [varP y] (normalB [| do
                                                                   (gs, tw, _, _, _) <- ask
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

  co <- funD (mkName "create_ordered_turtles") [clause [varP y] (normalB [| do
                                                                           (gs, tw, _, _, _) <- ask
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
          p <- valD (varP (mkName v)) (normalB [| do (_,_,TurtleRef _ (MkTurtle {tvars_ = pv}) ,_,_) <- ask :: CSTM Context; lift $ readTVar (pv ! $(litE (integerL i))) |]) []
          u <- valD (varP (mkName ("unsafe_" ++ v))) (normalB [| do (_,_,TurtleRef _ (MkTurtle {tvars_ = pv}) ,_,_) <- ask :: CIO Context; lift $ readTVarIO (pv ! $(litE (integerL i))) |]) []
          y <- newName "y"
          w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do (_,_,TurtleRef _ (MkTurtle {tvars_ = pv}),_,_) <- ask :: CSTM Context; lift $ writeTVar (pv ! $(litE (integerL i))) $(varE y) |]) []]
          return [p,u,w]
            ) (zip vs [0..])
  return $ ct : co : crt : cro: concat pg

patches_own vs = do
  pl <- valD (varP (mkName "patches_length")) (normalB [| $(litE (integerL (genericLength vs))) |]) []
  -- Variables setters/getters
  pg <- mapM (\ (v, i) -> do
          p <- valD (varP (mkName v)) (normalB [| do (_,_,PatchRef _ (MkPatch {pvars_ = pv}) ,_,_) <- ask :: CSTM Context; lift $ readTVar (pv ! $(litE (integerL i))) |]) []
          u <- valD (varP (mkName ("unsafe_" ++ v))) (normalB [| do (_,_,PatchRef _ (MkPatch {pvars_ = pv}) ,_,_) <- ask :: CIO Context; lift $ readTVarIO (pv ! $(litE (integerL i))) |]) []
          y <- newName "y"
          w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do (_,_,PatchRef _ (MkPatch {pvars_ = pv}),_,_) <- ask :: CSTM Context; lift $ writeTVar (pv ! $(litE (integerL i))) $(varE y) |]) []]
          return [p,u,w]
            ) (zip vs [0..])
  return $ pl : concat pg


links_own vs = [d| |]

breeds_own p vs = do
  y <- newName "y"
  cb <- funD (mkName ("create_" ++ p)) [clause [varP y]
                                       (normalB [| create_breeds $(litE (stringL p)) $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  cob <- funD (mkName ("create_ordered_" ++ p)) [clause [varP y]
                                                (normalB [| create_ordered_breeds $(litE (stringL p)) $(varE y) $(litE (integerL (genericLength vs))) |]) []]
  pg <- mapM (\ (v, i) -> do
          p <- valD (varP (mkName v)) (normalB [| do (_,_,TurtleRef _ (MkTurtle {tvars_ = pv}) ,_,_) <- ask :: CSTM Context; lift $ readTVar (pv ! $(litE (integerL i))) |]) []
          u <- valD (varP (mkName ("unsafe_" ++ v))) (normalB [| do (_,_,TurtleRef _ (MkTurtle {tvars_ = pv}) ,_,_) <- ask :: CIO Context; lift $ readTVarIO (pv ! $(litE (integerL i))) |]) []
          y <- newName "y"
          w <- funD (mkName ("set_" ++ v)) [clause [varP y] (normalB [| do (_,_,TurtleRef _ (MkTurtle {tvars_ = pv}),_,_) <- ask :: CSTM Context; lift $ writeTVar (pv ! $(litE (integerL i))) $(varE y) |]) []]
          return [p,u,w]
            ) (zip vs [0..])
  return $ cb : cob : concat pg

breeds [p,s] = do
  sp <- valD (varP (mkName p)) (normalB [| do ts <- turtles; filterM (\ (TurtleRef _ (MkTurtle {breed_ = b})) -> do return $ b == $(litE (stringL p))) ts |]) []
  up <- valD (varP (mkName ("unsafe_" ++ p))) (normalB [| with (breed >>= \ b -> return (b == $(litE (stringL p)))) =<< unsafe_turtles |]) []
  x <- newName "x"
  y <- newName "y"
  us <- funD (mkName ("unsafe_" ++ s)) [clause [varP y] 
                                       (normalB [| do (_,tw,_, _, _) <- ask :: CIO Context; (MkWorld _ ts _) <- lift (readTVarIO tw); let {t = ts IM.! $(varE y)}; return $ if (breed_ t) == $(litE (stringL p)) then [TurtleRef $(varE y) t] else error ("turtle is not a " ++ s) |]) []]
  ss <- funD (mkName s) [clause [varP y] 
                        (normalB [| do (_,tw,_, _, _) <- ask :: CSTM Context; (MkWorld _ ts _) <- lift (readTVar tw); let {t = ts IM.! $(varE y)}; return $ if (breed_ t) == $(litE (stringL p)) then [TurtleRef $(varE y) t] else error ("turtle is not a " ++ s) |]) []]
  th <- valD (varP (mkName (p ++ "_here"))) (normalB [| do [s] <- self; [PatchRef (px,py) _] <- patch_here;  ts <- turtles;  filterM (\ (TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y, breed_ = b})) -> do x' <- lift $ readTVar x;  y' <- lift $ readTVar y; return $ round x' == px && round y' == py && b == $(litE (stringL p))) ts |]) []
  ta <- funD (mkName (p ++ "_at")) [clause [varP x, varP y] 
                                   (normalB [| do [s] <- self; [PatchRef (px,py) _] <- patch_at $(varE x) $(varE y);  ts <- turtles;  filterM (\ (TurtleRef _ (MkTurtle {xcor_ = x, ycor_ = y, breed_ = b})) -> do x' <- lift $ readTVar x;  y' <- lift $ readTVar y; return $ round x' == px && round y' == py && b == $(litE (stringL p))) ts |]) []]
  ib <- funD (mkName ("is_" ++ s ++ "p")) [clause [varP y] 
                                   (normalB [| return $ maybe False (\ t -> case t of [TurtleRef _ (MkTurtle {breed_ = b})] -> b == $(litE (stringL p)); _ -> False) (cast $(varE y) :: Maybe [AgentRef]) |]) []]
  return [sp,up,us,ss,th,ta, ib]


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
  [d| main = do c <- cInit $(varE (mkName "globals_length")) $(varE (mkName "patches_length")); runReaderT (foldl1 (>>) $(listE (map (\ a -> infixE (Just (varE a)) (varE (mkName ">>")) (Just (appE (varE (mkName "return")) (conE (mkName "()"))))) as))) c |]


-- | Internal
random_primary_color :: CSTM Double
random_primary_color = do
  (_,_,_,_,ts) <- ask
  s <- lift $ readTVar ts
  let (v,s') = randomR (0,13) s
  lift $ writeTVar ts s'
  return (primary_colors !! v)

-- | Internal
random_integer_heading :: CSTM Integer
random_integer_heading = do
  (_,_,_,_,ts) <- ask
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
       return b <*>
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
       return b <*>
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
       return "turtles" <*>
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
newOrderedTurtle :: Int -> Int -> Int -> Int -> STM Turtle -- ^ Index -> Order -> Who -> VarLength -> CSTM Turtle
newOrderedTurtle i o x to = do
    let rpc = primary_colors !! ((i-1) `mod` 14)
    let rdh = ((toEnum i-1) / toEnum o) * 360 :: Double
    MkTurtle <$>
             return x <*>
             return "turtles" <*>
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
  (gs, tw, _, _, _) <- ask
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
  (gs, tw, _, _, _) <- ask
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

