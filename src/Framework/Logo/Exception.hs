{-# LANGUAGE DeriveDataTypeable #-}
-- | The exception handling of the framework. Uses the built-in exception mechanism of Haskell, so the EDSL is defined as a very shallow embedding. Alternative way with
-- deeper embedding is to use the ErrorT transformer.
module Framework.Logo.Exception
    (throw,                     
     catch,
     catchIO,
     -- * The type class
     Exception,
     -- * Built-in exceptions imported from Haskell base
     SomeException, IOException , ArithException (..) , AssertionFailed (..), AsyncException (..), NestedAtomically (..) , BlockedIndefinitelyOnSTM (..) , Deadlock (..),
    -- * Exceptions specific to HLogo
     ContextException (..) , TypeException (..)
    )
        where

import Framework.Logo.Base
import Control.Exception (Exception, throw, SomeException, IOException, ArithException, AssertionFailed, AsyncException, NestedAtomically, BlockedIndefinitelyOnSTM, Deadlock)
import qualified Control.Exception as E (catch)
import Control.Concurrent.STM
import Data.Typeable
import Control.Monad.Trans.Reader

{-# INLINE catch #-}
-- | Catches an Exception in STM monad
--catch :: Exception e => CSTM a -> (e -> CSTM a) -> CSTM a
--catch :: Exception e => CSTM a -> (e -> CSTM a) -> CSTM a
catch :: Exception e => CSTM a -> (e -> CSTM a) -> CSTM a
catch = liftCatch catchSTM

{-# INLINE catchIO #-}
-- | Catches an Exception in IO monad
catchIO :: Exception e => CIO a -> (e -> CIO a) -> CIO a
catchIO = liftCatch E.catch

-- | Thrown when a primitive procedure expected a different calling context.
-- Used as ContextException ExpectedString GotInsteadAgentRef
data ContextException = ContextException String AgentRef
                      deriving (Typeable)

-- | A type error thrown when a procedure expected a different type of agent argument.
-- Normally we use the static typing of Haskell, except for AgentRefs, which are checked dynamically on run-time
-- Used as TypeException ExpectedString GotInsteadAgentRef
data TypeException = TypeException String AgentRef
                        deriving (Typeable)


instance Exception ContextException
instance Exception TypeException     


instance Show ContextException where
    show (ContextException expected got) = "Expected:" ++ expected ++ " Got:" ++ fromAgentRef got
                                                            
instance Show TypeException where
    show (TypeException expected got) = "Expected:" ++ expected ++ " Got:" ++ fromAgentRef got


-- Helper functions
fromAgentRef :: AgentRef -> String
fromAgentRef a = case a of
                   PatchRef _ _ -> "patch"
                   TurtleRef _ _ -> "turtle"
                   LinkRef _ _ -> "link"
                   ObserverRef -> "observer"
                   Nobody -> "nobody"

