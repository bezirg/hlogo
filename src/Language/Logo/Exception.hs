{-# OPTIONS_HADDOCK show-extensions #-}
-- | 
-- Module      :  Language.Logo.Exception
-- Copyright   :  (c) 2013-2016, the HLogo team
-- License     :  BSD3
-- Maintainer  :  Nikolaos Bezirgiannis <bezirgia@cwi.nl>
-- Stability   :  experimental
--
-- The exception handling of the framework. Uses the built-in exception mechanism of Haskell, 
-- so the EDSL is defined as a very shallow embedding. Alternative way with deeper embedding is to use the ErrorT transformer.
module Language.Logo.Exception
    (throw,                     
     catch,
     catchIO,
     evaluate,
     -- * The type class
     Exception, assert,
     -- * Built-in exceptions imported from Haskell base
     SomeException, IOException , ArithException (..) , AssertionFailed (..), AsyncException (..), NestedAtomically (..) , BlockedIndefinitelyOnSTM (..) , Deadlock (..), ErrorCall (..),
    -- * Exceptions specific to HLogo
     TypeException (..), StopException (..), DevException (..)
    )
        where

import Language.Logo.Base
import Control.Exception hiding (catch)
import qualified Control.Exception as E (catch)
import Control.Concurrent.STM
import Data.Typeable
import Control.Monad.Trans.Reader (liftCatch)

{-# INLINE catch #-}
-- | Catches an Exception in STM monad
catch :: Exception e => C _s _s' STM a -> (e -> C _s _s' STM a) -> C _s _s' STM a
catch = liftCatch catchSTM

{-# INLINE catchIO #-}
-- | Catches an Exception in IO monad
catchIO :: Exception e => C _s _s' IO a -> (e -> C _s _s' IO a) -> C _s _s' IO a
catchIO = liftCatch E.catch

-- | Thrown when a primitive procedure expected a different *this* context (i.e. the callee inside the context monad transformer).
--
-- Used as ContextException ExpectedCalleeAsString GotInsteadAgentRef

-- | A type error thrown when a procedure expected a different type of agent argument.
-- Normally we use the static typing of Haskell, except for 'AgentRef's, which are checked dynamically on run-time
--
-- Used as TypeException ExpectedTypeAsString GotInsteadAgentRef

-- | A type error thrown when a procedure expected a different type of agent argument.
-- Normally we use the static typing of Haskell, except for 'AgentRef's, which are checked dynamically on run-time
--
-- Used as TypeException ExpectedTypeAsString GotInsteadAgentRef
data TypeException = TypeException String
                     deriving (Eq, Typeable)


-- | The error thrown by the 'Language.Logo.Prim.stop' primitive. It is a trick and should be catched in an upper caller primitive.
data StopException = StopException
                   deriving (Eq, Typeable)

data DevException = DevException
                  deriving (Eq,Typeable)

instance Exception StopException
instance Exception DevException
instance Exception TypeException

instance Show TypeException where
    show (TypeException expected) = "TypeException " ++ expected

instance Show StopException where
    show StopException = "Stop called and has not be catched. This should not normally happen"

instance Show DevException where
    show DevException = "error: this should not happen, contact the developers"
