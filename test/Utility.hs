module Utility where

import Framework.Logo.Exception
import Control.Monad.Trans.Class
import Test.HUnit

-- For HUnit
assertTypeException action = catchIO 
                            (action >> af)
                            (\ e -> let _ = e :: TypeException
                                   in if e == TypeException undefined undefined
                                      then return ()
                                      else af
                            )
    where 
      af = lift $ assertFailure $ "Expected exception: TypeException" 

-- For HUnit
assertContextException action = catchIO 
                            (action >> af)
                            (\ e -> let _ = e :: ContextException
                                   in if e == ContextException undefined undefined
                                      then return ()
                                      else af
                            )
    where 
      af = lift $ assertFailure $ "Expected exception: ContextException" 
