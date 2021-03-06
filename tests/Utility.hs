module Utility where

import Language.Logo

import Control.Monad.Trans.Class
import Test.HUnit

-- For HUnit
assertTypeException action = catchIO 
                            (action >> af)
                            (\ e -> let _ = e :: TypeException
                                   in if e == TypeException undefined
                                      then return ()
                                      else af
                            )
    where 
      af = lift $ assertFailure $ "Expected exception: TypeException" 


-- For HUnit
assertErrorCall action = catchIO 
                            (action >> af)
                            (\ e -> let _ = e :: ErrorCall
                                   in return ()
                            )
    where 
      af = lift $ assertFailure $ "Expected exception: ErrorCall" 


-- For HUnit
assertSomeException action = catchIO 
                            (action >> af)
                            (\ e -> let _ = e :: SomeException
                                   in return ()
                            )
    where 
      af = lift $ assertFailure $ "Expected exception: SomeException" 


