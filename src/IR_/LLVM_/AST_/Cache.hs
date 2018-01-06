{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

import GHC.TypeLits
import Data.Type.Equality

data Foo :: Nat -> * where
  Small    :: (n <= 2)  => Foo n
  Big      :: (3 <= n) => Foo n

  Empty    :: ((n == 0) ~ True) => Foo n
  NonEmpty :: ((n == 0) ~ False) => Foo n



data INT :: Nat -> * where
  INT1 :: ((n == 1) ~ True) => INT n
  INT2 :: ((n == 2) ~ True) => INT n
  INT3 :: ((n == 3) ~ True) => INT n
  INT4 :: ((n == 4) ~ True) => INT n
  
  
f1 :: INT 1
f1 = INT1



