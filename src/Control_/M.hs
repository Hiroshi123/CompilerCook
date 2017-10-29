
module Control_.M where

class M m where
  r'    :: a -> m a
  (>==) :: m a -> (a -> m b) -> (m b)
  (**>) :: m a -> m b -> m b
  (**<) :: m a -> m b -> m a
  
  (<**) :: m a -> m b -> m a
  (>**) :: m a -> m b -> m b
  
  
class (M m) => MPlus m where
  -- mzero :: m a
  (<>)  :: m a
  --(<|>) :: m a -> m a -> m a
  mmplus :: m a -> m a -> m a

  --(>>>) :: m a -> m b -> m b

instance M (Either a) where
  r' x = Right x
  Left x  >== _ = Left x
  Right x >== f = f x
  
instance M IO where
  r' = return
  action >== f =
    do
      x <- action
      f x
      
instance M [] where
  r' x = [x]
  f1 >== f2 = concatMap f2 f1
  
  
              
