{-# LANGUAGE FlexibleContexts #-}

--f :: forall t Num [[t]] => [[t]]

f :: Lis a -> Lis a
f (Nod x a) = (Nod x (f a))
f Nil = Nil


--f [] = []
--f x = (head x) ++ ( f (tail x) )
  
  --case head x of
  --[] -> ( f (tail x) )
  --otherwise -> (head x) ++ ( f (tail x) )
  

--  [[1,2],2,3]

data Lis a = Nod a (Lis a) | Nil
  deriving (Show)


data Tr x a =
  Nd x  (Tr x a) (Tr x a)
  | Nin
  deriving (Show)
