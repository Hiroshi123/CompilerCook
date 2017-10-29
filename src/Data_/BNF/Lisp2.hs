{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module MData.BNF.Lisp2 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

import qualified Data.Char
import Data.Word

import MData.M
import MData.Parser.Base
import MData.Parser.ByteStr
import MData.Parser.List
import MData.Parser.Int

type Bs = BS.ByteString

data Lisp =
    Atom  LAtom 
  | Cell Ope Lisp Lisp
  deriving (Show)


data Ope =
     CAR
   | CDR
   | CONS
   | EQ
 --   LF PreFunc
   | Defun Bs
   
--  | SP SpecialForm
  deriving (Show)

-- data PreFunc =
--     CAR  
--   | CDR  
--   | CONS 
--   | EQ 
--   deriving (Show)

-- data SpecialForm =
--     Lambda 
--   | Defun  
--   | Cond   
--   deriving (Show)

data LAtom =
    LNum Bs
  | LString Bs
  | LBool Bs
  | LVar Bs
  | Nil
  deriving (Show)

car =
  
  string ("car " :: Bs) >==
  (\_ -> cell >==  
    (\x ->
       case x of 
         (Cell CONS a b) -> r' $ Cell CAR a b
         a               -> r' $ Cell CAR a (Atom Nil)
    )
  )
  
  
cdr =
  string ("cdr " :: Bs) >==
  (\_ -> cell >==  
    (\x ->
       case x of 
         (Cell CONS a b) -> r' $ Cell CDR a b
         a               -> r' $ Atom Nil -- Cell CAR a (Atom Nil)
    )
  )
  
  
---defun have to be encoded as a combination of

type Genv = [(Bs,Lisp)]

--gEnv :: Genv
--gEnv = []

  
defun genv =
  (<->) **>
  string ("defun " :: Bs)
  **> (<->) **>
  many1 (satisfy letter) >== 
  (\fname -> cell >==
    ( \y ->
        case y of
          (Cell CONS arg b) ->
            let gEnv = [( fname , (Cell CONS arg b) )] ++ gEnv
            in r' (Cell (Defun fname) arg b)
    )
  )
  
  
temp2 = many temp1
  
temp1 =
  (<->) **>
  many1 (satisfy letter)
  **< (<->)
  
carF (Cell CONS a b) = a

--cdrF

-- CAR  car'
-- CDR  cdr'
-- Cons cons'
  
-- deff = Parser $ \x -> [(Nil,x)]
-- atom = lstr <|> lnum <|> lbool  -- <|> Nil
-- ele = (<->) **> atom <|> cell **< (<->)

atoms = atom >==
  (\x ->
     atoms >==
     (\y -> 
        r' (Cell CONS x y)
     ) <|>  (r' x) --  (r' (Atom Nil)) 
  )
  
  
atom = lstr <|> lnum <|> lbool <|> lvar -- <|> Nil
  
expr = (<->) **> atoms <|> cell **< (<->)

-- cell2 =
--   ele >==
--   (\x -> ele >==
--     (\y -> r' (Cell x y))
--   )
  
lstr =
  str >== (\x -> (r' $ Atom (LString x)) )
  
  
lnum  =
  (<->) **> 
  satisfy digit >== (\x -> (r' $ Atom (LNum x)) )
  **< (<->)
  
lbool =
  (<->) **> 
  char ("t" :: Bs)  >== (\x -> (r' $ Atom (LBool x)) )
  **< (<->)
  
lvar =
  (<->) **> 
  many1 (satisfy letter) >== (\x -> (r' $ Atom (LVar x)) )
  **< (<->)
  
  
--lnum = satisfy digit >== (\x -> (r' $ LAtom (LNum x)) )

--cell :: Parser a -> Parser a

cell =
  
  (<->) **>
  char ("(" :: Bs)
  **> (<->) **>
  expr
  --item >== (\x -> r' $ Cell Nil Nil)
  **< (<->) **<
  char (")" :: Bs)
  **< (<->)
  
  
-- instance Show PreFunc where
--   show (CAR x) = "car " -- ++ (show x)
--   show (Hu) = "hei!"





