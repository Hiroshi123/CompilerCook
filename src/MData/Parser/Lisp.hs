{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module MData.Parser.Lisp where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

import Data.Word

-- myModule

import MData.M
import MData.Parser.Base
import MData.Parser.ByteStr
import MData.Parser.List
import MData.Parser.Int

import MData.BNF.Lisp


-- (defun f1 x) (x)

-- data Lisp
--   = LList [Lisp]
--   | LAtom BS.ByteString
--   | LNum BS.ByteString
--   | LBool  BS.ByteString
--   | LNil   BS.ByteString
--   | LCons Lisp Lisp
-- --  | LClosure [String] Code Env
-- --  | LError String
--   deriving (Show)

type MyInt8 = BS.ByteString

-- ( add 3 5 )
-- (defun f (x) (x))
-- (defun ff (x) (+ x 3))

-- atom :: Parser Lisp
-- atom = Parser $ \x ->
--   case x of
--     "" -> []
--     otherwise -> [(LNil x,x)]

atom :: Parser Lisp
atom = satisfy digit >== (\x -> r' $ LAtom (LNum x))
       
dig' :: Parser Word8
dig' = satisfy digit >== (\x -> r' $ BS.head x)

dig =
  
  (<->) **>
  dig' >== (\x -> r' $ N x)
  **< (<->)
  
-- function ()
  
arith =
  ope ( "+":: Bs ) ari Add
  <|>
  ope ( "-":: Bs ) ari Sub
  <|>
  ope ( "*":: Bs ) ari Mul
  <|>
  ope ( "/":: Bs ) ari Div
  
  
ones =
  (<->) **>
  (satisfy digit) >== (\x -> r' $ W (BS.head x))
  **< (<->)
  
  
logical :: Parser Logical
logical =
  
  ope ( "and":: Bs ) logi And
  <|>
  ope ( "or" :: Bs ) logi Or
  
  
ope :: BC.ByteString -> Parser t -> (t -> t -> b) -> Parser b
ope bs p f =
  
  (<->) **>
  string bs
  **> (<->) **>
  p >==
  (\x ->
     p >==
     (\y ->
         r' (f x y)
     )
  )
  
  
--Add (W dig) (W dig)

-- num :: Parser Lisp
-- num = Parser

--add :: Parser 
--sub ::



logi :: Parser Logical
logi = parenthesis logical <|> ones

ari :: Parser Arithmetic
ari = dig <|> parenthesis arith

parenthesis :: Parser a -> Parser a
parenthesis f =
  (<->) **>
  char ("(" :: Bs)
  **> (<->) **>
  f
  **< (<->) **<
  char (")" :: Bs)
  **< (<->)
  
  
