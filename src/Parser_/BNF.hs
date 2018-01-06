{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}


module MData.Parser.BNF where

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)

import Data.Word

-- myModule

import MData.M
import MData.Parser.Base
import MData.Parser.ByteStr
import MData.Parser.List
import MData.Parser.Int

--import MData.BNF.BNF

data Rule = Rule RuleName Expression deriving (Show)

data RuleName = RuleName BS.ByteString deriving (Show)

data Expression = Expression [Term] deriving (Show)

data Term =
  Term BS.ByteString
  | Literal BS.ByteString
  deriving (Show)


--data Literal = Literal BS.ByteString deriving (Show)

--data Expression =

--rule ::

rule = rule_name
  >==
  (\x ->
     expression >==
     (\y -> r' (Rule x y))
  )
  
  
rule_name = rule_name' >== (\x -> r' (RuleName x))
  
rule_name' =
  
  (<->) **>
  many1 (satisfy letter)
  **< (<->) **<
  string (BC.pack "::=")
  **< (<->)
  
  
expression = expression' >== (\x -> (r' (Expression x)))

expression' = term <**> expression'  <|> r' []
  
term = coverList a
  where a = term' >== (\x -> r' (Term x))

term' =
  
  (<->) **>
  satisfy ( (BC.pack "<") == )
  **> (<->) **>
  many1 (satisfy letter)
  **< (<->) **<
  satisfy ( (BC.pack ">") == )
  **< (<->)
  
  
--literal :: Parser
literal = str >== (\x -> r' (Literal x))


-- term' =
  
--   (<->) **>
--   satisfy ( (BC.pack "<") == )
--   **> (<->) **>
--   many1 (satisfy letter)
--   **< (<->) **<
--   satisfy ( (BC.pack ">") == )
--   **< (<->)



--text :: Parser Int
--text = 
  

