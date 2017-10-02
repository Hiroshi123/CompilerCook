{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}


module MData.Parser.GhcData where

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)

import Data.Word

-- myModule ---------------------------------------

import MData.M
import MData.Parser.Base
import MData.Parser.ByteStr
import MData.Parser.List
import MData.Parser.Int

import MData.BNF.Json

f1 =
  
  (<->) **> 
  string "data "
  
  **> (<->) **>
  rakudaL
  **< typeParam **<
  string (BC.pack "= ")
  **>
  dataConst
--  **>
  
--  many
  
  
rakudaL = (<->) **> satisfy lCap <**> many (satisfy letter) **< (<->)

dataConst = rakudaL

typeParam = 
  
  (<->) **>
  many (satisfy sCap)
  **< (<->)
  
  
