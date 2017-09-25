{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}


module MData.Parser.Json where

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)

import Data.Word

-- myModule

import MData.M
import MData.Parser.Base
import MData.Parser.ByteStr
import MData.Parser.List
import MData.Parser.Int

import MData.BNF.Json

        
--jvalue            
jvalue = object <|> array 

jelem :: Parser JValue
jelem = jbool <|> jstr <|> jnum
  
jnum :: Parser JValue
jnum  = num >== (\x -> r' $ JInt x )

jstr :: Parser JValue
jstr  = str >== (\y -> r' $ JString y )

jbool :: Parser JValue
jbool = bool >== (\x -> r' $ JBool x )

object :: Parser BS.ByteString --JValue
object =
  
  (<->) **>
  char (BC.pack "{")
  **> (<->) **>
  c
  **< (<->) **<
  char (BC.pack "}")
  **< (<->)
  
  
array :: Parser BC.ByteString
array =
  
  (<->) **>
  char (BC.pack "[")
  **> (<->) **>
  c
  **< (<->) **<
  char (BC.pack "]")
  **< (<->)
  
  
c :: Parser BS.ByteString
c = 
  
  char (BS.pack [34])
  **> (<->) **>
  many1 (satisfy letter)
  **< (<->) **<
  char (BS.pack [34])
  **< (<->) **<
  char (BS.pack [58])


key =
  key' >==
  (\x -> key'' >== 
         (\y -> r' $ JObject [(x, y)] ) 
  )
  
  
key' =
  (<->) **>
  str
  **< (<->) **<
  char (BS.pack [58])
  **< (<->)
  
key'' =
  
  (<->) **>
  char (BS.pack [58])
  **> (<->) **>
  jnum --jvalue
  **< (<->)
  
  
--[34] is "\""
--[58] is ":"


