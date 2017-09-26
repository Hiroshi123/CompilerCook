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

------------------------------------------------------
-- jvalue --

jvalue = jobject <|> jarray <|> jelem

jelem :: Parser JValue
jelem = jbool <|> jstr <|> jnum

------------------------------------------------------

jbool :: Parser JValue
jbool = bool >== (\x -> r' $ JBool x )
  
jnum :: Parser JValue
jnum  = num >== (\x -> r' $ JInt x )

jstr :: Parser JValue
jstr  = str >== (\y -> r' $ JString y )

jobject :: Parser JValue -- BS.ByteString --JValue
jobject =
  
  (<->) **>
  char (BC.pack "{")
  **> (<->) **>
  key_val
  **< (<->) **<
  char (BC.pack "}")
  **< (<->)
  
  
jarray :: Parser JValue
jarray =
  
  (<->) **>
  char (BC.pack "[")
  **> (<->) **>
  jvalue
  **< (<->) **<
  char (BC.pack "]")
  **< (<->)
  
  
  
key_val :: Parser JValue
key_val =
  
  key >==
  (\x -> val >== 
         (\y -> r' $ JObject [(x, y)] ) 
  )
  
  
key :: Parser BS.ByteString  
key =
  (<->) **>
  str
  **< (<->) **<
  char (BS.pack [58])
  **< (<->)

--[34] is "\""
--[58] is ":"


val :: Parser JValue
val =
  
  (<->) **>
  jvalue
  **< (<->)
  


