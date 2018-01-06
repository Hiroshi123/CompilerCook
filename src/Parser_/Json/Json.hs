{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}


module Parser_.Json.Json where

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)

import Data.Word

-- myModule

import Control_.M

import Parser_.Base.Base
import Parser_.Base.ByteStr
import Parser_.Base.List
import Parser_.Base.Int

import Data_.BNF.Json

------------------------------------------------------
-- jvalue --

-- instance ParserC JValue where  
--   many x =  many1 x  <|> r' ""
--   many1 p = p >== (\x -> many p >== (\y -> r' $ BS.append x y) )
       
-- jvalues = jvalue

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
  (sepby key_val comma) >== (\x -> r' $ JObject x)
  **< (<->) **<
  char (BC.pack "}")
  **< (<->)
  
  
jarray :: Parser JValue
jarray =
  
  (<->) **>
  char (BC.pack "[")
  **> (<->) **>
  sepby jvalue comma >== (\x -> r' $ JArray x)
  -- <|> r' (JArray [])
  **< (<->) **<
  char (BC.pack "]")
  **< (<->)
  
  
key_val_s = coverList key_val

key_val =
  
  key >==
  (\x -> val >== 
         (\y -> r' $ (x, y) )
  )
  
  
key :: Parser BS.ByteString  
key =
  (<->) **>
  str --anyLetters
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
  
  
