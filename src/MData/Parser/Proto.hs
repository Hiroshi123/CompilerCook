{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module MData.Parser.Proto where

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)

import Data.Word

-- myModule ---------------------------------------

import MData.M
import MData.Parser.Base
import MData.Parser.ByteStr
import MData.Parser.List
import MData.Parser.Int

import MData.BNF.ProtoBuff

---------- definition of a message of protocol-buffer -------------

-- message is composed of two-parts on its abstruct figure, messageName & list of fields


message :: Parser Proto
message =
  messageName >== 
  (\x -> paranthesis >==
    (\y -> r' (Proto x y) )
  )
  
-------- message-name extraction (which will return bytestring) -------

messageName :: Parser BS.ByteString
messageName =
  (<->) **>
  string ( BC.pack "message" )
  **> (<->) **>
  anyLetters
  <** (<->)

-------- list of fields which would follow the name of message (which will data type ; [Field]) -------
  
paranthesis =
  (<->) **>
  char (BC.pack "(")
  **> (<->) **>
  many field
  **< (<->) **<
  char (BC.pack ")")
  **< (<->)  

-------- a field is composed by 4 information (elements,types,name of field,key) -----
---      which are represented by 4 parser (elements,types,anyLetters,num)   ---------


field =
  elements >==
  (\a ->
     (types >==
      (\b ->
          (anyLetters >==
           (\c ->
              (num >==
               (\d -> r' [Field a b c d] )
              )
           )
          )
      )
     )
  )

---- elements parser will return data Elements --- 

elements =
  (<->) **>
  (or_ elementsList)
  **< (<->)

elementsList = [("optional",Optional),("required",Required),("repeated",Repeated)]

---- types parser will return data types --- 
  
types =
  (<->) **>
  (or_ typeList)
  **< (<->)

typeList = fmap (\x -> ( show x , x ) ) ty

ty =
  [
    SInt32 , UInt32 , Fixed32 , SFixed32,
    Int64  , SInt64 , UInt64  , Fixed64 , SFixed64 ,
    Bool   , String , Bytes   , Float   , Double
  ]

--  selection statement ----
  
or_ []    = (<>)
or_ (h:t) =
  (string (BC.pack (fst h)) >== (\_ -> r' (snd h)) )
  <|>
  or_ t
  
  
inp = BS.getLine >>= (\x -> return $ parse field x)



