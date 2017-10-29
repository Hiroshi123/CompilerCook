{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Parser_.Protobuf.Proto where

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)

import Data.Word

-- myModule ---------------------------------------

import Control_.M

import Parser_.Base.Base
import Parser_.Base.ByteStr
import Parser_.Base.List
import Parser_.Base.Int

import Data_.BNF.ProtoBuff

--import Data_.Graphviz.Vis

---------- definition of a message of protocol-buffer -------------

-- message is composed of two-parts on its abstruct figure, messageName & list of fields

proto :: Parser [Protobuf]
proto = many $ coverList ( message <|> enum <|> union )

message :: Parser Protobuf
message =
  name_ "message" >== 
  (\x -> paranthesis
    ( many ( field <|> (( union <|> enum <|> message ) >== (\x -> r' [Nested x]) ) )) >==
    (\y -> r' (Message x y) )
  )
  
----- union -----

union =
  name_ "oneof" >== 
  (\x -> paranthesis (many $ coverList field_3) >==
    (\y -> r' ( Oneof x y ) )
  )
  
--------------- enum ---------------

enum =
  name_ "enum" >== 
  (\x -> paranthesis (many $ coverList field_2) >==
    (\y -> r' ( Enum x y ) )
  )
  
-------- message-name extraction (which will return bytestring) -------

name_ :: String -> Parser BC.ByteString
name_ x =
  (<->) **>
  string ( BC.pack x)
  **> (<->) **>
  anyLetters
  <** (<->)
  
paranthesis :: Parser a -> Parser a
paranthesis x =
  (<->) **>
  char (BC.pack "{")
  **> (<->) **> x **< (<->) **<
  char (BC.pack "}")
   **< (<-:>)
  
  -- **<
  -- sem_
  -- **< (<->)
  
-------- a field is composed by 4 information (elements,types,name of field,key) -----
---      which are represented by 4 parser (elements,types,anyLetters,num)   ---------
---      else, you need two additional concrete information which are "=" and ";". ---
  
field :: Parser [Field]
field =
  elements >==
  (\a ->
     (field_3 >==
       (\b -> let (c,d,e) = b in r' [Field a c d e] )
     )
  )

field_3 :: Parser (Type,Bs,Tag)
field_3 =
  types >==
  (\b ->
      field_2 >==
      (\c -> r' (b,fst c,snd c) )
  )
  
field_2 =
  anyLetters >==
  (\c ->
      char_ "=" >==
      (\_ ->
          (num >==
            (\d ->
                (jump "[packed = true]" >==
                 (\_ ->
                     (char_ ";" >==
                       (\_ -> r' ( c, d ) )
                     )
                 )
                )
            )
          )
      )
  )  
  
---- elements parser will return data Elements ---


elements :: Parser Elements
elements =
  (<->) **>
  (or_ elementsList)
  **< (<->)
  
elementsList =
  [
    ("optional",Optional),
    ("required",Required),
    ("repeated",Repeated)
  ]
  
-- actually it is called union in protobuf but, avoiding overraping

  
  
---- types parser will return data types --- 

types :: Parser Type
types =
  (<->) **>
  (or_ typeList) <|> (anyLetters >== (\x -> r' $ Pointer x))
  **< (<->)
  
typeList = fmap (\x -> ( show x , x ) ) ty

ty =
  [
    Int32  , SInt32 , UInt32 , Fixed32 , SFixed32,
    Int64  , SInt64 , UInt64 , Fixed64 , SFixed64 ,
    Bool   , String , Bytes  , Float   , Double
  ]
  
--  selection statement ----

or_ :: [(String, a)] -> Parser a
or_ []    = (<>)
or_ (h:t) =
  (string__ (BC.pack (fst h)) >== (\_ -> r' (snd h)) )
  <|>
  or_ t
  
--------------------------- below are for tests utility --------

comment_rem :: BC.ByteString -> BC.ByteString
comment_rem "" = ""
comment_rem x =
  let h  = BS.head x
      t  = BS.tail x
      th = BS.head t
  in case ( h , th ) of
       ( 47 ,47 ) -> ""
       otherwise  -> BS.cons h (comment_rem t)
       
       
         

