{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}


module MData.Parser.ByteStr where

import MData.Parser.Base
import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --()
--import Data.Char

import Data.Word
import MData.M

--type Bs = BS.ByteString

instance ParserC BS.ByteString where
  
  -- Conjunction (bytestring version)
  
  a <**> b =
    a >== 
    (\x -> b >== 
           (\y -> r' $ BS.append x y )
    )
    
    
  many x =  many1 x  <|> r' ""
  -- many1 is going to be ended when many1 returns empty list
  many1 p = p >== (\x -> many p >== (\y -> r' $ BS.append x y) )
  
  -- any =
  --   Parser $
  --   \x -> case x of
  --           ""  -> []
  --           _   -> [(BS.singleton $ BS.head x,BS.tail x)]
  
            
----------- Conjunction -------------------------------------------------------------------

  
---------------- following monadic functions are only for bytestring data type ---------------------

-- bottom parser which will produce bytestring Type

item :: Parser BS.ByteString
item =
  Parser $
  \x -> case x of
          ""  -> []
          _   -> [(BS.singleton $ BS.head x,BS.tail x)]

          
items :: Int -> Parser BS.ByteString
items a =
  Parser $
  \x -> case x of
          ""  -> []
          _   -> [(BS.take a x ,BS.drop a x)]
          
          
satisfys' :: Int -> (BS.ByteString -> Bool) -> Parser BS.ByteString
satisfys' a f = items a >== (\x -> if f x then r' x else (<>))

satisfys :: BS.ByteString -> Parser BS.ByteString
satisfys bs =
  satisfys' l ( bs /= )
  where l = BS.length bs
  

-- many is going to be ended when many1 returns empty list
  
char :: BS.ByteString -> Parser BS.ByteString
char c = satisfy (c ==)

stringNot :: BS.ByteString -> Parser BS.ByteString
stringNot "" = r' ""
stringNot s = 
  ( satisfy (h /=) ) >==
  (\_ -> (stringNot t) >==
         (\_ -> (r' s))
  )
  where h = BS.singleton $ BS.head s
        t = BS.tail s
        

satisfy :: (BS.ByteString -> Bool) -> Parser BS.ByteString
satisfy f = item >== (\x -> if f x then r' x else (<>))

satisfyNot :: (BS.ByteString -> Bool) -> Parser BS.ByteString
satisfyNot f = item >== (\x -> if f x then (<>) else r' x )

string :: BS.ByteString -> Parser BS.ByteString
string "" = r' ""
string s = 
  (char h) >==
  (\_ -> (string t) >==
         (\_ -> (r' s))
  )
  where h = BS.singleton $ BS.head s
        t = BS.tail s

        
getALine =
  many (satisfys' 3 ( ( "bbb" :: BS.ByteString ) /= ))
  --many (satisfys ("b " :: BS.ByteString)) -- **< (items 2)
  
  
--alias-----------------------------------------------------

--(.>>) = many
--(>>.) = many1

------------------------------------------------------------
------ Useful functions for general parsing


str :: Parser BS.ByteString
str =
  
  (<->) **>
  char (BS.pack [34])
  **>  (<->) **>
  many1 (satisfy letter)
  **<  (<->) **<
  char (BS.pack [34])
   **< (<->)
  
  
bool' :: Parser BS.ByteString
bool' =
  
  (<->) **>
  string "True"
  <|>
  string "False"
  **< (<->)
  
  
bool :: Parser Bool
bool = bool' >==
  (\x ->
     case BS.head x of 
       84  -> r' True
       70  -> r' False
  )
  
  
skip_space :: Parser BS.ByteString
skip_space = many $ satisfy ((BC.pack " ") == )

(<->) = skip_space
