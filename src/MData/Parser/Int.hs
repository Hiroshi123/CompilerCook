{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}


module MData.Parser.Int where

import MData.Parser.Base
import MData.Parser.ByteStr
import MData.Parser.List


import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --()
--import Data.Char

import Data.Word
import MData.M

instance ParserC Int where
  
  a <**> b =
    a >== 
    (\x -> b >== 
           (\y -> r' $ x * y )
    )
    
    
  many x =  many1 x  <|> r' 0
  
  -- many1 is going to be ended when many1 returns empty list
  
  many1 p = p >== (\x -> many p >== (\y -> r' $ toNum x y ) )
            where toNum x y = x * 10 + y
                  
                  
  --many1 p = p >== (\x -> many p >== (\y -> r' $ foldl toNum 0 y) )
  --          where toNum x y = x * 10 + y
  
  --satisfy :: (BS.ByteString -> Bool) -> Parser BS.ByteString
  --satisfy f = item >== (\x -> if f x then r' 0 else r' 0)
  
  
itemN :: Parser Int
itemN = Parser
  $ \x -> case x of
            "" -> []
            _  -> [ ( fromIntegral $ BS.head x - 48, BS.tail x ) ]
            
            
num'' :: Parser Int
num'' = 
  (satisfy digit)
  >==
  (\x -> r' (toNum x))
  where toNum x = fromIntegral $ BS.head x - 48
  
num' :: Parser [Int]
num' = coverList num'' <**> num' <|> r' []
  
num :: Parser Int
num = num' >== (\x -> r' $ foldl (\a y -> 10 * a + y) 0 x )


      
