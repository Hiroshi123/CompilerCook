{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}    


module MData.Parser.List where

import MData.Parser.Base
import MData.Parser.ByteStr

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --()
--import Data.Char

import Data.Word
import MData.M

--instance ParserC [BS.ByteString] where

instance ParserC [a] where
  
  a <**> b =
    a >== 
    (\x -> b >== 
           (\y -> r' $ x ++ y )
    )
    
    
  many x =  many1 x  <|> r' []
  many1 p = p >== (\x -> many p >== (\y -> r' $ x ++ y) )
            
  
  
--------------------------------------------

itemL :: Parser [BS.ByteString]
itemL = Parser $ \x ->
  case x of
    "" -> []
    _  -> [([BS.singleton $ BS.head x],BS.tail x)]
    

satisfyL f = itemL >== (\x -> if f (head x) then r' x else (<>))
    
charL :: BS.ByteString -> Parser [BS.ByteString]
charL c = satisfyL (c == )

stringL :: BS.ByteString -> Parser [BS.ByteString]
stringL (BS.uncons -> Nothing) = r' [BS.empty]
                                 
stringL s = (charL h) **> (stringL t) **> r' [s]
  where h = BS.singleton $ BS.head s
        t = BS.tail s
        
        
--------------------------------------------

bList :: Parser [BS.ByteString]
bList = many1 $ coverList ( skip_space **> many1 ( satisfy letter ) )



