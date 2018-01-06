
module Parser_.Python_.SA_.PEP8_.Base where

import Control_.M
import Parser_.Base_.Base
import Parser_.Base_.ByteStr


import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC


till = (<->) **> char "\n" -- **> (<->)


-- W291 trailing whitespace

w291 "" = ""
w291 x =
  case h of
    32        -> w291 t
    otherwise -> x
  where h = BS.head x
        t = BS.tail x
        

-- W293 blank line contains whitespace

w293 "" = ""
w293 x  = ""


-- E501 (line length)

-- e501 = (\x -> r' x)

f = blank <|> com1_ <|> com2_

blank = (<->) **> char "\n"
        
com1_ = between a b c >== (\x -> r' x)
  where a = char "#" 
        b = many (satisfyNot ( == "\n")) --stringNot "\n" --anyLetters
        c = char "\n"
        
          
com2_ = between a b a  >== (\x -> r' x)
  where a = string "\"\"\""
        b = many ( char "\n" <|> satisfyNot ( == "\"" ) )
        l = (<->) **< char "\n"
        
        
        
-- jump
-- till
-- between
-- sep

        
