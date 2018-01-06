
module Data_.Parser.Csv where

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)

import Data.Word

-- myModule

import Control_.M

import Parser.Base.Base
import Parser.Base.ByteStr
import Parser.Base.List


-- record = field *(COMMA field)   （2）
-- record :: 

-- escaped = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE （4）
-- non-escaped = *TEXTDATA

-- TEXTDATA = %x20-21 / %x23-2B / %x2D-7E

hei a = a + 1

escaped :: Parser BS.ByteString
escaped = many1 ( satisfy letter )

record :: Parser [BS.ByteString]
record = coverList field <**> record'

record' :: Parser [BS.ByteString]
record' = many1 ( comma **> (coverList field) )

comma :: Parser BS.ByteString
comma = satisfy ( BC.pack "," == )

field :: Parser BS.ByteString
field = (skip_space **> many1 (satisfy letter) **< skip_space)



--field = many1 (skip_space **> (satisfy letter) **< skip_space)

--comma :: Parser 


-- import Data_.Parser.ByteStr

-- output [BS.Bytestring]



