
module Lib
    ( someFunc
    ) where


import MData.Parser.Base

-- Either one should be picked up

--import MData.Parser.ByteStr

import MData.M
import MData.Parser.List
import MData.Parser.ByteStr
import MData.Parser.Int

import MData.Parser.Csv
import MData.Parser.Json
import MData.Parser.BNF


import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --()


--input = BS.getLine >>= (\x -> return $ parse (manyL (itemL) <**> itemL) x)

--inpu :: [(BS.ByteString,)]

ff :: Parser [BS.ByteString]
ff =  many1 $ stringL $ BC.pack "abcd"

inp' = BS.getLine >>= (\x -> return $ parse record x)

inp'' = BS.getLine >>= (\x -> return $ parse itemN x)

--inp = BS.getLine >>= (\x -> return $ parse num x)

inp = BS.getLine >>= (\x -> return $ parse jvalue x)

--inpu = BS.getLine >>= (\x -> return $ (parse (itemL <**> (string $ BC.pack "abc"))) x)
--inpu = BS.getLine >>= (\x -> return $ parse (strings (BC.pack "abc")) x)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
