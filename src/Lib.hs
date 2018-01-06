{-# LANGUAGE OverloadedStrings #-}

module Lib
    (
      someFunc
    ) where


-- import MData.Parser.Base

-- Either one should be picked up

--import MData.Parser.ByteStr

-- import MData.M
-- import MControl.State

-- import MData.Parser.List
-- import MData.Parser.ByteStr
-- import MData.Parser.Int

-- import MData.BNF.ProtoBuff

--import MData.Parser.Csv
--import MData.Parser.Json
--import MData.Parser.BNF

--import MData.Parser.GhcData

--import MData.BNF.MessagePack

--import MData.BNF.Lisp 

-- import MData.Parser.Lisp
-- import MData.BNF.Lisp2


-- import qualified Data.ByteString as BS --(ByteString,unpack)
-- import qualified Data.ByteString.Char8 as BC --()

-- import System.IO

-- ff :: Parser [BS.ByteString]
-- ff =  many1 $ stringL $ BC.pack ""

-- inp = BS.getLine >>= (\x -> return $ parse ff x)


someFunc :: IO ()
someFunc = putStrLn "someFunc"
