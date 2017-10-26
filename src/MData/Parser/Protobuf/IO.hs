{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module MData.Parser.Protobuf.IO where

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)

import MData.M
import MData.Parser.Base
import MData.Parser.ByteStr
import MData.Parser.List
import MData.Parser.Int

import MData.Parser.Protobuf.VizTool
import MData.Parser.Protobuf.Proto

import MData.Graphviz.Vis
import System.IO
import System.Directory
import System.Process


hGetLines :: Handle -> IO [BC.ByteString]
hGetLines h =
  hIsEOF h >>=
  (\x ->
      case x of
        True  -> return []
        False ->
          BS.hGetLine h >>=
          (\x ->
             let xx = comment_rem x
             in hGetLines h >>=
                (\xs -> return (xx:xs))
          )
  )
  
save :: [Char] -> [Char] -> IO ()
save fn c =
  getCurrentDirectory >>= 
  (\cd ->
     withFile (cd ++ "/" ++ fn) WriteMode (\h -> BS.hPutStr h $ BC.pack c)
     -- (\h -> BS.hPutStr h $ BC.pack contents) -- (\h -> BS.hPutStr h $ BC.pack contents)
  )
  
f x = save "out/dot/ex1.dot" (make_graph e)
  where c = fst $ head $ parse proto $ foldr1 (\a b -> BS.append a b) x
        d = fmap (\x -> (make_node x , make_edge x)) c
        e = fmap make_subgraph $ zip ["title1","title2"] d
        
        
main =
  withFile "data/protobuf/ex0.proto" ReadMode
  
  --(\h -> hGetLines h >>= (\x -> return $ foldr1 (\a b -> BS.append a b) x) )
  --(\h -> hGetLines h >>= (\x -> return $ parse messages $ foldr1 (\a b -> BS.append a b) x) )
  
  (\h -> hGetLines h >>=
    (\x -> f x --mapM_ print (f x)
    )
  )
  
  
inp = BS.getLine >>= (\x -> return $ parse (string__ (BC.pack "hei")) x)

svgout = rawSystem "dot" ["./out/dot/ex1.dot","-T","svg","-o","./out/svg/ex1.svg"]

open = rawSystem "open" ["./out/svg/ex1.svg"]


