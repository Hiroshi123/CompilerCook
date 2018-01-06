{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}


--module MData.Parser.Protobuf.IO where

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)

import System.IO
import System.Directory
import System.Process

import Parser_.Base.Base

import Parser_.Protobuf.Proto
import Parser_.Protobuf.VizTool (make_node,make_edge)

import Data_.Graphviz.Vis

import System_.IO

unlines_ x = foldr1 (\a b -> BS.append a b) x
load x = fst $ head $ parse proto x

f x = save "example/protobuf/out/dot/ex1.dot" (make_graph e)
  where c = load (unlines_ x)
        d = fmap (\x -> (make_node x , make_edge x)) c
        e = fmap make_subgraph $ zip [("0","message A"),("1","message B")] d
        
        
main =
  withFile "example/protobuf/in/ex0.proto" ReadMode
  
  --(\h -> hGetLines h >>= (\x -> return $ foldr1 (\a b -> BS.append a b) x) )
  --(\h -> hGetLines h >>= (\x -> return $ parse messages $ foldr1 (\a b -> BS.append a b) x) )
  
  (\h -> hGetLines h >>=
    (\x -> f x --mapM_ print (f x)
    )
  )
  
  
--inp = BS.getLine >>= (\x -> return $ parse (string__ (BC.pack "hei")) x)

svgout = rawSystem "dot" ["./example/protobuf/out/dot/ex3.dot","-T","svg","-o","./example/protobuf/out/svg/ex3.svg"]

open = rawSystem "open" ["./example/protobuf/out/svg/ex3.svg"]


