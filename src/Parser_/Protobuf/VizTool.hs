{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Parser_.Protobuf.VizTool where


import Data_.BNF.ProtoBuff

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)


make_edge :: Protobuf -> [(String,String)]
make_edge (Message name field) = fmap (\x -> (make_edge' x,n)) field
  where n = BC.unpack name
  
make_edge' field =
  case field of
    (Field ele tp n tag) -> BC.unpack n
    otherwise            -> ""

make_node :: Protobuf -> [(String,String)]
make_node (Message name field) = (n,n) : a
  where n = BC.unpack name
        a = fmap (\x -> let xx = make_node' x in (xx,xx) ) field
        
  
make_node' field =
  case field of
    (Field ele tp n tag) -> BC.unpack n
    otherwise            -> ""
    

