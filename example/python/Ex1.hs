

import System_.IO

import System.IO
import System.Directory

import Parser_.Base_.Base

import Parser_.Python_.SA_.PEP8_.Base


import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC

unlines_ x = foldr1 (\a b -> BS.append a b) x

unlines__ x = foldl (\s a -> BS.append (BS.append s a) "\n" ) "" x


read_ ps path =
  withFile path ReadMode
  (\h -> hGetLines h >>=
    (
      -- (\x -> return $ x )
      (\x -> return $ parse (many ps) (unlines__ x) )
      -- \x -> return ( parse ps (unlines x) )
      --mapM_ print (f x)
    )
  )

  
  
