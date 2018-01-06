

module System_.IO where

import System.IO
import System.Directory
import System.Process

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC

import Parser_.Base.Preprocess

hGetLines :: Handle -> IO [BC.ByteString]
hGetLines h =
  hIsEOF h >>=
  (\x ->
      case x of
        True  -> return []
        False ->
          BS.hGetLine h >>=
          (\x ->
             --let xx = comment_rem x
             hGetLines h >>=
                (\xs -> return (x:xs))
          )
  )
  
  
save :: [Char] -> [Char] -> IO ()
save fn c =
  getCurrentDirectory >>= 
  (\cd ->
     withFile (cd ++ "/" ++ fn) WriteMode (\h -> BS.hPutStr h $ BC.pack c)
     -- (\h -> BS.hPutStr h $ BC.pack contents) -- (\h -> BS.hPutStr h $ BC.pack contents)
  )
  
  
