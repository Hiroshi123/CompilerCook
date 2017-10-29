

import System.IO  
import System_.IO (hGetLines)
import Parser_.Base.Base (parse)
import Parser_.Json.Json (jvalue)

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)


path = "example/json/in/"

unlines_ x = foldr1 (\a b -> BS.append a b) x

parse_ x = parse jvalue x

main =
  withFile (path ++ "ex3.json") ReadMode
  (\h -> hGetLines h >>=
    (\x -> return $ parse_ (unlines_ x) --mapM_ print (f x)
    )
  )
  
  
