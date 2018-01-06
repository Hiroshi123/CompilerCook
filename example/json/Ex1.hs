

import System.IO  
import System_.IO -- (hGetLines)
import Parser_.Base.Base (parse)
import Parser_.Json.Json (jvalue)

import Parser_.Json.VizTool
--import Data_.Graphviz.Vis
import System_.IO
import System.Process

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)

path = "example/json/in/"

unlines_ x = foldr1 (\a b -> BS.append a b) x

--g = save "example/json/out/dot/ex1.dot" ff

parse_ x = parse jvalue x

tm x = save "example/json/out/dot/ex1.dot" a
--tm x = save "example/json/out/dot/ex1.dot" a
  where a = ff x
  
parse__ x = fst $ head $ parse_ (unlines_ x)
            
main =
  withFile (path ++ "ex4.json") ReadMode
  (\h -> hGetLines h >>=
    (
      \x -> tm $ parse__ x --mapM_ print (f x)
    )
  )
  
  
g_path = "./example/json/out/"

svgout = rawSystem "dot" [g_path ++ "dot/ex1.dot","-T","svg","-o",g_path ++ "svg/ex1.svg"]

open = rawSystem "open"  [ g_path ++ "svg/ex1.svg" ]


  
