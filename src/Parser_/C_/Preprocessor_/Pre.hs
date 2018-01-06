

module Parser_.C_.Combinator_.Preprocessor_.Pre where

import Control_.M
import Parser_.Base_.Base
import Parser_.Base_.Bool
import Parser_.Base_.ByteStr
import Parser_.Base_.List
import Parser_.Base_.Int

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

--import qualified System_.IO as System_

type Bs = BS.ByteString

data DIRECTIVE =
  INCLUDE Bs | COMMENT | MACRO [(Bs,Bs)] | COND (Maybe Bool)
  deriving (Eq,Show)


pre_process c st =
  case c of
    Nothing    -> cond1_ st   <|> pre_process' st
    Just True  -> cond2_ True <|> pre_process' st
    Just False -> cond2_ False
    
    
pre_process' st = comment_c <|> include_ <|> define_ st <|> undef_ st

ret_fname = between a b c
  where a = char "\""
        b = anyLetters
        c = char "\""
        
            
comment_c =
  with_space ( string "//" )
  >== (\_ -> r' COMMENT)  
  
include_ =
  with_space (string "#include ") **>
  ret_fname >== (\x -> r' (INCLUDE x) )
  
define_ st =
  with_space (string "#define")  -- >== (\x -> r' (MACRO ("def","def")))
  **> with_space anyLetters >== -- (\x -> r' (MACRO ("def","def")))
  (\x1 -> with_space anyLetters >== (\x2 -> r' $ MACRO ( [(x1,x2)] ++ st) ) )
  
undef_ st =
  with_space (string "#undef")
  **> with_space anyLetters >==
  (\k -> case lookup k st of
      Nothing -> r' ( MACRO st )
      Just a  -> r' ( MACRO st')
        where st' = remove k st
  )
  
-- ) -- with_space anyLetters >== (\x2 -> r' $ MACRO (x1,x2) ) )


remove :: (Eq k) => k -> [(k,v)] -> [(k,v)]
remove _ [] = []
remove k ((k_,v_):t) =
  case k == k_ of
    True  -> t
    False -> (k_,v_) : (remove k t)
    
             
cond1_ st = ifdef'_ st <|> ifndef'_ st

ifdef'_ st =
  with_space (string "#ifdef ") **>
  with_space anyLetters >==
  (\k -> case lookup k st of
           Nothing -> r' (COND (Just False) )
           Just _  -> r' (COND (Just True ) )
  )
  
ifndef'_ st =
  with_space (string "#ifndef ") **>
  anyLetters >==
  (\k -> case lookup k st of
           Nothing -> r' (COND (Just True  ) )
           Just _  -> r' (COND (Just False ) )
  )
  
  
cond2_ st = else_ st <|> endif_ st

if_ a = a
elif_ a = a

else_  a =
  with_space (string "#else") >==
  (\_ ->
     case a of
       True  -> r' (COND (Just False))
       False -> r' (COND (Just True ))
  )
  
  
endif_ _ = with_space (string "#endif") >== (\_ -> r' (COND Nothing))



--  read_ "ex1.h" >== (\x -> r' $ read_ x)


--  with_space ( string "#include ") >>*
  
  
  
