

module Parser_.Cpp_.Class_.Base where

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

class_specifier =
  class_head >==
  (\x -> class_body >==
    (\y -> r' (x,y))
  )
  
class_body = between a b c
  where a = char "{"
        b = member_specification
        c = char "}"
        
member_specification =
  access_specifier
  
--   access_specifier

-- member_declaration =
  
class_head =
  with_space class_key  **>
  with_space identifier **>
  class_body
  
  
-- {};
  
class_key =
  string "class"  <|>
  string "struct" <|>
  string "union"
  
--derived classes

data AccessSpecifier =
  Private | Public | Protected
  deriving (Show)

access_specifier =
  private   <|>
  protected <|>
  public
  
private   = with_space (string "private")   **> with_space (char ":") >== (\_ -> r' Private)
protected = with_space (string "protected") **> with_space (char ":") >== (\_ -> r' Protected)
public    = with_space (string "public")    **> with_space (char ":") >== (\_ -> r' Public)
  
identifier = anyLetters

  
