

module Parser_.C_.Combinator_.Base where


import Control_.M
import Parser_.Base_.Base
import Parser_.Base_.Bool
import Parser_.Base_.ByteStr
import Parser_.Base_.List
import Parser_.Base_.Int


import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC


import Parser_.C_.AST_.Base


f =
  
  with_space (string "int ") **>
  with_space (string "main") **>
  with_space (string "()") **>
  between a b c
  where a = with_space ( char (BS.pack [123]) )
        b = with_space anyLetters
        c = with_space ( char (BS.pack [125]) )
        
        
--var_declaration :: Parser Python.Expr

ptr =
  with_space type_  >==
  (\t -> with_space (char "*") >==
         (\_ -> with_space anyLetters >==
                (\x -> r' t )
         )
  )
  
allocation =
  type_ >==
  (\t -> with_space anyLetters >==
         (\n -> r' $ Allocate t n)
  ) **< with_space ( satisfy semicolon )
  

as = allocation store_
  
store_ =
  with_space (satisfy equal) >==
  (\_ -> with_space  ( num ) >==
         (\x__ -> with_space (satisfy semicolon) >==
                  (\_ -> r' $ Var t x x__)
         )
  )
  
type_ = int_ <|> char'_ <|> float_

int_   = with_space (string "int "  ) >== (\_ -> r' Int )
char'_ = with_space (string "char " ) >== (\_ -> r' Char)
float_ = with_space (string "float ") >== (\_ -> r' Float)

--float_ = with_space (string "float ") >== (\_ -> r' Float)

f1 = between a b c
  where a = with_space ( char (BS.pack [123]) )
        b = with_space allocation
        c = with_space ( char (BS.pack [125]) )
        
--defFunc_ :: Parser C_.Expr
-- defFunc_ :: Parser Python.Expr

defFunc_ =
  
  type_ >==
  (\a -> with_space anyLetters >==
         (\b -> args_ >==
                (\c -> f1 >==
                       (\d  -> r' $ DefFunc $ Func a b c [d])
                )
         )
  )
  
  
args_ =
  
  with_space ( char (BC.pack "(") ) **>
  sepby anyLetters comma >== (\x -> r' $ Args x) **<
  with_space ( char (BC.pack ")") )
  

