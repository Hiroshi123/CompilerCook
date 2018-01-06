

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
import Parser_.C_.AST_.Type

f =
  
  with_space (string "int ") **>
  with_space (string "main") **>
  with_space (string "()") **>
  between a b c
  where a = with_space ( char (BS.pack [123]) )
        b = with_space anyLetters
        c = with_space ( char (BS.pack [125]) )
        
--var_declaration :: Parser Python.Expr

-- Ptr = Ptr'' T
-- Ptr''
-- Ptr' $ Ptr'' ttt

struct' =
  with_space (string "struct ") **>
  with_space anyLetters >==
  (\x -> f1 >== (\_ -> r' x)
  )
  
  
ptr =
  with_space type_  >==
  (\t -> with_space (char "*") >==
         (\_ -> with_space anyLetters >==
                (\x -> r' $ Ptr'' t )
         )
  )
  
  
ptr_ = ptr >== (\x -> r' (Ptr' x))

ptr__ = ptr_ >== (\x -> r' (DerivedT' x))

allocation =
  prim_type >==
  (\t -> with_space anyLetters >==
         (\n -> r' $ MemAccess' $ Allocate ttt "")
  ) **< with_space ( satisfy semicolon )
  
  
-- as = allocation store_
  
-- store_ =
--   with_space (satisfy equal) >==
--   (\_ -> with_space  ( num ) >==
--          (\x__ -> with_space (satisfy semicolon) >==
--                   (\_ -> r' $ Var t x x__)
--          )
--   )


type_ :: Parser T
type_  = int__ <|> ptr__


prim_type :: Parser PrimT
prim_type = int_ <|> float_

--derived_type :: Parser DerivedT
--derived_type = ptr >== (\x -> r' $ Ptr' x)

int__   = with_space (string "int "  ) >== (\_ -> r' $ PrimT' $ Int' $ Int'' 8)

--prim_type = int_ <|> char'_ <|> float_


int_   = with_space (string "int "  ) >== (\_ -> r' $ Int' $ Int'' 8)
--char'_ = with_space (string "char " ) >== (\_ -> r' Char'')
float_ = with_space (string "float ") >== (\_ -> r' $ FP' Float)

-- --float_ = with_space (string "float ") >== (\_ -> r' Float)

f1 = between a b c
  where a = with_space ( char "{" )
        b = with_space allocation
        c = with_space ( char "}" )
        
          
-- -- defFunc_ :: Parser C_.Expr
-- -- defFunc_ :: Parser Python.Expr

ttt = PrimT' (Int' (Int'' 3))


defFunc_ =
  
  type_ >==
  (\a -> with_space anyLetters >==
         (\b -> args_ >==
                (\c -> item >==
                       (\d  -> r' $ Define' $ Func'' a [ ttt ] )
                       --(\d  -> r' $ Define $ Func a b c [d])
                )
         )
  )
  
  
args_ =
  
  with_space ( char (BC.pack "(") ) **>
  sepby anyLetters comma >== (\x -> r' $ x) **<
  with_space ( char (BC.pack ")") )
  
  
  
