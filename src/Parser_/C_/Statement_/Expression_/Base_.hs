

module Parser_.C_.Statement_.Expression_.Base_ where


import Parser_.Base_.Base
import Parser_.Base_.Bool
import Parser_.Base_.ByteStr
import Parser_.Base_.Int
import Parser_.Base_.List

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --()

import Data.Word
import Control_.M

import Parser_.C_.Data_.Base_

term_ =
  num >==
  (\x -> r'
         $ Term'
         (Prim' NOst NOql Signed' Int')
         ""
         (Value' 4 [1,0,0,0])
  )
  
prim_type =
  
  prim_type_ "void"   Void'   0  <|>
  prim_type_ "bool"   Bool'   1  <|>
  prim_type_ "char"   Char'   1  <|>
  prim_type_ "short"  Short'  2  <|>
  prim_type_ "long"   Long'   4  <|>
  prim_type_ "int"    Int'    4  <|>
  prim_type_ "float"  Float'  4  <|>
  prim_type_ "double" Double' 8 -- <|>
  
prim_type_ id_ cons b =
  storage_class_ >==
  (\s ->
      type_qualifier_ >==
      (\tq ->
         (sign_ >==
          (\si -> 
              string id_ **> char " " **>
              with_space anyLetters >==
              (\x -> r' $
                Term'
                (Prim' s tq si cons)
                x
                (Value' b [0,0,0,0])
              )
          )
         )
      )
  )
  
identifier_ = with_space anyLetters


struct_union =
  struct_union1 <|>
  struct_union2 <|>
  struct_union3
  
struct_union1 =
  struct_union_declare >==
  (\x1 -> struct_union_define >==
    (\x2 -> struct_union_var >==
      (\x3 -> r' x1)
    )
  )
  
struct_union2 =
  struct_union_declare >==
  (\x1 -> struct_union_define >==
    (\x2 -> r' x1)
  )
  
struct_union3 =
  struct_union_declare >==
  (\x1 -> r' x1)
  
  
struct_union_define =
  definition_ initializer_
  
struct_union_var =
  sepby (char ",") identifier_
  
struct_union_declare =
  struct_union_declare_ "struct" Struct' <|>
  struct_union_declare_ "union"  Union'


-- [(Term' (Derived' NOst NOql Signed' (Struct' [])) "a" (Value' 1 [0]),"a1,a2")]


-- term 
  
struct_union_declare_ s_ cons =
  storage_class_ >==
  (\s ->
      type_qualifier_ >==
      (\tq ->
         (sign_ >==
          (\si ->
             string s_ **>
             struct_union_tag_ >== 
             (\tag -> r' $ -- $ Term'
               (s , tq , si , tag )               
               --tag
               -- (Value' 1 [0])
             )
          )
         )
      )
  )
  
struct_union_tag_ = identifier_ <|> item0 >== (\x -> r' "")

-- >== (\x -> r' (Just x)) <|> Nothing
  
initializer_ = sepby (char ";") prim_type
  
definition_ b = between a b c
  where a = char "{"
        c = char "}"
        
sign_ =
  (string "sign"    >== (\_ -> r' Signed'  ))  <|>
  (string "unsign"  >== (\_ -> r' Unsigned'))  <|>
  (item0  >== (\_ -> r' Signed'))
  

data TypeSpMap =
   TypeSpMap'
   {
     id_  :: Bs,
     cst  :: Prim,
     byte :: Int
   }


a :: [TypeSpMap]
a =
  [
    TypeSpMap' "void" Void' 0,
    TypeSpMap' "char" Char' 1
  ]
  
  
  
--prim_type_specifier_map :: 
prim_type_specifier_map =
  [
    ("void"    ,Void'  ,0 ),
    ("char"    ,Char'  ,1 ),
    ("int"     ,Int'   ,4 ),
    ("short"   ,Short' ,2 ),
    ("long"    ,Long'  ,4 ),
    ("float"   ,Float' ,4 ),
    ("double"  ,Double',8 )
    -- ("signed"  ,Signed'),
    -- ("unsigned",Unsigned')
  ]

type_qualifier_ =
  (string "const"    >== (\_ -> r' Const))    <|>
  (string "volatile" >== (\_ -> r' Volatile)) <|>
  (item0 >== (\_ -> r' NOql))
  
  
storage_class_map :: [(Bs,StorageClass)]
storage_class_map =
  [
    ("auto"    ,Auto    ),
    ("register",Register),
    ("static"  ,Static  ),
    ("extern"  ,Extern  ),
    ("typedef" ,TypeDef )
    
  ]
  
storage_class_ =
  
  (string "auto"     >== (\_ -> r' Auto)   )  <|>
  (string "register" >== (\_ -> r' Register)) <|>
  (string "static"   >== (\_ -> r' Static))    <|>
  (string "extern"   >== (\_ -> r' Extern))   <|>
  (string "typedef"  >== (\_ -> r' TypeDef))  <|>
  (item0             >== (\_ -> r' NOst))
  
  
-- term_ = num >== (\x -> r' $ Term' (Prim' NOst NOql Int') "" (Value' 4 [1,0,0,0]))

-- const value

-- num -> int,float
-- 'a' -> char 
-- []  -> Array
-- 
-- 

