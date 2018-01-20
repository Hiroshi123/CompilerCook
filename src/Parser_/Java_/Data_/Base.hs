


--data Declaration =

data Modifier =
  Public | Private | Protected | Static | Final |
  Native | Synchronized | Abstract | Threadsafe | Transient
  deriving (Show)

data Type =
  Bool' | Char' | Byte' | Short' |
  Int'  | Float'| Long' | Float' |
  Double'
  deriving (Show)


modifier :: Parser Modifier
modifier =
  
  (string "public"    >== (\_ -> r' Public   ) )  |
  (string "private"   >== (\_ -> r' Private  ) )  |
  (string "protected" >== (\_ -> r' Protected) )  |
  (string "static"    >== (\_ -> r' Static   ) )  |
  (string "final"     >== (\_ -> r' Final    ) )  |
  (string "native"    >== (\_ -> r' Native   ) )  |
  
  (string "synchronized"    >== (\_ -> r' Synchronized   ) )  |
  (string "abstract"    >== (\_ -> r' Abstract   ) )  |
  
  (string "threadsafe"    >== (\_ -> r' Threadsafe   ) )  |
  
  (string "transient"    >== (\_ -> r' Transient   ) )
  
  
extends = string "extends"

implements = string "implements"

field_declaration =
  
  method_declaration |
  constructor_declaration |
  variable_declaration |
  static_initializer

method_declaration =
  modifier
  (\m ->
     type_ >== (\)
     identifier >== 
  )
  
type_ =
  
  ( string "boolean" >== (\_ -> r' Boolean') )  |
  ( string "char"    >== (\_ -> r' Char') )     |
  ( string "byte"    >== (\_ -> r' Byte') )     |
  ( string "short"   >== (\_ -> r' Short' ) )   |
  ( string "int"     >== (\_ -> r' Int'   ) )   |
  ( string "long"    >== (\_ -> r' Long'  ) )   |
  ( string "float"   >== (\_ -> r' Float' ) )   |
  ( string "double"  >== (\_ -> r' Double') )
  
  
constructor_declaration =
  modifier >==
  (\m ->
      identifier >==
      (\i ->
         char "(" **> parameter_list
         char ")"
      )
  )
  
parameter_list = sepby parameter (char ",")

statement_block =
  char "{" char "}"
  
  
f = string "class" **> identifier

identifier = 


