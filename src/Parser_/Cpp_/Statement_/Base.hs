

module Parser_.Cpp_.Statement_.Base where

--import Parser_.Cpp_.Exception_.Base
--import Parser_.Cpp_.Expression_.Base

--statement =


-- labeled_statement    :: Parser_ Statement
-- expression_statement :: Parser_ Statement


compound_statement   :: Parser_ Statement
compound_statement =
  char "{" **> statement **< char "}"
  

--selection_statement  :: Parser_ Statement
--selection_statement =
  
  
if_ = 
  with_space (string "if") **> with_space (char "(") **>
  condition **< with_space (char ")") >==
  (\c ->
      statement_ >==
      (\s1 ->
         r' (e,s1)
      )
  )
  
else_ =
  with_space (string "else") **> statement_
  
  
switch_ =
  with_space (string "switch") **>
  with_space (char "(") **>
  condition **< with_space (char ")")  >==
  (\c -> statement_ >==
    (\s1 ->
      (\c -> r' (c,s1)
      )
    )
  )
  
switch =
  
  with_space (char "(")
  condition
  with_space (char ")")
  statement
  
-- condition ::
-- condition =
--   exression -- <|> or something
  
  
iteration_statement  :: Parser_ Statement
iteration_statement =
  while_ <|> do_while_ <|> for_
  
  
while_ =
  
  with_space (char "(")
  condition **< with_space (char ")") >==
  (\c -> 
     statement >== (\s -> r' (c,s))
  )
  
do_while_ =
  string "do" **> statement_ >==
  (\s -> string "while" **> expression
  )
  
for_ =
  with_space (string "for") **>
  with_space (char "(") **>
  statement_

for_init =
  expression_statement
  --simple_declaration
  
  
jump_statement =
  
  string "break" <|> string "continue" <|>
  string "return" **> expression <|>
  string "goto" **> identifier
  
  
-- declaration_statement =
--   block_declaration
  
-- jump_statement       :: Parser_ Statement

-- declaration_statement :: Parser_ Statement


-- try_block            :: Parser_ Statement




