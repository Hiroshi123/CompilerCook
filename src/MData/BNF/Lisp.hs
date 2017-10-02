{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module MData.BNF.Lisp where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

import qualified Data.Char
import Data.Word

import MData.M
import MData.Parser.Base
import MData.Parser.ByteStr
import MData.Parser.List
import MData.Parser.Int


--import MData.BNF.Lisp

-- lots of irritating superfluous parentheses

-- ' -> Following parenthesis are not going to be evaluated
-- '( 3  "a" 'abc )

-- without quotation, functions are going to be instantly evaluated 
-- (car '(3 4)) -> return 3

-- if there is no quation before parenthesis, there might be 4 followings cases have to be
-- 1st element of the list.

-- predefined function ->

-- car
-- cdr
-- cons

--(define (my-cons a d) (lambda (f) (f a d)))
--(define (my-car ad) (ad (lambda (a d) a)))
--(define (my-cdr ad) (ad (lambda (a d) d)))

-- eq
-- atom

-- pre-defined operator ->
-- macro ->

-- special form -> if () () ()

-- #'(function) denotes (function)

-- closure
-- (defun f (x y) (+ x y))

type Bs = BS.ByteString

data Lisp
  = LList  Lfunc
  | LAtom  Atom -- BS.ByteString
  | Quote  Symbol
  | PrimF  PreFunc 
--  | FQuote PreFunc
  | Cell Lisp Lisp
  | Nil
  deriving (Show)


data Symbol = Symbol Bs deriving (Show)

type LEnv = [( Bs , Lisp )]
type GEnv = [( Bs , Lisp )]

data Lfunc =
    LF PreFunc 
  | SP SpecialForm
  deriving (Show)


car' (Cell x y) = x
cdr' (Cell x y) = y
cons' x y = (Cell x y)


car'' =
  string ("car " :: Bs) **>
  cell >==
  (\x -> case x of
      Cell h t -> r' h
  )
  
  
cdr'' =
  string ("cdr " :: Bs) **>
  cell >==
  (\x -> case x of
      Cell h t -> r' t
  )
  
  
cons'' =
  string ("cdr " :: Bs) **>
  cell >==
  (\x -> cell >==
    (\y -> r' $ Cell x y)
  )
  
  
data PreFunc =
    CAR  ( Lisp -> Lisp )
  | CDR  ( Lisp -> Lisp )
  | CONS ( Lisp -> Lisp )
  | EQ   ( Lisp -> Lisp )
  | Hu
  --deriving (Show)
  
  
instance Show PreFunc where
  show (CAR x) = "car " -- ++ (show x)
  show (Hu) = "hei!"
  
data SpecialForm =
    Lambda (GEnv -> LEnv -> Lisp -> (Lisp , GEnv) )  -- Either Bs (Lisp, GEnv))
  | Defun  (GEnv -> LEnv -> Lisp -> (Lisp , GEnv) )  -- Either Bs (Lisp, GEnv))
  | Cond   (GEnv -> LEnv -> Lisp -> (Lisp , GEnv) )  -- Either Bs (Lisp, GEnv))
  --deriving (Show)

instance Show SpecialForm where
  show (Lambda x) = "Lambda!"
  show (Defun x)  = "Lambda!"
  
car =
  string ("car " :: Bs) >==
  (\_ -> cell >==
    (\x -> r' $ Cell ( PrimF (CAR car') ) x )
  )
  

-- CAR  car'
-- CDR  cdr'
-- Cons cons'
  
deff = Parser $ \x -> [(Nil,x)]

atom = lstr <|> lnum <|> lbool  -- <|> Nil

ele = (<->) **> atom <|> cell **< (<->)
  
cell2 =
  ele >==
  (\x -> ele >==
    (\y -> r' (Cell x y))
  )
  
  
lstr  = str >== (\x -> (r' $ LAtom (LString x)) )
lnum  = satisfy digit >== (\x -> (r' $ LAtom (LNum x)) )
lbool = char ("t" :: Bs) >== (\x -> (r' $ LAtom (LBool x)) )

--lnum = satisfy digit >== (\x -> (r' $ LAtom (LNum x)) )

--cell :: Parser a -> Parser a
cell =
  
  (<->) **>
  char ("(" :: Bs)
  **> (<->) **>
  cell2
  --item >== (\x -> r' $ Cell Nil Nil)
  **< (<->) **<
  char (")" :: Bs)
  **< (<->)
  

data Atom =
  LNum Bs
--  | Symbol Bs
  | LString Bs
  | LBool Bs
  | LVar Bs
  | LNil
  deriving (Show)

type ArgRet = [Lisp]

data OpeCode =
  OPL Logical
  | OPA Arithmetic
  deriving (Show)

data Logical = 
    And Logical Logical
  | Or  Logical Logical
  | W Word8
  | L Lisp
  --deriving (Show)  
  
data Arithmetic = 
    Add Arithmetic Arithmetic
  | Sub Arithmetic Arithmetic
  | Mul Arithmetic Arithmetic
  | Div Arithmetic Arithmetic
  | N Word8
  -- deriving (Show)
  
instance Show Logical where
  
  show (W x) = " " ++ show x ++ " "
  show (And x y) = "( ∧ " ++ (show x) ++ (show y) ++ " )"
  show (Or  x y) = "( ∨ " ++ (show x) ++ (show y) ++ " )"
  
instance Show Arithmetic where
  
  show (N x) = " " ++ show (x - 48) ++ " "
  show (Add x y) = "( + " ++ (show x) ++ (show y) ++ " )"
  show (Sub x y) = "( - " ++ (show x) ++ (show y) ++ " )"
  show (Mul x y) = "( * " ++ (show x) ++ (show y) ++ " )"
  show (Div x y) = "( / " ++ (show x) ++ (show y) ++ " )"
  
  -- show (Sub x y) = "+" ++ x ++ y
  -- show (Mul x y) = "+" ++ x ++ y
  -- show (Div x y) = "+" ++ x ++ y
  
-- s_expression = atomic_symbol \
--               / "(" s_expression "."s_expression ")" \
--               / list 

-- list = "(" s_expression < s_expression > ")"

-- atomic_symbol = letter atom_part

-- atom_part = empty / letter atom_part / number atom_part

-- letter = "a" / "b" / " ..." / "z"

-- number = "1" / "2" / " ..." / "9"

-- empty = " "


