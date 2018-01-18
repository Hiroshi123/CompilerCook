

module Parser_.C_.AST_.Type where

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC

data T = PrimT' PrimT | DerivedT' DerivedT
  deriving (Show,Eq)


data DerivedT =
    Array'  Array
  | Func'   Func
  | Ptr'    Ptr
  | Struct' Struct
  | Vec'    Vec
  | Opaque' Opaque
  deriving (Show,Eq)

data Array = Array'' (ElemN,T)
  deriving (Show,Eq)

data Func = Func'' T [T]
  deriving (Show,Eq)

data Ptr = Ptr'' T
  deriving (Show,Eq)

data Struct = Struct'' [T]
  deriving (Show,Eq)

data Vec  = Vec'' (ElemN,T)
  deriving (Show,Eq)

data Opaque = Opaque''
  deriving (Show,Eq)

data PrimT =
    Int'   Int_T
  | FP'    FP_T
  | Void'  Void_T
  | Label' Label_T 
  -- X86mmx
  -- metadata
  deriving (Show,Eq)


data Int_T = Int'' BitN
  deriving (Show,Eq)


data Void_T = Void
  deriving (Show,Eq)

data Label_T = Label
  deriving (Show,Eq)

data FP_T =
    Half
  | Float
  | Double
  -- X86_fp80
  -- fp128
  -- ppc_fp128
  deriving (Show,Eq)


type BitN  = Int
type ElemN = Int

type FName   = BS.ByteString
type VarName = BS.ByteString



