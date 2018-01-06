

module Parser_.C_.Data_.Base_ where

import Parser_.Base_.Base
import Parser_.Base_.Bool

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

import Data.Word
import Control_.M

type Bs = BS.ByteString

--2.  !   ~   -   +   *   &   sizeof   type cast   ++   --


-- Identifier


data Expr =
  
    Term' Type Identifier Value
  | Identifier' Type Bs
  | Epsilon
  
  -------------------------------
  --1.
  
  | Call' Type Expr Expr
  | Elem' Type Expr Expr
  | Dref' Type Expr Expr
  | Uref' Type Expr Expr
  
  --post placed operator
  | PostInc' Type Expr Expr
  | PostDec' Type Expr Expr
  
  -------------------------------
  --2.
  
  -- pre placed operator
  | NotB'   Type Expr Expr
  | NotL'   Type Expr Expr
  | Neg'    Type Expr Expr
  | Pos'    Type Expr Expr
  | Ptr''   Type Expr Expr
  
  | Adr'    Type Expr Expr
  | Sizeof' Type Expr Expr
  | Cast'   Type Expr Expr
  | PreInc' Type Expr Expr
  | PreDec' Type Expr Expr
  
  -------------------------------
  --3.
  
  | Mul'   Type Expr Expr
  | Div'   Type Expr Expr
  | Mod'   Type Expr Expr
  
  -------------------------------
  --4.
  
  | Add'   Type Expr Expr
  | Sub'   Type Expr Expr
  
  -------------------------------
  --5.
  
  | ShiftL' Type Expr Expr
  | ShiftR' Type Expr Expr

  -------------------------------
  --6.
  
  | Greater'   Type Expr Expr
  | EqGreater' Type Expr Expr
  | Less'      Type Expr Expr
  | EqLess'    Type Expr Expr
  
  -------------------------------
  --7.
  
  | Eq'    Type Expr Expr
  | NotEq' Type Expr Expr
  
  -------------------------------
  --8.
  | AndB'  Type Expr Expr
  -------------------------------
  --9.
  | XorB'   Type Expr Expr
  -------------------------------
  --10.
  | OrB'   Type Expr Expr
  -------------------------------
  --11.
  | AndL'  Type Expr Expr
  -------------------------------
  --12.
  | OrL'   Type Expr Expr
  
  -------------------------------
  --13.
  | Assign' Type Expr Expr
  
  -------------------------------
  --14.
  | Comma' [Type] Expr Expr
  
  deriving (Show)


data Type =
    Prim' StorageClass TQualifier Sign Prim
  | Derived' StorageClass TQualifier Sign Derived
  deriving (Show)

data StorageClass =
  Auto | Register | Static | Extern | TypeDef | NOst
  deriving (Show)

data TQualifier =
  Const | Volatile | NOql
  deriving (Show)

data Sign = Signed' | Unsigned'
  deriving (Show)


data Prim =
    Void'
  | Bool'   --Word8
  | Char'  --Word8
  
  | Int'      --Word16
  
  | Short' --Word16
  
  | Long'  --Word32
  
  | Float'  --Word32
  
  | Double' --Word64

  | Enum' Int
  
  deriving (Show)


data Derived =
    Array' Int  Type
  | Ptr'        Type
  | Struct'     Expr
  -- | EnumT   
  | Union'     [Type]
  | Function'  [Type] Type
  deriving (Show)

data Statement =
    Label'   Identifier     Statement
  | Case'    Expr           Statement
  | Default' Expr           Statement
  | Sel'     Expr Statement Statement
  | Switch'  Expr Statement Statement
  | While'   Expr Statement Statement
  | DoWhile' Expr Statement Statement
  | For'     Expr Expr Expr Statement Statement
  -- Statement'
  | Goto'     Identifier Statement
  | Continue' Statement
  | Break'    Statement
  | Return'   Expr Statement
  | Compound' Statement Statement
  | ExprST'   Expr
  deriving (Show)

type Identifier = Bs

data Declaration =
    DataDecl' Type Identifier Initializer
  | FuncDecl' Type Identifier Statement
  | Expr
  deriving (Show)


data Initializer =
    Init' Initializer Initializer
  | Type Identifier
  deriving (Show)

-- data BasicBlock =
--   | Cond' Expr BasicBlock BasicBlock BasicBlock
--   | Statement
--   deriving (Show)

data State_ =
    State' State_ State_
  | LeafExpr Expr
  deriving (Show)


data Value =
  Value' Int [Word8]
  deriving (Show)

  
