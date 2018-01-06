

module Parser_.C_.AST_.Base where

import Parser_.C_.AST_.Type


import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC

type Bs = BS.ByteString
type Val = Bs

type Fn = [Char]

data Module
  = Module' Fn (Maybe SymbolTree) (Maybe MemoryTree) Function [ Module ]
  | EndModule'
  deriving (Eq,Show)

-- data GlobalVariable
--   = GlobalVariable' (T,V) GlobalVariable GlobalVariable
--   | EndGlobalVariable'
--   deriving (Eq,Show)

data DriveT
  = DriveT' Function Function
  | EndDriveT'
  deriving (Eq,Show)

data Function
  = Function' BasicBlock Function -- Function Function
  | EndFunction'
  deriving (Eq,Show)

data BasicBlock
  = BasicBlock' Instruction BasicBlock BasicBlock
  | EndBasicBlock'
  deriving (Eq,Show)

data Instruction
  = Instruction' Expr Instruction Instruction
  | EndInstruction'
  deriving (Eq,Show)

data ExprT
  = ExprT' Expr ExprT ExprT
  | ExprL'
  deriving (Eq,Show)

data Expr =
  
  --  DefFunc Func
    Terminal'  Terminal
  | BiOp'      BiOp
  | BitOp'     BitOp
  | CptOp'     CptOp
  | MemAccess' MemAccess
  | CastOp'    CastOp
  | CondOp'    CondOp
  | CallOp'    CallOp
  | Define'    Func      [Expr]
  
  --  | CallFunc Func
  --  | Op2 OpeCode Expr Expr
  --  | Cond Condition
  --  | Const Type
  --  | Var Type_ VarName Int
  | None
  deriving (Show,Eq)

type TypeID = Bs

type AddressMaxDepth = Int
type AddressBitN = Int
type V = Int

data MemoryTree =
    MemoryTree' MemoryTree MemoryTree
  | MemLeaf' T V
  deriving (Eq,Show)

data SymbolTree
  = SymbolTree' SymbolTree SymbolTree
  | SymLeaf' VarName
  deriving (Eq,Show)


-- allocation
-- 1. insert a leaf on a symbol tree(address is dynamic,variable is same)
-- 2. insert a leaf on a MemoryTree(address is dynamic,type is specified but, not value)

-- store
-- 1, search from symbol table, and identify address which links to the variable name,
-- No evaluation ( 2, the identified address will indicate a leaf on a memory )

-- load
-- 1. search address from a symbol tree and identify address
-- 2. the address guides you to a stored variable

-- get(element)Ptr --> &
-- 1. access to the symbolic table and get its address


add :: (TypeID,TypeID) -> Val -> Val -> Val
add (t1,t2) a b
  | t1 == "nuw" && t2 == "nuw" = a
  | t1 == "nsw" && t2 == "nsw" = a
  | t1 == "nuw" && t2 == "nsw" = a
  | t1 == "nsw" && t2 == "nuw" = a
  
  
-- add_v :: (Vec,Val) -> (Vec,Val) -> (Vec,Val)
-- add_v (t1,v1) (t2,v2) =
--   case t1 of
--     (Vec'' [(a,t)]) -> (t1,v1)
--     --(Vec'' [(a,t)]) -> (t1,v1)    
    
-- fadd :: (FP_T,Val) -> (FP_T,Val) -> (FP_T,Val)
-- fadd (Float,v) b = (Float,v)

mul :: (a,Val) -> (a,Val)
mul a = a

data Add =
    Add1  ( Int_T -> Int_T -> Int_T )
  | Add2  ( (Vec,Int_T) -> (Vec,Int_T) -> (Vec,Int_T) )
  
data Fadd =
    FAdd1 ( FP_T  -> FP_T  -> FP_T  )
  | FAdd2 ( (Vec,FP_T)  -> (Vec,FP_T)  -> FP_T )
  
  
data BiOp =
    Add'  Add
  | Fadd' Fadd
  | Sub'  --Sub
  | Fsub' 
  | Mul'
  | Fmul'
  | Udiv'
  | Sdiv'
  | Fdiv'
  | Urem'
  | Srem'
  | Frem'
  deriving (Show,Eq)



data BitOp =
    Shl'  Shl
  | Lshr' --Lshr
  | Ashr' --Ashr
  | And'  --And
  | Or'   --Or
  | Xor'  --Xor
  deriving (Show,Eq)


data Shl =
    Shl1 (Int_T -> Int_T -> Int_T)
  | Shl2 (Int_T -> Int_T -> Int_T)

data Terminal =
    Ret'    Ret
  | Br'     Br
  | Switch' Switch
  deriving (Show,Eq)

data MemAccess =
    Allocate T VarName
  | Store T Int Ptr T
  | Load Ptr T
  deriving (Show,Eq)


data Ret =
  Ret
  deriving (Show,Eq)


data Br  = Br'' Label_T
  deriving (Show,Eq)

data Switch = Switch''
  deriving (Show,Eq)


data CptOp =
    ExtractVal
  | InsertVal
  deriving (Show,Eq)


data CastOp =
    Trunc'  Trunc
  | Sex2
  | FpTrunc
  | FpExt
  | Fp2Ui
  | Ui2Fp
  | Si2Fp
  | Ptr2Int
  | Int2Ptr
  | BitCast
  deriving (Show,Eq)

data CondOp =
    Icmp' Icmp
  | Fcmp' Fcmp 
  | Phi'
  deriving (Show,Eq)

data Trunc =
    Trunc1 (Int_T -> Int_T -> Int_T)
  | Trunc2 ( (Vec,Int_T) -> (Vec,Int_T) -> (Vec,Int_T) )
  
  
data Icmp =
    Icmp1 ( Icond -> Int_T -> Int_T -> Int_T )
  | Icmp2 ( Icond -> (Vec,Int_T) -> (Vec,Int_T) -> (Vec,Int_T) )
  | Icmp3 ( Icond -> Ptr   -> Ptr   -> Int_T )
  
  
data Fcmp =
    Fcmp1 ( Fcond -> (Vec,FP_T) -> (Vec,FP_T) -> (Vec,Int_T) )
  | Fcmp2 ( Fcond -> FP_T -> FP_T  -> FP_T )
  
  
data Icond =
    Eq'
  | Ne'
  deriving (Show,Eq)


data Fcond =
    False'
  | True'
  deriving (Show,Eq)

  
data CallOp =
  Call
  deriving (Show,Eq)


instance Show Add where
  show _ = "a"

instance Show Fadd where
  show _ = "a"

instance Show Icmp where
  show _ = "a"

instance Show Fcmp where
  show _ = "a"
  
instance Show Shl where
  show _ = "a"
  

instance Show Trunc where
  show _ = "a"

  
instance Eq Add where
  (==) a b = True
  (/=) a b = True
  
instance Eq Fadd where
  (==) a b = True
  (/=) a b = True
  
instance Eq Icmp where
  (==) a b = True
  (/=) a b = True

instance Eq Fcmp where
  (==) a b = True
  (/=) a b = True
  
instance Eq Shl where
  (==) a b = True
  (/=) a b = True

instance Eq Trunc where
  (==) a b = True
  (/=) a b = True
  

