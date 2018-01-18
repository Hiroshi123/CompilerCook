

module VM_.JVM_.Data_.Mnemonic where




data Mnemonic =
  Nop | AconstNull | IConstM1 |
  IConst0 | IConst1 | IConst2 | IConst3 | IConst4 | IConst5
  LConst0 | LConst1 | 
  FConst0 | FConst1 | FConst2 |
  DConst0 | DConst1 |
  
  deriving (Enum,Show)


data PCRegister =
  PC' PCRegister
  | Reg RegBitN
  deriving (Show)


type RegBitN = Int
type ThreadN = Int

data Stack =
    Stack' Stack Stack
  | StackFrame LocalVariableArray OpStack FrameData
  deriving (Show)


data Heap =
  Heap' [Instance] StringPool
  deriving (Show)

data StringPool =
  StringPool' [Bs]
  deriving (Show)

data Instance =
  Instance' Field
  deriving (Show)
        
data MethodArea =
  MethodArea' [Class]
  deriving (Show)


data Class =
  Class' Constructor Method Field ConstantPool
  deriving (Show)
  
  
data Arch =
  Arch
  {
    :: Heap,
    :: MethodArea,
    :: NativeMethodStack
    
  } 


