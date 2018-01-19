

module VM_.JVM_.Data_.Mnemonic where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

import Data.Word

type Bs = BS.ByteString

data Mnemonic =
  
  -- const operation (21)
  Nop | AconstNull | IConstM1 |
  IConst0 | IConst1 | IConst2 | IConst3 | IConst4 | IConst5 |
  LConst0 | LConst1 | 
  FConst0 | FConst1 | FConst2 |
  DConst0 | DConst1 |
  BiPush  | SiPush  | Ldc | Ldcw | Ldc2w |
  
  -- load is kinds of push (25)
  Iload   | Fload   | Lload | Dload | Aload |
  Iload0  | Iload1  | Iload2  | Iload3  |
  Fload0  | Fload1  | Fload2  | Fload3  |
  Lload0  | Lload1  | Lload2  | Lload3  |
  Dload0  | Dload1  | Dload2  | Dload3  |
  
  -- Instruction which gets array elements
  IAload  | LAload  | FAload2 | DAload |
  AAload  | BAload  | CAload2 | SAload |
  
  -- store is kinds of pop
  Istore | Lstore | Fstore | Dstore | Astore |
  
  Istore_0  | Istore_1  | Istore_2  | Istore_3  |
  Lstore_0  | Lstore_1  | Lstore_2  | Lstore_3  |
  Fstore_0  | Fstore_1  | Fstore_2  | Fstore_3  |
  Dstore_0  | Dstore_1  | Dstore_2  | Dstore_3  |
  Astore_0  | Astore_1  | Astore_2  | Astore_3  |
  
  -- Instruction which stores array element
  IAstore | LAstore  | FAstore | DAstore | AAstore |
  BAstore | CAstore  | SAstore |
  
  Pop | Pop2 |
  
  -- stack duplication
  -- duplicate top-element of stack
  Dup | DupX1 | DupX2 |
  
  -- duplicate 1st & 2nd element of stack &
  -- place them bottom of the stack
  Dup2 | Dup2X1 | Dup2X2 |
  
  -- swap 2 element of a stack
  Swap |
  
  -- binary arithmetic instruction
  IAdd | LAdd | FAdd | DAdd |
  ISub | LSub | FSub | DSub |
  IMul | LMul | FMul | DMul |
  IDiv | LDiv | FDiv | DDiv |
  IRem | LRem | FRem | DRem |
  
  -- this instruction is unary which means
  -- gets just 1 element
  
  INeg | LNeg | FNeg | DNeg |
  
  -- logical operation instruction
  
  IShl  | LShl  |
  IShr  | LShr  |
  IuShr | LuShr |
  
  IAnd  | Land |
  IOr   | LOr  |
  IXor  | LXor |
  
  -- casting operation
  I2l | I2f | I2d |
  L2i | L2f | L2d |
  F2i | F2f | F2d |
  D2i | D2f | D2d |
  I2b | I2c | I2s |
  
  -- compare
  Icmp  |
  Fcmpl | Fcmpg |
  Dcmpl | Dcmpg |
  
  -- condition
  Ifeq  | Ifne  | Iflt | Ifge | Ifgt  | Ifle  |
  
  -- 6 different 
  Ificmpeq | Ificmpne | Ificmplt | Ificmpge | Ificmpgt | Ificmple |
  
  -- get two objects from stack and check if they are identical or not
  Ifacmpeq | Ifacmpne |
  
  -- jump instruction
  Goto | Jsr | Ret | TableSwitch | LookupSwitch |
  
  -- return instruction
  IReturn | LReturn | FReturn | DReturn | AReturn | Return |
  
  -- get/put static
  GetStatic | PutStatic |
  
  -- get/put field
  GetField | Putfield |

  -- call instruction
  InvoleVirtual | InvokeSpecial |
  InvoleStatic  | InvokeInterface | InvoleDynamic |
  
  -- object instantiation
  New | NewArray | ANewArray |
  
  ArrayLength | Athrow |
  
  CheckCast | InstanceOf |

  -- 
  MonitorEnter | MonitorExit |

  -- extension instruction
  Wide |

  -- 
  MultiANewArray | IfNull | IfNonNull | GotoW | JsrW |
  BreakPoint
  
  deriving (Enum,Show)

data OpType =
  NOP | CONST | STORE | PUSH | LOAD | POP |
  DUP | SWAP | ADD | SUB | MUL | DIV | REM |
  NEG | SHL  | SHR | AND | OR | XOR | INC |
  CAST | CMP | GREATER | LESS | EQUAL | IF |
  SWITCH | RETURN |
  GET | PUT | STATIC | FIELD | INVOKE |
  SPECIAL | VIRTUAL | DYNAMIC |
  NEW | ARRAY | THROW | MONITOR |
  GOTO | JSR  | BREAKPOINT
  
  deriving (Enum,Show)

data Type =
  I | L | S | B | C | F | D | Z | A
  deriving (Enum,Show)


aa :: [(Mnemonic,([Type],[OpType],Maybe Int))]
aa =
  
  [  
    (IConst0,([I],[CONST],Just 0 )),
    (IConst1,([I],[CONST],Just 1 )),
    (IConst2,([I],[CONST],Just 2 )),
    (IConst3,([I],[CONST],Just 3 )),
    
    (LConst0,([L],[CONST],Just 0 )),
    (LConst1,([L],[CONST],Just 1 )),
    
    (FConst0,([F],[CONST],Just 0 )),
    (FConst1,([F],[CONST],Just 1 )),
    (FConst2,([F],[CONST],Just 2 )),
    
    (DConst0,([D],[CONST],Just 0 )),
    (DConst1,([D],[CONST],Just 1 )),
    
    (BiPush,([B],[PUSH] ,Nothing)),
    (SiPush,([S],[PUSH] ,Nothing)) --,
    
    -- (SiPush,([ ],Ldc  ,Nothing)),
    -- (SiPush,([W],Ldc ,Nothing)),
    -- (SiPush,([S],Ldc2w,Nothing)),
    
    -- BiPush  | SiPush  | Ldc | Ldcw | Ldc2w |
    
    ]
  
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

data LocalVariableArray =
    LVA' LocalVariableArray
  | Val' Word
  deriving (Show)

data OpStack =
  OpStack' 
  deriving (Show)

data FrameData = FrameData
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

data Field = Field
  deriving (Show)

data MethodArea =
  MethodArea' [Class]
  deriving (Show)

data Class = Class
  deriving (Show)



-- data Class =
--   Class' Constructor Method Field ConstantPool
--   deriving (Show)  

-- data Arch =
--   Arch
--   {
--     :: Heap,
--     :: MethodArea,
--     :: NativeMethodStack
    
--   } 


