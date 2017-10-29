
module MData.BNF.ProtoBuff where

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)

type Bs = BS.ByteString

data Protobuf =
  Message Bs [Field]
  | Oneof Bs [(Type,Bs,Tag)]
  | Enum  Bs [(Bs,Tag)]
  -- data constructor message-name(bytestring) fields 
  deriving (Show)


data Field =
  Field Elements Type Bs Tag
  | Nested Protobuf
  -- fields name
  deriving (Show)

data Elements =
  Optional | Required | Repeated
  deriving (Show)

type Tag = Int

data Type =
  Int32 | SInt32 | UInt32 | Fixed32 | SFixed32 |
  Int64 | SInt64 | UInt64 | Fixed64 | SFixed64 |
  Bool  | String | Bytes  | Float   | Double   |
  Pointer Bs
  deriving (Show)

-- Pointer type is reference to another protobuffer

-- message A {  ; ; ; }

-- optional = 1;
-- required = 2;
-- repeated = 3;

-- enum
-- oneof (actually equal to union)
-- type 15(int32*5,int64*5,float,double,string,bool,bytes).


-- message inside of message;

-- first parse should get all of message.


