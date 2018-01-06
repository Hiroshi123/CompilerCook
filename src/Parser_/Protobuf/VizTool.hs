{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Parser_.Protobuf.VizTool where

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)

import qualified Data_.BNF.ProtoBuff as PB
import Data_.Graphviz.Vis as GV

type Bs = BS.ByteString

-- type Table = [Field]
-- type Vname = String
-- type Field = [( String,String )]

-- type GVString = String
-- type ID_index = (Int,Int)
-- type Key = String
-- type Edge = ( Vname , Vname )
-- type Node = ( Vname , Table )


-- -- this would be unlined inserting "\n" in between, and turning into GVString
-- type GVList   = [GVString]

-- This file is for converting protobuf object to Graphviz visualization.
-- 

-- protobuf -> string
-- protobuf -> string

-- make_edge :: Protobuf -> [(String,String)]
-- make_edge (Message name field) = fmap (\x -> (make_edge' x,n)) field
--   where n = BC.unpack name
  
  
-- make_edge' field =
--   case field of
--     (Field ele tp n tag) -> BC.unpack n
--     otherwise            -> ""
    
                            
-- make_node :: Protobuf -> [(String,String)]
-- make_node (Message name field) = (n,n) : a
--   where n = BC.unpack name
--         a = fmap (\x -> make_node' x ) field
        
        
-- make_node' field =
--   case field of
--     (Field ele tp n tag) -> ( name , name )
--       where name = (BC.unpack n)
--             ror  = "[ " ++ (show ele) ++ "]"
--             tp_  = show tp
--             tag_ = BC.unpack n
--     otherwise            -> ( "" , "" )

-- data Protobuf =
--   Message Bs [Field]
--   | Oneof Bs [(Type,Bs,Tag)]
--   | Enum  Bs [(Bs,Tag)]
--   -- data constructor message-name(bytestring) fields 
--   deriving (Show)


-- data Field =
--   Field Elements Type Bs Tag
--   | Nested Protobuf
--   -- fields name
--   deriving (Show)

-- data Elements =
--   Optional | Required | Repeated
--   deriving (Show)

-- type Tag = Int

-- data Type =
--   Int32 | SInt32 | UInt32 | Fixed32 | SFixed32 |
--   Int64 | SInt64 | UInt64 | Fixed64 | SFixed64 |
--   Bool  | String | Bytes  | Float   | Double   |
--   Pointer Bs
--   deriving (Show)

make_table_pb :: PB.Protobuf -> ( [GV.Edge] , [GV.Node] )
make_table_pb (PB.Message n fi) = ( [("","")] , [("",[[("","")]])] )
  where x = make_row_pb fi
make_table_pb (PB.Oneof   n fi) = ( [("","")] , [("",[[("","")]])] )
  where x = make_row_pb' fi
make_table_pb (PB.Enum    n fi) = ( [("","")] , [("",[[("","")]])] )
  where x = make_row_pb'' fi
  

-- make_table_from_pb :: ID_index -> Protobuf -> ( ID_index , [Node] ) -- , [Edge] , [Node] )
-- make_table_from_pb (pi,i) (Message n fi) = ((pi_,i_),node)  -- , edge , node )
--   where ((pi_,i_),a)  = make_row (pi,i) fi
--         node = ("id_" ++ show i, a ) : b

        
-- -- make_table_from_pb (pi,i) (Oneof n fi) = ( (pi_,i_),edge,node )
-- --   where ((pi_,i_),a,(edge,b)) = iterate_array (pi,i) ( zip [0..] o )
-- --         node = ("id_" ++ show i, a ) : b

make_row_pb :: [PB.Field] -> ( GV.Table , ( [Edge] , [Node] ) )
make_row_pb [] = ( [] , ( [] , []) )
make_row_pb (h:t) = ( [r] ++ r_ , ( e ++ e_ , n ++ n_ ) )
  where (r  ,( e,n )) = make_field_pb h
        (r_ ,(e_,n_)) = make_row_pb t
        
make_row_pb' :: [(PB.Type,PB.Bs,PB.Tag)] -> GV.Table
make_row_pb' [] = []
make_row_pb' (h:t) = [x] ++ r_
  where x  = make_field_pb' h
        r_ = make_row_pb' t
        
        
make_row_pb'' :: [(PB.Bs,PB.Tag)] -> GV.Table 
make_row_pb'' [] = []
make_row_pb'' (h:t) = [x] ++ r_
  where x  = make_field_pb'' h
        r_ = make_row_pb'' t

                 
-- make_row :: ID_index -> [Protobuf] -> ( ID_index , Table ) -- , ( [Edge],[Node] ) )
-- make_row id [] = ( id , [] ) -- , ([],[]) )
-- make_row (pi,i) (fi:t) = r ++ r_
--   where r  = make_field_pb fi
--         r_ = make_row (pi,i) t

                 
make_field_pb :: PB.Field -> ( GV.Field , ( [Edge] , [Node] ) )
make_field_pb (PB.Field ele tp n tag) = ( r , ([],[]) )
  where r = [(f ele)] ++ ( make_field_pb' (tp,n,tag) )
        f x = ( "" , "" ++ show x ++ "" )
        
make_field_pb (PB.Nested pb) = ( [("","pb")] , ([],[]) )
  where x = make_table_pb pb


make_field_pb' (tp,n,tag) = r
  where r = [(f tp)] ++ ( make_field_pb'' (n,tag) )
        f x = ( "" , "" ++ show x ++ "" )

make_field_pb'' (n,tag) = r
  where r = [(f tag)] ++ [(BC.unpack n,BC.unpack n)]
        f x = ( "" , "" ++ show x ++ "" )
        
            
  -- ((pi__,i__),fi,(e_++e,n_++n))
  -- where ((pi_,i_)  ,fi_ ,(e , n )) = make_field_from_json (pi,i) ((BC.unpack h1), h2)
  --       ((pi__,i__),fi__,(e_, n_)) = make_raw (pi_,i_) t
  --       fi = [ (f h1) ++ fi_ ] ++ fi__
  --       f x = [ ( "" , "[key] " ++ BC.unpack x ) ]
        --f x = [ ( "<" ++ BC.unpack x ++ ">" , "[key] " ++ BC.unpack x ) ]
        
        
-- iterate_array :: ID_index -> [( Int , JValue)] -> (ID_index,Table,([Edge],[Node]) )
-- iterate_array id [] = ( id, [],([],[]) )
-- iterate_array (pi,i) ((h1,h2):t) = ((pi__,i__),fi,(e_++e,n_++n))
--   where ((pi_,i_)  ,fi_  ,(e ,n )) = make_field_from_json (pi,i) ((show h1), h2)
--         ((pi__,i__),fi__ ,(e_,n_)) = iterate_array (pi_,i_) t
--         fi  = [ (f h1) ++ fi_] ++ fi__
--         f x = [ ( "" , "[" ++ s ++ "]" ) ]
--           where s = show x
--         -- f x = [ ( "<" ++ s ++ ">" , "[" ++ s ++ "]" ) ]
--         --   where s = show x
          
                    
-- make_field_from_pb :: ID_index -> (Key,JValue) -> (ID_index , Field ,( [Edge],[Node] ) )
-- make_field_from_pb id (_,JInt a )   = ( id, [ ("","Int")  , ("",show a) ] , ( [],[] ) )
-- make_field_from_pb id (_,JBool a)   = ( id, [ ("","Bool") , ("",show a) ] , ([],[]) )
-- make_field_from_pb id (_,JString a) = ( id, [ ("","String") , ("",BC.unpack a) ],([],[]) )
-- make_field_from_pb (pi,i) (k,JObject a) = ( (pi_- diff ,i_), field,(e,n))
--   where ((pi_,i_),e_,n) = make_table_from_pb (i+1,i+1) (JObject a)
--         diff = i - pi + 1
--         field  = [ ("<"++k++">","Jobject") ]
--         e      = [( "id_" ++ show (pi) ++":"++ k , "id_" ++ show (i+1))] ++ e_
        
                 
-- make_field_from_json (pi,i) (k,JArray a) = ( (pi_- diff,i_),field,(e,n) )
--   where ((pi_,i_),e_,n) = make_table_from_pb (i+1,i+1) (JArray a)
--         diff   = i - pi + 1
--         field  = [ ("<"++k++">","Array") ]
--         e      = [( "id_" ++ show (pi) ++":"++ k,"id_" ++ show (i+1))] ++ e_
        
