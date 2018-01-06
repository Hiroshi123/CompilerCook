
module Parser_.Json.VizTool where

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC --(ByteString,unpack)

import Data_.BNF.Json
import Data_.Graphviz.Vis

-- field_ex :: Table
-- field_ex = [[("","className")],[("<id1>","field1"),("","int"),("","var1"),("","1")],[("<id2>","field2")]]
-- field_ex1 :: Table
-- field_ex1 = [[("","class2")],[("<id>","field1"),("","int"),("","var1"),("","1")],[("<id2>","field2")]]



make_table_from_json :: ID_index -> JValue -> ( ID_index , [Edge] , [Node] )
make_table_from_json (pi,i) (JObject o) = ( (pi_,i_) , edge , node )
  where ((pi_,i_),a,(edge,b)) = iterate_obj (pi,i) o
        node = ("id_" ++ show i, a ) : b
make_table_from_json (pi,i) (JArray o) = ( (pi_,i_),edge,node )
  where ((pi_,i_),a,(edge,b)) = iterate_array (pi,i) ( zip [0..] o )
        node = ("id_" ++ show i, a ) : b
        
        
iterate_obj :: ID_index -> [( BC.ByteString , JValue)] -> ( ID_index , Table,( [Edge],[Node] ) )
iterate_obj id [] = ( id , [] , ([],[]) )
iterate_obj (pi,i) ((h1,h2):t) = ((pi__,i__),fi,(e_++e,n_++n))
  where ((pi_,i_)  ,fi_ ,(e , n )) = make_field_from_json (pi,i) ((BC.unpack h1), h2)
        ((pi__,i__),fi__,(e_, n_)) = iterate_obj (pi_,i_) t
        fi = [ (f h1) ++ fi_ ] ++ fi__
        f x = [ ( "" , "[key] " ++ BC.unpack x ) ]
        --f x = [ ( "<" ++ BC.unpack x ++ ">" , "[key] " ++ BC.unpack x ) ]
        
        
iterate_array :: ID_index -> [( Int , JValue)] -> (ID_index,Table,([Edge],[Node]) )
iterate_array id [] = ( id, [],([],[]) )
iterate_array (pi,i) ((h1,h2):t) = ((pi__,i__),fi,(e_++e,n_++n))
  where ((pi_,i_)  ,fi_  ,(e ,n )) = make_field_from_json (pi,i) ((show h1), h2)
        ((pi__,i__),fi__ ,(e_,n_)) = iterate_array (pi_,i_) t
        fi  = [ (f h1) ++ fi_] ++ fi__
        f x = [ ( "" , "[" ++ s ++ "]" ) ]
          where s = show x
        -- f x = [ ( "<" ++ s ++ ">" , "[" ++ s ++ "]" ) ]
        --   where s = show x
          
                    
make_field_from_json :: ID_index -> (Key,JValue) -> (ID_index , Field ,( [Edge],[Node] ) )
make_field_from_json id (_,JInt a )   = ( id, [ ("","Int")  , ("",show a) ] , ( [],[] ) )
make_field_from_json id (_,JBool a)   = ( id, [ ("","Bool") , ("",show a) ] , ([],[]) )
make_field_from_json id (_,JString a) = ( id, [ ("","String") , ("",BC.unpack a) ],([],[]) )
make_field_from_json (pi,i) (k,JObject a) = ( (pi_- diff ,i_), field,(e,n))
  where ((pi_,i_),e_,n) = make_table_from_json (i+1,i+1) (JObject a)
        diff = i - pi + 1
        field  = [ ("<"++k++">","Jobject") ]
        e      = [( "id_" ++ show (pi) ++":"++ k , "id_" ++ show (i+1))] ++ e_
        
                 
make_field_from_json (pi,i) (k,JArray a) = ( (pi_- diff,i_),field,(e,n) )
  where ((pi_,i_),e_,n) = make_table_from_json (i+1,i+1) (JArray a)
        diff   = i - pi + 1
        field  = [ ("<"++k++">","Array") ]
        e      = [( "id_" ++ show (pi) ++":"++ k,"id_" ++ show (i+1))] ++ e_
        
        
--f'' (JInt a )  = [ ("","Int")  , ("",show a) ]
--f'' (JNumber d)= [ ("","Double") , ("",d) ]
--f'' Null       = [ ("","Null") ]

--f'' (JArray a) = [ ("","array") ]
--f'' (JObject a)= [ ("","obj") ]
--f'' (JString a)= [ ("","string"),("",BC.unpack a) ]
  
--f (JInt a)    = (show a,field_ex)

--f' :: JValue -> GVList
--f' a = make_block ( f a )

ff a = make_graph (n ++ e)
  where (_,e_,n_) = make_table_from_json (1,1) a 
        n = fmap make_block n_
        e = fmap make_edge  e_
        
            
  --[ ("id ",field_ex) , ("id2 ",field_ex1) ]
  
  --where x = fmap f' ("id ",field_ex) ("id2 ",field_ex) -- [ JInt 10 , JInt 20 ]
  
--  where x = f a
  
  --[(BS.ByteString, JValue)]
  
-- make_block :: ( Vname , Table ) -> GVList


-- data JValue =
--   JString BS.ByteString
--   -- | JNumber Double
--   | JInt Int
--   | JBool Bool
--   | JNull
--   | JObject [(BS.ByteString, JValue)]
--   | JArray [JValue]
--   deriving (Eq, Ord, Show)

