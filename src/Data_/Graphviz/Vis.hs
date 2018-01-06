

module Data_.Graphviz.Vis where

import Data_.Graphviz.Layout

type Table = [Field]
type Vname = String
type Field = [( String,String )]
type ID_index = (Int,Int)
type Key = String
type Edge = ( Vname , Vname )
type Node = ( Vname , Table )


type GVString = String
-- this would be unlined inserting "\n" in between, and turning into GVString
type GVList   = [GVString]

-- nodes ("a","a"),("b","b") & 
-- edges ("a","b") have to be provided for visualization

make_subgraph ((index,title),x) =
  ["subgraph cluster" ++ index ++ " { "] ++ (make_subgraph' title x) ++ [" };"]
  
  
make_subgraph' title (n,e) =
  [
    "  label = \" " ++ title ++ " \" ;",
    "  labelloc = \"t\";",
    "  labeljust = \"l\";",
    "  fillcolor = \"#888888\";"
  ] ++ (createLabel n) ++ (createEdge e)
  
createLabel nl =
  map (\x -> "  " ++ (fst x) ++ " [label = \"" ++ (snd x) ++ "\"];" ) nl -- shape = diamond];") nl -- node_list
  
createEdge el =
  map (\x -> (snd x) ++ " -> " ++ (fst x) ++ ";" ) el

make_edge x = [ (fst x) ++ " -> " ++ (snd x) ++ ";" ]
              
--make_edge el = a -- ++ [ " [ " ++ name ++ " ] " ]
--where a = map (\x -> (snd x) ++ " -> " ++ (fst x) ++ ";" ) el
  
make_graph :: [GVList] -> GVString
make_graph s = unlines $ [ "digraph NNNN {"] ++ (make_graph' s) ++ [ "}" ]


make_graph' :: [GVList] -> GVList
make_graph' s = foldr1 (++) ([graph_layout',node_layout',edge_layout ] ++ s )

make_graph_ :: GVList -> GVString
make_graph_ s = unlines $ [ "digraph NNNN {"] ++ (make_graph_' s) ++ [ "}" ]


make_graph_' :: GVList -> GVList
make_graph_' s = x ++ s
  where x = foldr1 (++) [graph_layout',node_layout',edge_layout ]
  
  
make_field :: Field -> GVString
make_field x = foldl (\a b -> a ++ " | " ++ (fst b) ++ " " ++ (snd b) ) init (tail x) ++ " }"
  where (c,d) = head x
        init  = "{ " ++ c ++ " " ++ d
        
--make_table :: Foldable t => t Field -> GVString

make_table :: Table -> GVList
make_table x = [" [ "] ++  [" label = "] ++ [ " \"{ "] ++ r ++ [ " }\""] ++ [" ]" ]
  where r = map (\a -> " | " ++ ( make_field a ) ) x 
  
make_block :: ( Vname , Table ) -> GVList
make_block (n,x) = [ n ] ++ ( make_table x )


-- alpha:pl -> beta:pl [ label = "a-b", weight = 2.0];

-- samp = ([("a","a"),("b","b")],[("a","b")])


