

module MData.Graphviz.Vis (make_graph,make_subgraph) where

import MData.Graphviz.Layout

-- nodes ("a","a"),("b","b") & 
-- edges ("a","b") have to be provided for visualization

make_subgraph (title,x) =
  ["subgraph " ++ title ++ " { "] ++ (make_subgraph' title x) ++ [" };"]
  
  
make_subgraph' title (n,e) =
  [ 
    "  label = \" " ++ title ++ " \" ;",
    "  labelloc = \"t\";",
    "  labeljust = \"l\";",
    "  fillcolor = \"#888888\";"
  ] ++ (createLabel n) ++ (createEdge e)
  
  
createLabel nl =
  map (\x -> "  " ++ (fst x) ++ " [label = \"" ++ (snd x) ++ "\", shape = diamond];") nl -- node_list
  
createEdge el =
  map (\x -> (snd x) ++ " -> " ++ (fst x) ++ ";" ) el
  
  
make_graph :: [[[Char]]] -> String
make_graph s = unlines $ [ "digraph NNNN {"] ++ (make_graph' s) ++ [ "}" ]

make_graph' :: [[[Char]]] -> [[Char]]
make_graph' s = foldr1 (++) ([graph_layout,node_layout,node_layout,edge_layout , head s] )-- ++ s)


-- samp = ([("a","a"),("b","b")],[("a","b")])


